using Base: isbitsunion

# ---------------------------------------------------------------------------
# Swiss-table primitives
# ---------------------------------------------------------------------------

@inline function split_hash(key)
    h = hash(key) % UInt
    h2 = UInt8((h >> (8 * sizeof(UInt) - 7)) & 0x7F)
    return h, h2
end

@inline group_for(h1::UInt, ngroups::Int) =
    Int(h1 & (UInt(ngroups) - UInt(1))) + 1

@inline slot_in_group(g_idx::Int, bit::Int) = (g_idx - 1) * 16 + bit + 1

@inline next_group_idx(g_idx::Int, step::Int, ngroups::Int) =
    Int((UInt(g_idx - 1) + UInt(step)) & (UInt(ngroups) - UInt(1))) + 1

# 16-byte SIMD vector type for the llvmcall helpers below.
const _Vec16u8 = NTuple{16, VecElement{UInt8}}

@inline _to_vec(g::NTuple{16,UInt8}) =
    ntuple(i -> VecElement(@inbounds g[i]), Val(16))

# Generic LLVM movemask: compare 16 bytes, bitcast i1-vector to i16.
# LLVM lowers this to pmovmskb on x86 and equivalent NEON on aarch64.
@inline function _eq_movemask(g::_Vec16u8, t::_Vec16u8)
    Base.llvmcall(
        """
        %cmp = icmp eq <16 x i8> %0, %1
        %mask = bitcast <16 x i1> %cmp to i16
        ret i16 %mask
        """,
        UInt16, Tuple{_Vec16u8, _Vec16u8}, g, t,
    )
end

# High-bit movemask: each byte's MSB → corresponding bit in UInt16.
# `icmp slt ... 0` treats the byte as signed for the test — signedness is
# per-instruction, not per-type, so the UInt8 vector arg is fine.
@inline function _high_bit_movemask(g::_Vec16u8)
    Base.llvmcall(
        """
        %cmp = icmp slt <16 x i8> %0, zeroinitializer
        %mask = bitcast <16 x i1> %cmp to i16
        ret i16 %mask
        """,
        UInt16, Tuple{_Vec16u8}, g,
    )
end

@inline function match_byte(g::NTuple{16,UInt8}, target::UInt8)
    gv = _to_vec(g)
    tv = ntuple(_ -> VecElement(target), Val(16))
    return _eq_movemask(gv, tv)
end

@inline match_empty_or_deleted(g::NTuple{16,UInt8}) = _high_bit_movemask(_to_vec(g))

# Control-byte sentinels. A full slot stores the 7-bit h2 fingerprint
# (0x00..0x7F); the high bit is reserved for these two sentinels.
const CTRL_EMPTY   = 0x80
const CTRL_DELETED = 0xFE

const _empty_group = ntuple(_ -> CTRL_EMPTY, Val(16))

@inline load_group(ctrl::Memory{NTuple{16,UInt8}}, g_idx::Int) =
    @inbounds ctrl[g_idx]

@inline function set_ctrl_byte!(ctrl::Memory{NTuple{16,UInt8}}, slot::Int, byte::UInt8)
    g_idx = ((slot - 1) >> 4) + 1
    bit = ((slot - 1) & 15) + 1
    @inbounds ctrl[g_idx] = Base.setindex(ctrl[g_idx], byte, bit)
    return
end

"""
    OrderedDict

`OrderedDict`s are simply dictionaries whose entries have a particular order. The order
refers to insertion order, which allows deterministic iteration over the dictionary.
"""
mutable struct OrderedDict{K,V} <: AbstractDict{K,V}
    ctrl::Memory{NTuple{16,UInt8}}
    idx::Memory{Int32}
    keys::Vector{K}
    vals::Vector{V}
    ndel::Int
    dirty::Bool
end

function OrderedDict{K,V}() where {K,V}
    OrderedDict{K,V}(Memory{NTuple{16,UInt8}}(undef, 0), Memory{Int32}(undef, 0),
                     Vector{K}(), Vector{V}(), 0, false)
end

@inline _current_nslots(h::OrderedDict) = length(h.ctrl) << 4

@inline function _round_table_size(n::Integer)
    n <= 0 && return 0
    return max(16, 1 << (8*sizeof(Int) - leading_zeros(Int(n) - 1)))
end

function OrderedDict{K,V}(kv) where {K,V}
    h = OrderedDict{K,V}()
    for (k,v) in kv
        h[k] = v
    end
    return h
end

OrderedDict{K,V}(p::Pair) where {K,V} = setindex!(OrderedDict{K,V}(), p.second, p.first)

function OrderedDict{K,V}(ps::Pair...) where {K,V}
    h = OrderedDict{K,V}()
    sizehint!(h, length(ps))
    for p in ps
        h[p.first] = p.second
    end
    return h
end

function OrderedDict{K,V}(d::OrderedDict{K,V}) where {K,V}
    d.ndel > 0 && rehash!(d)
    @assert d.ndel == 0
    OrderedDict{K,V}(copy(d.ctrl), copy(d.idx), copy(d.keys), copy(d.vals), 0, false)
end

OrderedDict() = OrderedDict{Any,Any}()
OrderedDict(kv::Tuple{}) = OrderedDict()
copy(d::OrderedDict) = OrderedDict(d)

# TODO: this can probably be simplified using `eltype` as a THT (Tim Holy trait)
# OrderedDict{K,V}(kv::Tuple{Vararg{Tuple{K,V}}})     = OrderedDict{K,V}(kv)
# OrderedDict{K  }(kv::Tuple{Vararg{Tuple{K,Any}}})   = OrderedDict{K,Any}(kv)
# OrderedDict{V  }(kv::Tuple{Vararg{Tuple{Any,V}}})   = OrderedDict{Any,V}(kv)
OrderedDict(kv::Tuple{Vararg{Pair{K,V}}}) where {K,V}  = OrderedDict{K,V}(kv)

OrderedDict(kv::AbstractArray{Tuple{K,V}}) where {K,V} = OrderedDict{K,V}(kv)
OrderedDict(kv::AbstractArray{Pair{K,V}}) where {K,V}  = OrderedDict{K,V}(kv)
OrderedDict(kv::AbstractDict{K,V}) where {K,V}         = OrderedDict{K,V}(kv)

OrderedDict(ps::Pair{K,V}...) where {K,V} = OrderedDict{K,V}(ps)
OrderedDict(ps::Pair...)                  = OrderedDict(ps)

@static if VERSION >= v"1.11"
    # see JuliaLang/julia#53151
    OrderedDict(kv) = dict_with_eltype((K, V) -> OrderedDict{K, V}, kv, eltype(kv))
else
    function OrderedDict(kv)
        try
            dict_with_eltype((K, V) -> OrderedDict{K, V}, kv, eltype(kv))
        catch e
            if isempty(methods(iterate, (typeof(kv),))) ||
                !all(x->isa(x, Union{Tuple,Pair}), kv)
                throw(ArgumentError("OrderedDict(kv): kv needs to be an iterator of tuples or pairs"))
            else
                rethrow(e)
            end
        end
    end
end

empty(d::OrderedDict{K,V}) where {K,V} = OrderedDict{K,V}()
empty(d::OrderedDict, ::Type{K}, ::Type{V}) where {K, V} = OrderedDict{K, V}()

length(d::OrderedDict) = length(d.keys) - d.ndel
isempty(d::OrderedDict) = (length(d) == 0)

"""
    isordered(::Type)

Property of associative containers, that is `true` if the container type has a
defined order (such as `OrderedDict` and `SortedDict`), and `false` otherwise.
"""
isordered(::Type{T}) where {T<:AbstractDict} = false
isordered(::Type{T}) where {T<:OrderedDict} = true

# conversion between OrderedDict types
function convert(::Type{OrderedDict{K,V}}, d::AbstractDict) where {K,V}
    d isa OrderedDict{K, V} && return d
    if !isordered(typeof(d))
        Base.depwarn("Conversion to OrderedDict is deprecated for unordered associative containers (in this case, $(typeof(d))). Use an ordered or sorted associative type, such as SortedDict and OrderedDict.", :convert)
    end
    h = OrderedDict{K,V}()
    for (k,v) in d
        ck = convert(K,k)
        if !haskey(h,ck)
            h[ck] = convert(V,v)
        else
            error("key collision during dictionary conversion")
        end
    end
    return h
end

function insert_fresh!(ctrl::Memory{NTuple{16,UInt8}}, idx::Memory{Int32}, key,
                       pos::Int32, ngroups::Int)
    h1, h2 = split_hash(key)
    g_idx = group_for(h1, ngroups)
    step = 1
    while true
        g = load_group(ctrl, g_idx)
        m = match_byte(g, CTRL_EMPTY)
        if m != 0
            bit = trailing_zeros(m)
            slot = slot_in_group(g_idx, bit)
            set_ctrl_byte!(ctrl, slot, h2)
            @inbounds idx[slot] = pos
            return
        end
        g_idx = next_group_idx(g_idx, step, ngroups)
        step += 1
    end
end

function rehash!(h::OrderedDict{K,V}, newsz::Integer = _current_nslots(h)) where {K,V}
    h.dirty = true
    newsz = _round_table_size(newsz)
    ngroups = newsz >> 4
    new_ctrl = Memory{NTuple{16,UInt8}}(undef, ngroups)
    new_idx  = Memory{Int32}(undef, newsz)
    fill!(new_ctrl, _empty_group)

    count0 = length(h)
    if count0 == 0
        empty!(h.keys)
        empty!(h.vals)
        h.ndel = 0
    elseif h.ndel == 0
        # All entries live: rebuild ctrl/idx, leave keys/vals in place.
        @inbounds for i in 1:count0
            insert_fresh!(new_ctrl, new_idx, h.keys[i], Int32(i), ngroups)
        end
    else
        # Build a bitmap of live keys-positions by walking ctrl, then migrate.
        old_ctrl = h.ctrl
        old_idx = h.idx
        live = falses(length(h.keys))
        @inbounds for og in 1:length(old_ctrl)
            g = load_group(old_ctrl, og)
            m_full = (~match_empty_or_deleted(g)) & 0xFFFF
            while m_full != 0
                bit = trailing_zeros(m_full)
                live[old_idx[slot_in_group(og, bit)]] = true
                m_full &= m_full - UInt16(1)
            end
        end

        newkeys = similar(h.keys, count0)
        newvals = similar(h.vals, count0)
        to = 1
        @inbounds for from in 1:length(h.keys)
            live[from] || continue
            k = h.keys[from]
            insert_fresh!(new_ctrl, new_idx, k, Int32(to), ngroups)
            newkeys[to] = k
            newvals[to] = h.vals[from]
            to += 1
        end
        h.keys = newkeys
        h.vals = newvals
        h.ndel = 0
    end

    h.ctrl = new_ctrl
    h.idx  = new_idx
    return h
end

function sizehint!(d::OrderedDict, newsz::Integer)
    needed = (Int(newsz) * 8 + 6) ÷ 7
    target = _round_table_size(needed)
    target > _current_nslots(d) && rehash!(d, target)
    return d
end

function empty!(h::OrderedDict{K,V}) where {K,V}
    if length(h.ctrl) > 0
        fill!(h.ctrl, _empty_group)
    end
    empty!(h.keys)
    empty!(h.vals)
    h.ndel = 0
    h.dirty = true
    return h
end

# Find `key` (with fingerprint `h2`) in a single group `g`. Returns the slot
# index (>0) on hit, 0 on miss. `g_zero` is (g_idx - 1) — its pre-decremented
# form lets the single-group fast path skip any group arithmetic.
@inline function _find_h2_in_group(g::NTuple{16,UInt8}, g_zero::Int,
                                   idx::Memory{Int32}, keys::Vector,
                                   key, h2::UInt8)
    m = match_byte(g, h2)
    @inbounds while m != 0
        bit = trailing_zeros(m)
        slot = g_zero * 16 + bit + 1
        i = idx[slot]
        if isequal(key, keys[i])
            return slot
        end
        m &= m - UInt16(1)
    end
    return 0
end

function ht_keyindex(h::OrderedDict{K,V}, key, direct::Bool) where {K,V}
    nctrl = length(h.ctrl)
    nctrl == 0 && return -1
    ctrl = h.ctrl
    idx = h.idx
    keys = h.keys
    h1, h2 = split_hash(key)

    if nctrl == 1
        # Single-group fast path: no probing, group is always ctrl[1].
        @inbounds g = ctrl[1]
        slot = _find_h2_in_group(g, 0, idx, keys, key, h2)
        slot != 0 && return direct ? Int(@inbounds idx[slot]) : slot
        return -1
    end

    ngroups = nctrl
    g_idx = group_for(h1, ngroups)
    step = 1
    @inbounds while true
        g = load_group(ctrl, g_idx)
        slot = _find_h2_in_group(g, g_idx - 1, idx, keys, key, h2)
        slot != 0 && return direct ? Int(idx[slot]) : slot
        match_byte(g, CTRL_EMPTY) != 0 && return -1
        g_idx = next_group_idx(g_idx, step, ngroups)
        step += 1
    end
end

function ht_keyindex2_h(h::OrderedDict{K,V}, key, h1::UInt, h2::UInt8) where {K,V}
    nctrl = length(h.ctrl)
    ctrl = h.ctrl
    idx = h.idx
    keys = h.keys

    if nctrl == 1
        # Single-group fast path. We're guaranteed at least one empty-or-deleted
        # byte (load factor invariant), so the insert slot always exists.
        @inbounds g = ctrl[1]
        slot = _find_h2_in_group(g, 0, idx, keys, key, h2)
        slot != 0 && return Int(@inbounds idx[slot])
        me = match_empty_or_deleted(g)
        return -(trailing_zeros(me) + 1)
    end

    ngroups = nctrl
    g_idx = group_for(h1, ngroups)
    step = 1
    insert_slot = 0

    @inbounds while true
        g = load_group(ctrl, g_idx)
        slot = _find_h2_in_group(g, g_idx - 1, idx, keys, key, h2)
        slot != 0 && return Int(idx[slot])

        if insert_slot == 0
            me = match_empty_or_deleted(g)
            if me != 0
                bit = trailing_zeros(me)
                insert_slot = slot_in_group(g_idx, bit)
            end
        end

        match_byte(g, CTRL_EMPTY) != 0 && return -insert_slot

        g_idx = next_group_idx(g_idx, step, ngroups)
        step += 1
    end
end

function _setindex!(h::OrderedDict, v, key, slot, h2::UInt8)
    push!(h.keys, key)
    push!(h.vals, v)
    nk = length(h.keys)
    @assert nk <= typemax(Int32) "OrderedDict cannot exceed $(typemax(Int32)) total inserts"
    set_ctrl_byte!(h.ctrl, slot, h2)
    @inbounds h.idx[slot] = Int32(nk)
    h.dirty = true
    maybe_rehash!(h)
end

function maybe_rehash!(h::OrderedDict)
    nslots = _current_nslots(h)
    nk = length(h.keys)
    if 8 * nk >= 7 * nslots
        if 4 * h.ndel > nslots
            rehash!(h, nslots)
        else
            rehash!(h, max(2 * nslots, 16))
        end
    end
end

function setindex!(h::OrderedDict{K,V}, v0, key0) where {K,V}
    key = convert(K, key0)
    if !isequal(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end
    v = convert(V, v0)

    length(h.ctrl) == 0 && rehash!(h, 16)

    h1, h2 = split_hash(key)
    index = ht_keyindex2_h(h, key, h1, h2)
    if index > 0
        @inbounds h.keys[index] = key
        @inbounds h.vals[index] = v
    else
        _setindex!(h, v, key, -index, h2)
    end
    return h
end

function get!(h::OrderedDict{K,V}, key0, default) where {K,V}
    key = convert(K, key0)
    if !isequal(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end

    length(h.ctrl) == 0 && rehash!(h, 16)

    h1, h2 = split_hash(key)
    index = ht_keyindex2_h(h, key, h1, h2)
    index > 0 && return h.vals[index]

    v = convert(V, default)
    _setindex!(h, v, key, -index, h2)
    return v
end

function get!(default::Base.Callable, h::OrderedDict{K,V}, key0) where {K,V}
    key = convert(K, key0)
    if !isequal(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end

    length(h.ctrl) == 0 && rehash!(h, 16)

    h1, h2 = split_hash(key)
    index = ht_keyindex2_h(h, key, h1, h2)
    index > 0 && return h.vals[index]

    h.dirty = false
    v = convert(V, default())
    if h.dirty
        length(h.ctrl) == 0 && rehash!(h, 16)
        index = ht_keyindex2_h(h, key, h1, h2)
    end
    if index > 0
        @inbounds h.keys[index] = key
        @inbounds h.vals[index] = v
    else
        _setindex!(h, v, key, -index, h2)
    end
    return v
end

function getindex(h::OrderedDict{K,V}, key) where {K,V}
    index = ht_keyindex(h, key, true)
    return (index<0) ? throw(KeyError(key)) : h.vals[index]::V
end

function get(h::OrderedDict{K,V}, key, default) where {K,V}
    index = ht_keyindex(h, key, true)
    return (index<0) ? default : h.vals[index]::V
end

function get(default::Base.Callable, h::OrderedDict{K,V}, key) where {K,V}
    index = ht_keyindex(h, key, true)
    return (index<0) ? default() : h.vals[index]::V
end

haskey(h::OrderedDict, key) = (ht_keyindex(h, key, true) >= 0)
in(key, v::Base.KeySet{K,T}) where {K,T<:OrderedDict{K}} = (ht_keyindex(v.dict, key, true) >= 0)

function getkey(h::OrderedDict{K,V}, key, default) where {K,V}
    index = ht_keyindex(h, key, true)
    return (index<0) ? default : h.keys[index]::K
end

function _pop!(h::OrderedDict, slot::Int)
    @inbounds val = h.vals[h.idx[slot]]
    _delete!(h, slot)
    return val
end

function pop!(h::OrderedDict)
    h.ndel > 0 && rehash!(h)
    key = h.keys[end]
    slot = ht_keyindex(h, key, false)
    return key => _pop!(h, slot)
end

function popfirst!(h::OrderedDict)
    h.ndel > 0 && rehash!(h)
    key = h.keys[1]
    slot = ht_keyindex(h, key, false)
    return key => _pop!(h, slot)
end

function pop!(h::OrderedDict, key)
    slot = ht_keyindex(h, key, false)
    return slot > 0 ? _pop!(h, slot) : throw(KeyError(key))
end

function pop!(h::OrderedDict, key, default)
    slot = ht_keyindex(h, key, false)
    return slot > 0 ? _pop!(h, slot) : default
end

function _delete!(h::OrderedDict, slot::Int)
    @inbounds i = h.idx[slot]
    Base._unsetindex!(h.keys, Int(i))
    Base._unsetindex!(h.vals, Int(i))
    h.ndel += 1
    h.dirty = true

    g_idx = ((slot - 1) >> 4) + 1
    g = load_group(h.ctrl, g_idx)
    if match_byte(g, CTRL_EMPTY) != 0
        set_ctrl_byte!(h.ctrl, slot, CTRL_EMPTY)
    else
        set_ctrl_byte!(h.ctrl, slot, CTRL_DELETED)
    end
    return h
end

function delete!(h::OrderedDict, key)
    slot = ht_keyindex(h, key, false)
    slot > 0 && _delete!(h, slot)
    return h
end

function iterate(t::OrderedDict)
    t.ndel > 0 && rehash!(t)
    length(t.keys) < 1 && return nothing
    return (Pair(t.keys[1], t.vals[1]), 2)
end
function iterate(t::OrderedDict, i)
    length(t.keys) < i && return nothing
    return (Pair(t.keys[i], t.vals[i]), i+1)
end

# lazy reverse iteration
function iterate(rt::Iterators.Reverse{<:OrderedDict})
    t = rt.itr
    t.ndel > 0 && rehash!(t)
    n = length(t.keys)
    n < 1 && return nothing
    return (Pair(t.keys[n], t.vals[n]), n - 1)
end
function iterate(rt::Iterators.Reverse{<:OrderedDict}, i)
    t = rt.itr
    i < 1 && return nothing
    return (Pair(t.keys[i], t.vals[i]), i - 1)
end


function _merge_kvtypes(d, others...)
    K, V = keytype(d), valtype(d)
    for other in others
        K = promote_type(K, keytype(other))
        V = promote_type(V, valtype(other))
    end
    return (K,V)
end

function merge(d::OrderedDict, others::AbstractDict...)
    K,V = _merge_kvtypes(d, others...)
    merge!(OrderedDict{K,V}(), d, others...)
end

function mergewith(combine, d::OrderedDict, others::AbstractDict...)
    K,V = _merge_kvtypes(d, others...)
    mergewith!(combine, OrderedDict{K,V}(), d, others...)
end

merge(combine::Function, d::OrderedDict, others::AbstractDict...) = mergewith(combine, d, others...)

function Base.map!(f, iter::Base.ValueIterator{<:OrderedDict})
    dict = iter.dict
    dict.ndel > 0 && rehash!(dict)
    vals = dict.vals
    @inbounds for i in 1:length(vals)
        vals[i] = f(vals[i])
    end
    return iter
end

last(h::OrderedDict) = h.keys[end] => h.vals[end]
