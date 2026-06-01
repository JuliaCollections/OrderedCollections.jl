using Base: isbitsunion

if !isdefined(Base, :Memory)
    const Memory = Vector
end

"""
    OrderedDict

`OrderedDict`s are simply dictionaries whose entries have a particular order. The order
refers to insertion order, which allows deterministic iteration over the dictionary.
"""
mutable struct OrderedDict{K,V} <: AbstractDict{K,V}
    slots::Memory{Int32}
    keys::Memory{K}
    vals::Memory{V}
    len::Int
    ndel::Int
    dirty::Bool
end

function OrderedDict{K,V}() where {K,V}
    OrderedDict{K,V}(Memory{Int32}(undef, 0), Memory{K}(undef, 0), Memory{V}(undef, 0), 0, 0, false)
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
    if d.ndel > 0
        rehash!(d)
    end
    @assert d.ndel == 0
    OrderedDict{K,V}(copy(d.slots), copy(d.keys), copy(d.vals), d.len, 0, false)
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

length(d::OrderedDict) = d.len - d.ndel
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

isslotempty(slot_value::Integer) = slot_value == 0
isslotfilled(slot_value::Integer) = slot_value > 0
isslotmissing(slot_value::Integer) = slot_value < 0

function rehash!(h::OrderedDict{K,V}, newsz::Integer = length(h.slots)) where {K,V}
    olds = h.slots
    oldk = h.keys
    oldv = h.vals
    sz = length(olds)
    oldlen = h.len
    newsz = _tablesz(newsz)
    h.dirty = true
    count0 = length(h)
    if count0 == 0
        h.slots = Memory{Int32}(undef, newsz); fill!(h.slots, 0)
        h.keys = Memory{K}(undef, newsz)
        h.vals = Memory{V}(undef, newsz)
        h.len = 0
        h.ndel = 0
        return h
    end

    slots = Memory{Int32}(undef, newsz); fill!(slots, 0)
    newkeys = Memory{K}(undef, newsz)
    newvals = Memory{V}(undef, newsz)

    if h.ndel > 0
        # Mark live positions by walking the old slot table, then compact in
        # ascending position order so insertion order is preserved.
        live = falses(oldlen)
        @inbounds for index in 1:sz
            si = olds[index]
            si > 0 && (live[si] = true)
        end
        to = 1
        @inbounds for from in 1:oldlen
            live[from] || continue
            k = oldk[from]
            index = hashindex(k, newsz)
            while slots[index] != 0
                index = (index & (newsz-1)) + 1
            end
            slots[index] = to
            newkeys[to] = k
            newvals[to] = oldv[from]
            to += 1
        end
        h.len = to - 1
        h.ndel = 0
    else
        @inbounds copyto!(newkeys, 1, oldk, 1, oldlen)
        @inbounds copyto!(newvals, 1, oldv, 1, oldlen)
        @inbounds for i = 1:oldlen
            k = newkeys[i]
            index = hashindex(k, newsz)
            while slots[index] != 0
                index = (index & (newsz-1)) + 1
            end
            slots[index] = i
        end
    end

    h.slots = slots
    h.keys = newkeys
    h.vals = newvals
    return h
end

function sizehint!(d::OrderedDict, newsz::Integer)
    slotsz = (newsz*3)>>1
    oldsz = length(d.slots)
    if slotsz <= oldsz
        # todo: shrink
        # be careful: rehash!() assumes everything fits. it was only designed
        # for growing.
        return d
    end
    # grow at least 25%
    slotsz = max(slotsz, (oldsz*5)>>2)
    rehash!(d, slotsz)
end

function empty!(h::OrderedDict{K,V}) where {K,V}
    fill!(h.slots, 0)
    h.len = 0
    h.ndel = 0
    h.dirty = true
    return h
end

# position of `key` in keys/vals (>0), or -1 if absent. Hot path for getindex.
function ht_keyindex(h::OrderedDict{K,V}, key) where {K,V}
    slots = h.slots
    sz = length(slots)
    sz == 0 && return -1
    keys = h.keys
    index = hashindex(key, sz)
    @inbounds while true
        si = slots[index]
        si == 0 && return -1
        if si > 0 && isequal(key, keys[si])
            return Int(si)
        end
        index = (index & (sz-1)) + 1
    end
end

# slot index of `key` (for delete!/pop!), or -1 if absent.
function ht_slotindex(h::OrderedDict{K,V}, key) where {K,V}
    slots = h.slots
    sz = length(slots)
    sz == 0 && return -1
    keys = h.keys
    index = hashindex(key, sz)
    @inbounds while true
        si = slots[index]
        si == 0 && return -1
        if si > 0 && isequal(key, keys[si])
            return index
        end
        index = (index & (sz-1)) + 1
    end
end

# get the index where a key is stored, or -pos if not present
# and the key would be inserted at pos
# This version is for use by setindex! and get!
function ht_keyindex2(h::OrderedDict{K,V}, key) where {K,V}
    slots = h.slots
    sz = length(slots)
    if sz == 0
        rehash!(h, 16)
        slots = h.slots
        sz = length(slots)
    end
    keys = h.keys
    index = hashindex(key, sz)
    avail = 0
    @inbounds while true
        si = slots[index]
        if si == 0
            return avail < 0 ? avail : -index
        end
        if si < 0
            avail == 0 && (avail = -index)
        elseif key === keys[si] || isequal(key, keys[si])
            return Int(si)
        end
        index = (index & (sz-1)) + 1
    end
end

# Append a new entry at position len+1. Capacity is guaranteed by the rehash
# trigger in _setindex! (cap(keys) == length(slots) == sz, and len < sz holds
# entering each append), so no bounds-grow check is needed.
@inline function _append!(h::OrderedDict, key, v)
    nk = h.len + 1
    h.len = nk
    @inbounds h.keys[nk] = key
    @inbounds h.vals[nk] = v
    return nk
end

function _setindex!(h::OrderedDict, v, key, index)
    nk = _append!(h, key, v)
    @inbounds h.slots[index] = nk
    h.dirty = true

    sz = length(h.slots)
    cnt = nk - h.ndel
    # Rehash now if necessary: > 3/4 dead, > 2/3 live, or keys/vals capacity hit.
    if h.ndel >= ((3*nk)>>2) > 4 || cnt*3 > sz*2 || nk >= sz
        rehash!(h, cnt > 64000 ? cnt*2 : cnt*4)
    end
end

function setindex!(h::OrderedDict{K,V}, v0, key0) where {K,V}
    key = convert(K, key0)
    if !isequal(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end
    v = convert(V, v0)

    index = ht_keyindex2(h, key)

    if index > 0
        @inbounds h.keys[index] = key
        @inbounds h.vals[index] = v
    else
        _setindex!(h, v, key, -index)
    end

    return h
end

function get!(h::OrderedDict{K,V}, key0, default) where {K,V}
    key = convert(K, key0)
    if !isequal(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end

    index = ht_keyindex2(h, key)

    index > 0 && return h.vals[index]

    v = convert(V,  default)
    _setindex!(h, v, key, -index)
    return v
end

function get!(default::Base.Callable, h::OrderedDict{K,V}, key0) where {K,V}
    key = convert(K, key0)
    if !isequal(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end

    index = ht_keyindex2(h, key)

    index > 0 && return h.vals[index]

    h.dirty = false
    v = convert(V,  default())
    if h.dirty  # calling default could have dirtied h
        index = ht_keyindex2(h, key)
    end
    if index > 0
        h.keys[index] = key
        h.vals[index] = v
    else
        _setindex!(h, v, key, -index)
    end
    return v
end

function getindex(h::OrderedDict{K,V}, key) where {K,V}
    index = ht_keyindex(h, key)
    return (index<0) ? throw(KeyError(key)) : h.vals[index]::V
end

function get(h::OrderedDict{K,V}, key, default) where {K,V}
    index = ht_keyindex(h, key)
    return (index<0) ? default : h.vals[index]::V
end

function get(default::Base.Callable, h::OrderedDict{K,V}, key) where {K,V}
    index = ht_keyindex(h, key)
    return (index<0) ? default() : h.vals[index]::V
end

haskey(h::OrderedDict, key) = (ht_keyindex(h, key) >= 0)
in(key, v::Base.KeySet{K,T}) where {K,T<:OrderedDict{K}} = (ht_keyindex(v.dict, key) >= 0)

function getkey(h::OrderedDict{K,V}, key, default) where {K,V}
    index = ht_keyindex(h, key)
    return (index<0) ? default : h.keys[index]::K
end

function _pop!(h::OrderedDict, index)
    @inbounds val = h.vals[h.slots[index]]
    _delete!(h, index)
    return val
end

function pop!(h::OrderedDict)
    h.ndel > 0 && rehash!(h)
    key = h.keys[h.len]
    index = ht_slotindex(h, key)
    return key => _pop!(h, index)
end

function popfirst!(h::OrderedDict)
    h.ndel > 0 && rehash!(h)
    key = h.keys[1]
    index = ht_slotindex(h, key)
    key => _pop!(h, index)
end

function pop!(h::OrderedDict, key)
    index = ht_slotindex(h, key)
    index > 0 ? _pop!(h, index) : throw(KeyError(key))
end

function pop!(h::OrderedDict, key, default)
    index = ht_slotindex(h, key)
    index > 0 ? _pop!(h, index) : default
end

function _delete!(h::OrderedDict, index)
    @inbounds ki = h.slots[index]
    @inbounds h.slots[index] = -ki
    @inbounds Base._unsetindex!(h.keys, Int(ki))
    @inbounds Base._unsetindex!(h.vals, Int(ki))
    h.ndel += 1
    h.dirty = true
    return h
end

function delete!(h::OrderedDict, key)
    index = ht_slotindex(h, key)
    if index > 0; _delete!(h, index); end
    return h
end

function iterate(t::OrderedDict{K,V}) where {K,V}
    t.ndel > 0 && rehash!(t)
    t.len < 1 && return nothing
    @inbounds return (Pair{K,V}(t.keys[1], t.vals[1]), 2)
end
function iterate(t::OrderedDict{K,V}, i) where {K,V}
    t.len < i && return nothing
    @inbounds return (Pair{K,V}(t.keys[i], t.vals[i]), i+1)
end

# lazy reverse iteration
function iterate(rt::Iterators.Reverse{<:OrderedDict{K,V}}) where {K,V}
    t = rt.itr
    t.ndel > 0 && rehash!(t)
    n = t.len
    n < 1 && return nothing
    @inbounds return (Pair{K,V}(t.keys[n], t.vals[n]), n - 1)
end
function iterate(rt::Iterators.Reverse{<:OrderedDict{K,V}}, i) where {K,V}
    t = rt.itr
    i < 1 && return nothing
    @inbounds return (Pair{K,V}(t.keys[i], t.vals[i]), i - 1)
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
    vals = dict.vals
    elements = length(dict)
    elements == 0 && return iter
    for i in dict.slots
        if i > 0
            @inbounds vals[i] = f(vals[i])
            elements -= 1
            elements == 0 && break
        end
    end
    return iter
end

last(h::OrderedDict) = h.keys[h.len] => h.vals[h.len]
