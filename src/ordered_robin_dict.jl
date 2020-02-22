import Base: setindex!, sizehint!, empty!, isempty, length, copy, empty,
             getindex, getkey, haskey, iterate, @propagate_inbounds,
             pop!, delete!, get, get!, isbitstype, in, hashindex, isbitsunion,
             isiterable, dict_with_eltype, KeySet, Callable, _tablesz, filter!

# the load factor after which the dictionary `rehash` happens
const ROBIN_DICT_LOAD_FACTOR = 0.70

mutable struct OrderedRobinDict{K,V} <: AbstractDict{K,V}
    slots::Vector{Int32}
    hashes::Vector{UInt32}
    keys::Array{K,1}
    vals::Array{V,1}
    count::Int
    idxfloor::Int
    ndel::Int
    dirty::Bool
end

function OrderedRobinDict{K, V}() where {K, V}
    n = 16
    OrderedRobinDict{K, V}(zeros(Int32, n), zeros(UInt32, n), Vector{K}(), Vector{V}(), 0, 0, 0, false)
end

function OrderedRobinDict{K, V}(d::OrderedRobinDict{K, V}) where {K, V}
    (d.ndel > 0) && rehash!(d)
    @assert d.ndel == 0
    new{K, V}(copy(d.slots), copy(d.hashes), copy(d.keys), copy(d.vals), d.count, d.idxfloor, d.ndel, d.dirty)
end

function OrderedRobinDict{K,V}(kv) where V where K
    h = OrderedRobinDict{K,V}()
    for (k,v) in kv
        h[k] = v
    end
    return h
end
OrderedRobinDict{K,V}(p::Pair) where {K,V} = setindex!(OrderedRobinDict{K,V}(), p.second, p.first)
function OrderedRobinDict{K,V}(ps::Pair...) where V where K
    h = OrderedRobinDict{K,V}()
    sizehint!(h, length(ps))
    for p in ps
        h[p.first] = p.second
    end
    return h
end

OrderedRobinDict() = OrderedRobinDict{Any,Any}()
OrderedRobinDict(kv::Tuple{}) = OrderedRobinDict()
copy(d::OrderedRobinDict) = OrderedRobinDict(d)
empty(d::OrderedRobinDict, ::Type{K}, ::Type{V}) where {K, V} = OrderedRobinDict{K, V}()

OrderedRobinDict(ps::Pair{K,V}...) where {K,V} = OrderedRobinDict{K,V}(ps)
OrderedRobinDict(ps::Pair...)                  = OrderedRobinDict(ps)

OrderedRobinDict(d::AbstractDict{K, V}) where {K, V} = OrderedRobinDict{K, V}(d)

function OrderedRobinDict(kv)
    try
        return dict_with_eltype((K, V) -> OrderedRobinDict{K, V}, kv, eltype(kv))
    catch e
        if !isiterable(typeof(kv)) || !all(x -> isa(x, Union{Tuple,Pair}), kv)
            !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError("OrderedRobinDict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end

hash_key(key) = (hash(key)%UInt32) | 0x80000000
desired_index(hash, sz) = (hash & (sz - 1)) + 1

function calculate_distance(h::OrderedRobinDict{K, V}, index) where {K, V}
    @assert isslotfilled(h, index)
    sz = length(h.keys)
    @inbounds index_init = desired_index(h.hashes[index], sz)
    return (index - index_init + sz) & (sz - 1)
end

@propagate_inbounds isslotfilled(h::OrderedRobinDict, index) = (h.hashes[index] != 0)
@propagate_inbounds isslotempty(h::OrderedRobinDict, index) = (h.hashes[index] == 0)

# insert algorithm
function rh_insert!(h::OrderedRobinDict{K, V}, key::K, val::V) where {K, V}
    # assume dict is not full
    ckey, cval, chash = key, val, hash_key(key)
    sz = length(h.slots)
    index_init = desired_index(chash, sz)

    index_curr = index_init
    probe_distance = 0
    probe_current = 0

    @inbounds while true
        if (isslotempty(h, index_curr) || (isslotfilled(h, index_curr) && isequal(h.keys[h.slots[index_curr], ckey]))
            break
        end
        probe_distance = calculate_distance(h, index_curr)

        if probe_current > probe_distance
            # the mirror-index
            si = h.slots[index_curr]


        end
        probe_current += 1
        index_curr = (index_curr & (sz - 1)) + 1
    end

    @inbounds if (isslotfilled(h, index_curr) && isequal(h.keys[h.slots[index_curr], ckey]))
        h.vals[h.slots[index_curr]] = ckey
        return index_curr
    end

    @inbounds if isslotempty(h, index_curr)
        h.count += 1
        hk, hv = h.keys, h.vals
        ccall(:jl_array_grow_end, Cvoid, (Any, UInt), hk, 1)
        nk = length(hk)
        @inbounds hk[nk] = ckey
        ccall(:jl_array_grow_end, Cvoid, (Any, UInt), hv, 1)
        @inbounds hv[nk] = cval
        h.dirty = true
        # mirror-indexing here
        h.slots[index_curr] = nk
        h.hashes[index_curr] = chash
        return index_curr
    end
end

function empty!(h::OrderedRobinDict{K,V}) where {K, V}
    sz = length(h.slots)
    empty!(h.hashes)
    empty!(h.slots)
    empty!(h.keys)
    empty!(h.vals)
    resize!(h.hashes, sz)
    h.count = 0
    h.ndel = 0
    h.dirty = false
    h.idxfloor = 0
    return h
end

@propagate_inbounds function iterate(t::OrderedRobinDict)
    t.count == 0 && return nothing
    return (Pair(t.keys[1], t.val[1]), 2)
end
@propagate_inbounds function iterate(t::OrderedRobinDict, i)
    t.count < i && return nothing
    return (Pair(t.keys[i], t.val[1]), i+1)
end
