import Base: setindex!, sizehint!, empty!, isempty, length, copy, empty,
             getindex, getkey, haskey, iterate, @propagate_inbounds,
             pop!, delete!, get, get!, isbitstype, in, merge, isiterable,
             dict_with_eltype, KeySet, Callable, _tablesz, filter!

const ALLOWABLE_USELESS_GROWTH = 0.25

mutable struct OrderedGeneralDict{K,V, DT <: AbstractDict{K, Int32}} <: AbstractDict{K,V}
    dict::DT 
    keys::Vector{K}
    vals::Vector{V}
    count::Int32

    function OrderedGeneralDict{K, V, DT}() where {K, V, DT}
        new{K, V, DT}(DT(), Vector{K}(), Vector{V}(), 0)
    end

    function OrderedGeneralDict{K, V, Dict{K, Int32}}() where {K, V}
        new{K, V, Dict{K, Int32}}(Dict{K, Int32}(), Vector{K}(), Vector{V}(), 0)
    end

    function OrderedGeneralDict{K, V, DT}(d::OrderedGeneralDict{K, V, DT}) where {K, V, DT}
        new{K, V, DT}(copy(d.dict), copy(d.keys), copy(d.vals), d.count)
    end

    function OrderedGeneralDict{K,V, DT}(kv) where {K, V, DT}
        h = OrderedGeneralDict{K,V, DT}()
        for (k,v) in kv
            h[k] = v
        end
        return h
    end
    OrderedGeneralDict{K,V, DT}(p::Pair) where {K,V, DT} = setindex!(OrderedGeneralDict{K,V,DT}(), p.second, p.first)
    function OrderedGeneralDict{K,V,DT}(ps::Pair...) where {K, V, DT}
        h = OrderedGeneralDict{K,V, DT}()
        sizehint!(h, length(ps))
        for p in ps
            h[p.first] = p.second
        end
        return h
    end
end

OrderedGeneralDict() = OrderedGeneralDict{Any,Any, Dict{Any, Int32}}()
OrderedGeneralDict(kv::Tuple{}) = OrderedGeneralDict()

OrderedGeneralDict(ps::Pair{K,V}...) where {K,V} = OrderedGeneralDict{K,V, Dict{K, Int32}}(ps)
OrderedGeneralDict(ps::Pair...)                  = OrderedGeneralDict(ps)

function OrderedGeneralDict(kv)
    try
        return dict_with_eltype((K, V) -> OrderedGeneralDict{K, V, Dict{K, Int32}}, kv, eltype(kv))
    catch e
        if !isiterable(typeof(kv)) || !all(x -> isa(x, Union{Tuple,Pair}), kv)
            !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError("OrderedGeneralDict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end

copy(d::OrderedGeneralDict) = OrderedGeneralDict(d)
empty(d::OrderedGeneralDict, ::Type{K}, ::Type{V}) where {K, V} = OrderedGeneralDict{K, V, Dict{K, Int32}}()
empty(d::OrderedGeneralDict{K,V, DT}) where {K,V,DT} = OrderedGeneralDict{K,V, DT}()

length(d::OrderedGeneralDict) = d.count
isempty(d::OrderedGeneralDict) = (length(d) == 0)

"""
    OrderedDict
`OrderedDict`s are  simply dictionaries  whose entries  have a  particular order.  The order
refers to insertion order, which allows deterministic iteration over the dictionary or set.
"""
struct OrderedDict{K, V} <: AbstractDict{K, V}
end

OrderedDict{K, V}() where {K, V} = OrderedGeneralDict{K, V, Dict{K, Int32}}()
OrderedDict() = OrderedDict{Any, Any}()

OrderedDict(ps::Pair{K,V}...) where {K,V} = OrderedGeneralDict{K,V, Dict{K, Int32}}(ps)
OrderedDict(ps::Pair...)                  = OrderedGeneralDict(ps)

OrderedDict(d::AbstractDict{K, V}) where {K, V} = OrderedGeneralDict{K, V, Dict{K, Int32}}(d)

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

function OrderedDict(kv)
    try
        return dict_with_eltype((K, V) -> OrderedGeneralDict{K, V, Dict{K, Int32}}, kv, eltype(kv))
    catch e
        if !isiterable(typeof(kv)) || !all(x -> isa(x, Union{Tuple,Pair}), kv)
            !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError("OrderedDict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end

# conversion between OrderedDict types
function Base.convert(::Type{OrderedDict{K,V}}, d::AbstractDict) where {K,V}
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
Base.convert(::Type{OrderedDict{K,V}},d::OrderedDict{K,V}) where {K,V} = d

empty(d::OrderedDict{K,V}) where {K,V} = OrderedDict{K,V}()
empty(d::OrderedDict, ::Type{K}, ::Type{V}) where {K, V} = OrderedDict{K, V}() 

function empty!(h::OrderedGeneralDict{K,V}) where {K, V}
    empty!(h.dict)
    empty!(h.keys)
    empty!(h.vals)
    h.count = 0
    return h
end

function _setindex!(h::OrderedGeneralDict, v, key)
    hk, hv = h.keys, h.vals
    ccall(:jl_array_grow_end, Cvoid, (Any, UInt), hk, 1)
    nk = length(hk)
    @inbounds hk[nk] = key
    ccall(:jl_array_grow_end, Cvoid, (Any, UInt), hv, 1)
    @inbounds hv[nk] = v
    @inbounds h.dict[key] = Int32(nk)
    h.count += 1
end

function setindex!(h::OrderedGeneralDict{K, V}, v0, key0) where {K,V}
    key = convert(K,key0)
    if !isequal(key,key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end
    v = convert(V,  v0)

    index = get(h.dict, key, -2)

    if index < 0
        _setindex!(h, v0, key0)
    else
        @assert haskey(h, key0)
        @inbounds orig_v = h.vals[index]
        (orig_v != v0) && (@inbounds h.vals[index] = v0)
    end

    check_for_rehash(h) && rehash!(h)

    return h
end

# rehash when there are ALLOWABLE_USELESS_GROWTH %
# tombstones, or non-mirrored entries in the dictionary
function check_for_rehash(h::OrderedGeneralDict)
    keysl = length(h.keys)
    dictl = length(h)
    return (keysl > (1 + ALLOWABLE_USELESS_GROWTH)*dictl)
end

function rehash!(h::OrderedGeneralDict{K, V}) where {K, V}
    keys = h.keys
    vals = h.vals
    newk = Vector{K}()
    newv = Vector{V}()
    hk, hv = newk, newv
    
    for (idx, (k, v)) in enumerate(zip(keys, vals))
        if get(h.dict, k, -1) == idx
            ccall(:jl_array_grow_end, Cvoid, (Any, UInt), hk, 1)
            nk = length(hk)
            @inbounds hk[nk] = k
            ccall(:jl_array_grow_end, Cvoid, (Any, UInt), hv, 1)
            @inbounds hv[nk] = v
        end
    end
    
    h.keys = newk
    h.vals = newv
    
    for (idx, k) in enumerate(h.keys)
        h.dict[k] = idx
    end
    return h
end

function sizehint!(d::OrderedGeneralDict, newsz)
    oldsz = length(d)
    # grow at least 25%
    if newsz < (oldsz*5)>>2
        return d
    end
    sizehint!(d.keys, newsz)
    sizehint!(d.vals, newsz)
    sizehint!(d.dict, newsz)
    return d
end

function get!(h::OrderedGeneralDict{K,V}, key0, default) where {K,V}
    key = convert(K,key0)
    if !isequal(key,key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end

    index = get(h.dict, key, -2)

    index > 0 && return h.vals[index]

    v = convert(V,  default)
    setindex!(h, v, key)
    return v
end

function get!(default::Base.Callable, h::OrderedGeneralDict{K,V}, key0) where {K,V}
    key = convert(K,key0)
    if !isequal(key,key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end

    index = get(h.dict, key, -2)

    index > 0 && return h.vals[index]

    v = convert(V,  default())

    if index > 0
        h.keys[index] = key
        h.vals[index] = v
    else
        setindex!(h, v, key)
    end
    return v
end

function getindex(h::OrderedGeneralDict{K,V}, key) where {K,V}
    index = get(h.dict, key, -1)
    return (index < 0) ? throw(KeyError(key)) : h.vals[index]::V
end

function get(h::OrderedGeneralDict{K,V}, key, default) where {K,V}
    index = get(h.dict, key, -1)
    return (index < 0) ? default : h.vals[index]::V
end

function get(default::Base.Callable, h::OrderedGeneralDict{K,V}, key) where {K,V}
    index = get(h.dict, key, -1)
    return (index < 0) ? default() : h.vals[index]::V
end

haskey(h::OrderedGeneralDict, key) = (get(h.dict, key, -2) > 0)
in(key, v::Base.KeySet{K,T}) where {K,T<:OrderedGeneralDict{K}} = (get(v.dict, key, -1) >= 0)

function getkey(h::OrderedGeneralDict{K,V}, key, default) where {K,V}
    index = get(h.dict, key, -1)
    return (index < 0) ? default : h.keys[index]::K
end

@propagate_inbounds isslotfilled(h::OrderedGeneralDict, index) = (h.dict[h.keys[index]] == index)

function _pop!(h::OrderedGeneralDict, index)
    @inbounds val = h.vals[index]
    _delete!(h, index)
    return val
end

function pop!(h::OrderedGeneralDict)
    isempty(h) && throw(ArgumentError("dic"))
    check_for_rehash(h) && rehash!(h)
    index = length(h.keys)
    @inbounds while (index > 0)
        isslotfilled(h, index) && break
        index -= 1
    end
    index == 0 && rehash!(h)
    @inbounds key = h.keys[index]
    return key => _pop!(h, index)
end

function pop!(h::OrderedGeneralDict, key)
    index = get(h.dict, key, -1)
    (index > 0) ? _pop!(h, index) : throw(KeyError(key))
end

function pop!(h::OrderedGeneralDict, key, default)
    index = get(h.dict, key, -1)
    (index > 0) ? _pop(h, index) : default
end

function delete!(h::OrderedGeneralDict, key)
    pop!(h, key)
    return h
end

function _delete!(h::OrderedGeneralDict, index)
    @inbounds h.dict[h.keys[index]] = -1
    h.count -= 1
    check_for_rehash(h) ? rehash!(h) : h
end

function get_first_filled_index(h::OrderedGeneralDict)
    index = 1
    while (true)
        isslotfilled(h, index) && return index
        index += 1
    end
end

function get_next_filled_index(h::OrderedGeneralDict, index)
    # get the next filled slot, including index and beyond
    while (index <= length(h.keys))
        isslotfilled(h, index) && return index
        index += 1
    end
    return -1
end

function iterate(h::OrderedGeneralDict)
    isempty(h) && return nothing
    check_for_rehash(h) && rehash!(h)
    index = get_first_filled_index(h)
    return (Pair(h.keys[index], h.vals[index]), index+1)
end

function iterate(h::OrderedGeneralDict, i)
    length(h.keys) < i && return nothing
    index = get_next_filled_index(h, i) 
    (index < 0) && return nothing
    return (Pair(h.keys[index], h.vals[index]), index+1)
end

filter!(f, d::OrderedGeneralDict) = Base.filter_in_one_pass!(f, d)

function _merge_kvtypes(d, others...)
    K, V = keytype(d), valtype(d)
    for other in others
        K = promote_type(K, keytype(other))
        V = promote_type(V, valtype(other))
    end
    return (K,V)
end

function merge(d::OrderedGeneralDict, others::AbstractDict...)
    K,V = _merge_kvtypes(d, others...)
    merge!(OrderedDict{K,V}(), d, others...)
end

function merge(combine::Function, d::OrderedGeneralDict, others::AbstractDict...)
    K,V = _merge_kvtypes(d, others...)
    merge!(combine, OrderedDict{K,V}(), d, others...)
end

"""
    isordered(::Type)
Property of associative containers, that is `true` if the container type has a
defined order (such as `OrderedDict` and `SortedDict`), and `false` otherwise.
"""
isordered(::Type{T}) where {T <: OrderedGeneralDict} = true
isordered(::Type{T}) where {T<:AbstractDict} = false
isordered(::Type{T}) where {T<:OrderedDict} = true

function Base.map!(f, iter::Base.ValueIterator{<:OrderedGeneralDict})
    h = iter.dict
    vals = h.vals
    elements = length(h)
    elements == 0 && return iter
    for i in values(h.dict)
        if i > 0
            @inbounds vals[i] = f(vals[i])
            elements -= 1
            elements == 0 && break
        end
    end
    return iter
end