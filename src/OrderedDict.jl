
struct OrderedDict{K,V,L,S} <: AbstractDict{K,V}
    keys::OrderedSet{K,L,S}
    values::Vector{V}

    global function _OrderedDict(keys::OrderedSet{K,L,S}, values::Vector{V}) where {K,V,L,S}
        new{K,V,L,S}(keys, values)
    end
end

OrderedDict{K,V}() where {K,V} = _OrderedDict(OrderedSet{K}(), Vector{V}())
OrderedDict{K,V}(hs::HashSettings) where {K,V} = _OrderedDict(OrderedSet{K}(hs), Vector{V}())
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
    _OrderedDict(copy(keys(d)), copy(_values(d)))
end

OrderedDict() = OrderedDict{Any,Any}()
OrderedDict(kv::Tuple{}) = OrderedDict()

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

const OrderedValues{K,V,L,S} = Base.ValueIterator{OrderedDict{K,V,L,S}}

_values(d::OrderedDict) = getfield(d, :values)
_values(d::OrderedValues) = getfield(getfield(d, 1), :values)

Base.keys(d::OrderedDict) = getfield(d, :keys)
Base.length(d::OrderedDict) = length(_values(d))

Base.isempty(d::OrderedDict) = length(d) === 0

Base.empty(d::OrderedDict{K,V}) where {K,V} = OrderedDict{K,V}()
Base.empty(d::OrderedDict, ::Type{K}, ::Type{V}) where {K, V} = OrderedDict{K, V}()

function Base.sizehint!(d::OrderedDict, newsz)
    sizehint!(keys(d), newsz)
    sizehint!(_values(d), newsz)
    return d
end

Base.haskey(d::OrderedDict, key) = in(key, keys(d))

function Base.getkey(d::OrderedDict{K}, key0, default) where {K}
    key = try_convert(K, key0)
    return in(key, keys(d)) ? key : default
end

function Base.empty!(d)
    empty!(keys(d))
    empty!(_values(d))
    return d
end

function Base.last(d::OrderedDict{K,V}) where {K,V}
    ks = keys(d)
    n = length(ks)
    key = _values(ks)[n]
    val = _get(_values(d), n)
    return Pair{K,V}(key, val)
end

function Base.iterate(d::OrderedDict{K,V}) where {K,V}
    if length(d) === 0
        return nothing
    else
        return (Pair{K,V}(_get(_values(keys(d)), 1), unsafe_get(_values(d), 1)), 2)
    end
end
function Base.iterate(d::OrderedDict{K,V}, i::Int) where {K,V}
    if i > length(d)
        return nothing
    else
        return (Pair{K,V}(_get(_values(keys(d)), i), unsafe_get(_values(d), i)), i + 1)
    end
end
function Base.iterate(d::OrderedValues)
    vs = _values(d)
    return length(vs) === 0 ? nothing : (unsafe_get(vs, 1), 2)
end
function Base.iterate(d::OrderedValues, i::Int)
    vs = _values(d)
    return i > length(vs) ? nothing : (unsafe_get(vs, i), i + 1)
end

Base.copy(d::OrderedDict) = OrderedDict(d)

function Base.setindex!(d::OrderedDict{K,V}, val0, key) where {K,V}
    val = try_convert(V, val0)
    success, index = try_push!(keys(d), key)
    if success
        vs = _values(d)
        unsafe_grow_end!(vs, 1)
        unsafe_set!(vs, index, val)
    else
        unsafe_set!(_values(d), index, val)
    end
    return d
end

function Base.getindex(d::OrderedDict, key)
    flag, slot, index = lookup(keys(d), key)
    @boundscheck flag === 0x02 || throw(KeyError(key))
    return unsafe_get(_values(d), slot)
end

function Base.get(d::OrderedDict, key, default)
    flag, index = lookup(keys(d), key)
    flag === 0x02 ? _get(_values(d), index) : default
end
function Base.get(default::Base.Callable, d::OrderedDict, key)
    flag, index = lookup(keys(d), key)
    flag === 0x02 ? _get(_values(d), index) : default()
end

function Base.get!(d::OrderedDict{K}, key, default) where {K}
    s = keys(d)
    age = s.age
    out = _get!(d, try_convert(K, key), default)
    @assert s.age === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return out
end
function Base.get!(f::Union{Type,Function}, d::OrderedDict{K}, key) where {K}
    s = keys(d)
    age = s.age
    out = _get!(f, d, try_convert(K, key))
    @assert s.age === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return out
end
function _get!(d::OrderedDict, key, default)
    s = keys(d)
    ks = _values(s)
    slots = _slots(s)
    hs = _settings(s)
    nkeys = length(ks)
    nslots = length(slots)
    mask = nslots - 1
    nkplus = nkeys + 1
    vs = _values(d)
    flag, idx = try_insert_slot2!(ks, slots, hs, nkeys, nslots, mask, key, nkplus)
    if flag === 0x02
        return unsafe_get(vs, idx)
    else
        nkplus = nkeys + 1
        unsafe_grow_end!(ks, 1)
        unsafe_set!(ks, nkplus, key)
        unsafe_grow_end!(vs, 1)
        unsafe_set!(vs, nkplus, default)
        return default
    end
end
function _get!(f::Union{Type,Function}, d::OrderedDict, key)
    s = keys(d)
    ks = _values(s)
    slots = _slots(s)
    hs = _settings(s)
    nkeys = length(ks)
    nslots = length(slots)
    mask = nslots - 1
    nkplus = nkeys + 1
    vs = _values(d)
    flag, idx = try_insert_slot2!(ks, slots, hs, nkeys, nslots, mask, key, nkplus)
    if flag === 0x02
        return unsafe_get(vs, idx)
    else
        nkplus = nkeys + 1
        unsafe_grow_end!(ks, 1)
        unsafe_set!(ks, nkplus, key)
        unsafe_grow_end!(vs, 1)
        default = f()
        unsafe_set!(vs, nkplus, default)
        return default
    end
end

function Base.delete!(d::OrderedDict, key)
    ks = keys(d)
    age = ks.age
    success, index = try_delete!(ks, key)
    success && unsafe_delete_at!(_values(d), index, 1)
    @assert ks.age === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(ks)
    return d
end

function Base.pop!(d::OrderedDict{K,V}) where {K,V}
    ks = keys(d)
    key = pop!(ks)
    vs = _values(d)
    val = vs[end]
    unsafe_delete_end!(vs, 1)
    return Pair{K,V}(key, val)
end
function Base.popfirst!(d::OrderedDict{K,V}) where {K,V}
    ks = keys(d)
    key = popfirst!(ks)
    vs = _values(d)
    val = vs[begin]
    unsafe_delete_beg!(vs, 1)
    return Pair{K,V}(key, val)
end
function Base.pop!(d::OrderedDict, key)
    s = keys(d)
    age = s.age
    success, index = try_delete!(s, key)
    success || throw(KeyError(key))
    vs = _values(d)
    out = unsafe_get(vs, index)
    unsafe_delete_at!(vs, index, 1)
    @assert s.age === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return out
end

function Base.pop!(d::OrderedDict, key, default)
    ks = keys(d)
    vs = _values(d)
    age = ks.age
    success, index = try_delete!(ks, key)
    if success
        out = unsafe_get(vs, index)
        unsafe_delete_at!(vs, index, 1)
    else
        out = default
    end
    @assert ks.age === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(ks)
    return out
end

function _merge_kvtypes(d, others...)
    K, V = keytype(d), valtype(d)
    for other in others
        K = promote_type(K, keytype(other))
        V = promote_type(V, valtype(other))
    end
    return (K,V)
end

function Base.merge(d::OrderedDict, others::AbstractDict...)
    K,V = _merge_kvtypes(d, others...)
    merge!(OrderedDict{K,V}(), d, others...)
end

function Base.merge(combine::Function, d::OrderedDict, others::AbstractDict...)
    K,V = _merge_kvtypes(d, others...)
    merge!(combine, OrderedDict{K,V}(), d, others...)
end

#= TODO
function Base.map!(f, iter::Base.ValueIterator{<:OrderedDict})
    dict = iter.dict
    vals = dict.vals
    elements = length(vals) - dict.ndel
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
=#

function Base.showarg(io::IO, s::OrderedDict, toplevel::Bool)
    !toplevel && print(io, "::")
    print(io, "OrderedSet{$(keytype(s)), $(valtype(s))}")
end

#=
function Base.show(io::IO, s::OrderedDict)
    Base.showarg(io, s, true)
    print(io, "(")
    !isempty(s) && Base.show_vector(io, s,'[',']')
    print(io, ")")
end
=#


