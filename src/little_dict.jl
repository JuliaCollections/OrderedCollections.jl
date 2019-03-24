const StoreType = Union{<:Tuple, <:Vector}

"""
    LittleDict(keys, vals)<:AbstractDict

A ordered dictionary type for small numbers of keys.
Rather than using `hash` or some other sophisicated measure
to store the vals in a clever arrangement,
it just keeps everything in a pair of lists.

While theoretically this has expected time complexity _O(n)_,
vs the hash-based `OrderDict`/`Dict`'s expected time complexity _O(1)_,
and the search-tree-based `SortedDict`'s expected time complcity _O(log(n))_.
In practice it is really fast, because it is cache & SIMD friendly.

It is reasonable to expect it to outperform an `OrderedDict`,
with up to around 30 elements in general;
or with up to around 50 elements if using a `LittleDict` backed by `Tuples`
(see [`freeze`](@ref))
However, this depends on exactly how long `isequal` and `hash` take,
as well as on how many hash collisions occur etc.
"""
struct LittleDict{K,V,KS<:StoreType,VS<:StoreType} <: AbstractDict{K, V}
    keys::KS
    vals::VS
end

function LittleDict(ks::KS, vs::VS) where {KS,VS}
    return LittleDict{eltype(KS), eltype(VS), KS, VS}(ks, vs)
end


function LittleDict{K,V}(itr) where {K,V}
    ks = K[]
    vs = V[]
    for val in itr
        if !(val isa Union{Tuple{<:Any, <:Any}, Pair})
            throw(ArgumentError(
                "LittleDict(kv): kv needs to be an iterator of tuples or pairs")
            )
        end
        k, v = val
        push!(ks, k)
        push!(vs, v)
    end
    return LittleDict(ks, vs)
end

#LittleDict() = LittleDict{Any, Any}()
LittleDict{K,V}(itr...) where {K,V} = LittleDict{K,V}(itr)
LittleDict(itr...) = LittleDict(itr)
LittleDict(itr::T) where T = LittleDict{kvtype(eltype(T))...}(itr)

# Avoid contention between the core constructor, and the list of elements
LittleDict(itr1::Pair, itr2::Pair) = LittleDict(first.([itr1, itr2]), last.([itr1,itr2]))
LittleDict(itr1::Pair) = LittleDict([first(itr1)], [last(itr1)])

kvtype(::Any) = (Any, Any)
kvtype(::Type{Union{}}) = (Any,Any)

kvtype(::Type{Pair{K,V}}) where {K,V} = (K,V)
kvtype(::Type{Pair{<:Any,V}}) where {V} = (Any,V)
kvtype(::Type{Pair{K,<:Any}}) where {K} = (K,Any)

kvtype(::Type{Tuple{K,V}}) where {K,V} = (K,V)
kvtype(::Type{Tuple{<:Any,V}}) where {V} = (Any,V)
kvtype(::Type{Tuple{K,<:Any}}) where {K} = (K,Any)

"""
    freeze(dd::AbstractDict)
Render an dictionary immutable by converting it to a `Tuple` backed
`LittleDict`.
This will make it faster if it is small enough.
In particular the `Tuple` backed `LittleDict` is faster than the
`Vector` backed `LittleDict`.
"""
function freeze(dd::AbstractDict)
    ks = Tuple(keys(dd))
    vs = Tuple(values(dd))
    return LittleDict(ks, vs)
end

##### Methods that all AbstractDicts should implement

Base.length(dd::LittleDict) = length(dd.keys)

function Base.getkey(dd::LittleDict, key, default)
    if key ∈ dd.keys
        return key
    else
        return default
    end
end

struct NotFoundSentinel end  # Struct to mark not not found
function Base.get(dd::LittleDict, key, default)
    @assert length(dd.keys) == length(dd.vals)
    @simd for ii in 1:length(dd.keys)
        cand = @inbounds dd.keys[ii]
        isequal(cand, key) && return @inbounds(dd.vals[ii])
    end
    return default
end
function get(default::Base.Callable, dd::LittleDict, key)
    got = get(dd, key, NotFoundSentinel())
    if got isa NotFoundSentinel  # not found
        return default()
    else
        return got
    end
end

function Base.iterate(dd::LittleDict, ii=1)
    ii > length(dd.keys) && return nothing
    return (dd.keys[ii] => dd.vals[ii], ii+1)
end


######## Methods that all mutable AbstractDict's should implement

function Base.sizehint!(dd::LittleDict, sz)
    sizehint!(dd.keys, sz)
    sizehint!(dd.vals,sz)
    return dd
end

function add_new!(dd::LittleDict, key, value)
    # Not found, add to the end
    push!(dd.keys, key)
    try
        push!(dd.vals, value)
    catch
        # if we sucessfully added  a key, but failed to add a value
        # then we need to remove the key so the little dict remains with constistant state
        pop!(dd.keys)
        rethrow()
    end
    return value
end

function Base.setindex!(dd::LittleDict, value, key)
    @assert length(dd.keys) == length(dd.vals)
    @simd for ii in 1:length(dd.keys)
        cand = @inbounds dd.keys[ii]
        isequal(cand, key) && return @inbounds(dd.vals[ii] = value)
    end
    return add_new!(dd, key, value)
end

function Base.pop!(dd::LittleDict)
    pop!(dd.keys)
    return pop!(dd.vals)
end

function Base.pop!(dd::LittleDict, key)
    @assert length(dd.keys) == length(dd.vals)
    
    @simd for ii in 1:length(dd.keys)
        cand = @inbounds dd.keys[ii]
        if isequal(cand, key)
            deleteat!(dd.keys, ii)
            val = @inbounds dd.vals[ii]
            deleteat!(dd.vals, ii)
            return val
        end
    end
    # Not found, throw error
    throw(KeyError(key))
end

function Base.delete!(dd::LittleDict, key)
    pop!(dd, key)
    return dd
end

Base.empty!(dd::LittleDict) = (empty!(dd.keys); empty!(dd.vals); dd)

function get!(default::Base.Callable, dd::LittleDict, key)
    got = get(dd, key, NotFoundSentinel())
    if got isa NotFoundSentinel  # not found
        return add_new!(dd, key, default())
    else
        return got
    end
end
get!(dd::LittleDict, key, default) = get!(()->default, dd, key)
