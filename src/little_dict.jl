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

#LittleDict{K,V}() where {K,V} = LittleDict{K,V}(tuple())
LittleDict() = LittleDict{Any, Any}()
LittleDict{K,V}(itr...) where {K,V} = LittleDict{K,V}(itr)
LittleDict(itr::T) where T = LittleDict{kvtype(eltype(T))...}(itr)


LittleDict(kv::Pair) = LittleDict([first(kv)], [last(kv)])
function LittleDict(itr1::Pair{K,V}, itrs::Pair{K,V}...) where {K,V}
    return LittleDict{K,V}(itr1, itrs...)
end
#LittleDict(itr1::Pair, itr2::Pair, itrs::Pair...) = LittleDict(itr1, itr2, itrs...)

kvtype(::Any) = (Any, Any)
kvtype(::Type{Union{}}) = (Any,Any)
kvtype(::Type{<:Pair{K,V}}) where {K,V} = (K,V)
kvtype(::Type{<:Tuple{K,V}}) where {K,V} = (K,V)

Base.sizehint!(dd::LittleDict) = (sizehint!(dd.ks); sizehint!(dd.vs))

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

function Base.get(dd::LittleDict, key, default)
    @assert length(dd.keys) == length(dd.vals)
    @simd for ii in 1:length(dd.keys)
        cand = @inbounds dd.keys[ii]
        isequal(cand, key) && return @inbounds(dd.vals[ii])
    end
    return default
end

function Base.iterate(dd::LittleDict, ii=1)
    ii > length(dd.keys) && return nothing
    return (dd.keys[ii] => dd.vals[ii], ii+1)
end


######## Methods that all mutable AbstractDict's should implement

function Base.setindex!(dd::LittleDict, value, key)
    @assert length(dd.keys) == length(dd.vals)
    @simd for ii in 1:length(dd.keys)
        cand = @inbounds dd.keys[ii]
        isequal(cand, key) && return @inbounds(dd.vals[ii] = value)
    end
    # Not found, add to the end
    push!(dd.keys, key)
    push!(dd.vals, value)
    return value
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
