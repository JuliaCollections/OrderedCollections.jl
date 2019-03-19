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
struct LittleDict{K,V,KS,VS} <: AbstractDict{K, V}
    keys::KS
    vals::VS
end

function LittleDict(ks::KS, vs::VS) where {KS,VS}
    return LittleDict{eltype(KS), eltype(VS), KS, VS}(ks, vs)
end


function LittleDict{K,V}(itr) where {K,V}
    ks = K[]
    vs = V[]
    for (k, v) in itr
        push!(ks, k)
        push!(vs, v)
    end
    return LittleDict(ks, vs)
end

LittleDict{K,V}() where {K,V} = LittleDict{K,V}(tuple())
LittleDict() = LittleDict{Any, Any}()
LittleDict(itr::T) where T = LittleDict{kvtype(eltype(T))...}(itr)

kvtype(::Any) = (Any, Any)
kvtype(::Type{<:Pair{K,V}}) where {K,V} = (K,V)
kvtype(::Type{<:Tuple{K,V}}) where {K,V} = (K,V)


Base.length(dd::LittleDict) = length(dd.keys)
Base.sizehint!(dd::LittleDict) = (sizehint!(dd.ks); sizehint!(dd.vs))

function Base.getindex(dd::LittleDict, key)
    @assert length(dd.keys) == length(dd.vals)
    @simd for ii in 1:length(dd.keys)
        cand = @inbounds dd.keys[ii]
        isequal(cand, key) && return @inbounds(dd.vals[ii])
    end
    throw(KeyError(key))
end

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

Base.iterate(dd::LittleDict, args...) = iterate(zip(dd.keys, dd.vals), args...)


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
