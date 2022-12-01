
struct LazyOrderedSet{T,D<:StoreType} <: AbstractSet{T}
    data::D

    function LazyOrderedSet{T,D}(data) where {T,D}
        T<:eltype(D) || ArgumentError("Invalid store type $D, for set wieht  elements of type $T.")
        return new(data)
    end
    LazyOrderedSet{T}() where {T} = new{T,Vector{T}}(Vector{T}())
    LazyOrderedSet{T}(data::Vector{T}) where {T} = new{T,Vector{T}}(data)
    LazyOrderedSet{T}(data::Tuple{Vararg{T}}) where {T} = new{T,typeof(data)}(data)
    LazyOrderedSet{T}(itr) where {T} = union!(LazyOrderedSet{T}(), itr)
end

LazyOrderedSet() = LazyOrderedSet{Any}()
LazyOrderedSet(itr) = _LazyOrderedSet(itr, Base.IteratorEltype(itr))
_LazyOrderedSet(itr, ::Base.HasEltype) = LazyOrderedSet{eltype(itr)}(itr)
function _LazyOrderedSet(itr, ::Base.EltypeUnknown)
    T = Base.@default_eltype(itr)
    if (Base.isconcretetype(T) || T === Union{})
        return LazyOrderedSet{T}(itr)
    else
        return Base.grow_to!(LazyOrderedSet{T}(), itr)
    end
end

const FrozenLazyOrderedSet{T} = LazyOrderedSet{T,<:Tuple}
const UnfrozenLazyOrderedSet{T} = LazyOrderedSet{T,Vector{T}}

Base.Tuple(s::FrozenLazyOrderedSet) = s.data
Base.Tuple(s::UnfrozenLazyOrderedSet) = Tuple(s.data)

# lookup the position of `val`
function lookup(s::LazyOrderedSet, val)
    data = s.data
    i = length(data)
    while i > 0
        isequal(@inbounds(data[i]), val) && return i
        i -= 1
    end
    return -1
end

freeze(s::AbstractSet{T}) where {T} = LazyOrderedSet{T}(Tuple(s))

Base.length(s::LazyOrderedSet) = length(s.data)

Base.isempty(s::LazyOrderedSet) = length(s) == 0

Base.iterate(s::LazyOrderedSet, state=1) = iterate(s.data, state)

Base.in(x, s::LazyOrderedSet) = in(x, s.data)

function Base.push!(s::UnfrozenLazyOrderedSet, val)
    in(val, s) || push!(s.data, val)
    return s
end

Base.pop!(dd::UnfrozenLazyOrderedSet) = pop!(s.data)

function Base.pop!(s::UnfrozenLazyOrderedSet, key)
    index = lookup(s, key)
    deleteat!(s.data, index)
    key
end

Base.empty!(s::UnfrozenLazyOrderedSet) = (empty!(s.data); s)

function Base.delete!(dd::UnfrozenLazyOrderedSet, key)
    pop!(dd, key)
    return dd
end

