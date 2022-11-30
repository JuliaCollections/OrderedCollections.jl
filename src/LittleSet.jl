
struct LittleSet{T,D<:StoreType} <: AbstractSet{T}
    data::D

    function LittleSet{T,D}(data) where {T,D}
        T<:eltype(D) || ArgumentError("Invalid store type $D, for set wieht  elements of type $T.")
        return new(data)
    end
    LittleSet{T}() where {T} = new{T,Vector{T}}(Vector{T}())
    LittleSet{T}(data::Vector{T}) where {T} = new{T,Vector{T}}(data)
    LittleSet{T}(data::Tuple{Vararg{T}}) where {T} = new{T,typeof(data)}(data)
    LittleSet{T}(itr) where {T} = union!(LittleSet{T}(), itr)
end

LittleSet() = LittleSet{Any}()
LittleSet(itr) = _LittleSet(itr, Base.IteratorEltype(itr))
_LittleSet(itr, ::Base.HasEltype) = LittleSet{eltype(itr)}(itr)
function _LittleSet(itr, ::Base.EltypeUnknown)
    T = Base.@default_eltype(itr)
    if (Base.isconcretetype(T) || T === Union{})
        return LittleSet{T}(itr)
    else
        return Base.grow_to!(LittleSet{T}(), itr)
    end
end

const FrozenLittleSet{T} = LittleSet{T,<:Tuple}
const UnfrozenLittleSet{T} = LittleSet{T,Vector{T}}

Base.Tuple(s::FrozenLittleSet) = s.data
Base.Tuple(s::UnfrozenLittleSet) = Tuple(s.data)

# lookup the position of `val`
function lookup(s::LittleSet, val)
    data = s.data
    i = length(data)
    while i > 0
        isequal(@inbounds(data[i]), val) && return i
        i -= 1
    end
    return -1
end

freeze(s::AbstractSet{T}) where {T} = LittleSet{T}(Tuple(s))

Base.length(s::LittleSet) = length(s.data)

Base.isempty(s::LittleSet) = length(s) == 0

Base.iterate(s::LittleSet, state=1) = iterate(s.data, state)

Base.in(x, s::LittleSet) = in(x, s.data)

function Base.push!(s::UnfrozenLittleSet, val)
    in(val, s) || push!(s.data, val)
    return s
end

Base.pop!(dd::UnfrozenLittleSet) = pop!(s.data)

function Base.pop!(s::UnfrozenLittleSet, key)
    index = lookup(s, key)
    deleteat!(s.data, index)
    key
end

Base.empty!(s::UnfrozenLittleSet) = (empty!(s.data); s)

function Base.delete!(dd::UnfrozenLittleSet, key)
    pop!(dd, key)
    return dd
end

