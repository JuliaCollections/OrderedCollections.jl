
struct LittleSet{T, D<:StoreType{T}} <: AbstractSet{T}
    data::D

    LittleSet{T, D}(data) where {T,D} = new{T, D}(data)
    function LittleSet{T}(data::AbstractVector) where {T}
        if eltype(data) == T
            return new{T, typeof(data)}(data)
        else
            d = convert(AbstractVector{T}, data)
            return new{T, typeof(d)}(d)
        end
    end
    LittleSet{T}(data::Tuple{Vararg{T}}) where {T} = new{T, typeof(data)}(data)
    LittleSet{T}(itr) where {T} = union!(LittleSet{T}(), itr)
    LittleSet{T}() where {T} = new{T, Vector{T}}(Vector{T}())
    LittleSet() = LittleSet{Any}()
    function LittleSet(itr)
        IET = Base.IteratorEltype(itr)
        if isa(IET, Base.HasEltype)
            LittleSet{eltype(itr)}(itr)
        else
            T = Base.@default_eltype(itr)
            if (Base.isconcretetype(T) || T === Union{})
                return LittleSet{T}(itr)
            else
                return Base.grow_to!(LittleSet{T}(), itr)
            end
        end
    end
end

# `data` should not be directly accessed
Base.propertynames(::LittleSet) = ()
function Base.propertynames(::LittleSet, ::Symbol)
    throw(ArgumentError("LittleSet does not support public access to it's fields."))
end

const FrozenLittleSet{T} = LittleSet{T, <:Tuple}
const UnfrozenLittleSet{T} = LittleSet{T, <:AbstractVector{T}}

Base.Tuple(s::FrozenLittleSet) = getfield(s, :data)
Base.Tuple(s::UnfrozenLittleDict) = Tuple(getfield(s, :data))

# find the index position of `key`
function find_key_index(key, s::LittleSet)
    data = getfield(s, :data)
    for i in eachindex(data)
        isequal(@inbounds(data[i]), key) && return i
    end
    return ERROR_INDEX
end

freeze(s::AbstractSet{T}) where {T} = LittleSet{T}(Tuple(s))

function hash(s::LittleSet, h::UInt)
    hash(getfield(s, :data), hash(orderedset_seed, h))
end

Base.length(s::LittleSet) = length(getfield(s, :data))

Base.isempty(s::LittleSet) = isempty(getfield(s, :data))

Base.empty(s::LittleSet{T}) where {T} = LittleSet{T}(empty(getfield(s, :data)))

Base.iterate(s::LittleSet, state=1) = iterate(getfield(s, :data), state)

Base.in(x, s::LittleSet) = in(x, getfield(s, :data))

# since `Base.copy` is a shallow copy on collections, an immutable collection like `Tuple` the same
Base.copy(s::FrozenLittleSet) = s
Base.copy(s::UnfrozenLittleSet{T}) where {T} = LittleSet{T}(copy(getfield(s, :data)))

function Base.sizehint!(s::UnfrozenLittleSet, sz)
    sizehint!(getfield(s, :data), sz)
    return s
end

Base.filter!(f, s::UnfrozenLittleSet) = filter!(f, getfield(s, :data))

function Base.push!(s::UnfrozenLittleSet, val)
    data = getfield(s, :data)
    if !in(val, data)
        push!(data, val)
    end
    return s
end

Base.pop!(s::UnfrozenLittleSet) = pop!(getfield(s, :data))
function Base.pop!(s::UnfrozenLittleSet, key)
    index = find_key_index(key, s)
    index === ERROR_INDEX && throw(KeyError(key))
    deleteat!(getfield(s, :data), index)
    key
end
function Base.pop!(s::UnfrozenLittleSet, key, default)
    index = find_key_index(key, s)
    if index === ERROR_INDEX
        return default
    else
        deleteat!(getfield(s, :data), index)
        return key
    end
end


Base.empty!(s::UnfrozenLittleSet) = (empty!(getfield(s, :data)); s)

function Base.delete!(s::UnfrozenLittleSet, key)
    index = find_key_index(key, s)
    if index !== ERROR_INDEX
        deleteat!(getfield(s, :data), index)
    end
    return s
end

function Base.sort(s::UnfrozenLittleSet{T}; ks...) where {T}
    LittleSet{T}(sort(getfield(s, :data); ks...))
end
# this is a temporary hack to get around the lack of `sort` available for tuples
function Base.sort(s::FrozenLittleSet{T}; ks...) where {T}
    LittleSet{T}(sort(T[getfield(s, :data)...]; ks...))
end

function Base.sort!(s::UnfrozenLittleSet; ks...)
    sort!(getfield(s, :data); ks...)
    return s
end


