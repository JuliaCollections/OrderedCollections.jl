
struct LittleSet{T, D<:StoreType{T}} <: AbstractSet{T}
    data::D

    LittleSet{T, D}(data) where {T,D} = new{T, D}(data)
    function LittleSet{T, Tuple{Vararg{T}}}(@nospecialize(data)) where {T}
        if isa(data, Tuple{Vararg{T}})
            new{T, Tuple{Vararg{T}}}(data)
        else
            new{T, Tuple{Vararg{T}}}(Tuple(data))
        end
    end
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
Base.propertynames(@nospecialize(x::LittleSet)) = ()
function Base.propertynames(@nospecialize(x::LittleSet), ::Symbol)
    throw(ArgumentError("LittleSet does not support public access to it's fields."))
end

const FrozenLittleSet{T} = LittleSet{T, <:Tuple}
const UnfrozenLittleSet{T} = LittleSet{T, <:AbstractVector{T}}

Base.Tuple(s::FrozenLittleSet) = getfield(s, :data)
Base.Tuple(s::UnfrozenLittleDict) = Tuple(getfield(s, :data))

freeze(s::AbstractSet{T}) where {T} = LittleSet{T}(Tuple(s))

hash(s::LittleSet, h::UInt) = hash(getfield(s, :data), hash(orderedset_seed, h))

Base.length(s::LittleSet) = length(getfield(s, :data))

Base.isempty(s::UnfrozenLittleSet) = isempty(getfield(s, :data))

function Base.empty(@nospecialize(s::FrozenLittleSet))
    T = eltype(s)
    if isa(s, LittleSet{T, Tuple{Vararg{T}}})
        return LittleSet{T, Tuple{Vararg{T}}}(())
    else
        return LittleSet{T, Tuple{}}(())
    end
end
Base.empty(s::UnfrozenLittleSet{T}) where {T} = LittleSet{T}(empty(getfield(s, :data)))

Base.iterate(s::LittleSet, state...) = iterate(getfield(s, :data), state...)

Base.in(x, s::UnfrozenLittleSet) = in(x, getfield(s, :data))
function Base.in(x, s::FrozenLittleSet)
    data = getfield(s, :data)
    n = nfields(data)
    while n > 0
        isequal(x, getfield(data, n)) && return true
        n -= 1
    end
    return false
end

# since `Base.copy` is a shallow copy on collections, an immutable collection like `Tuple` the same
Base.copy(@nospecialize(s::FrozenLittleSet)) = s
Base.copy(s::UnfrozenLittleSet{T}) where {T} = LittleSet{T}(copy(getfield(s, :data)))

function Base.sizehint!(s::UnfrozenLittleSet, sz)
    sizehint!(getfield(s, :data), sz)
    return s
end

function Base.emptymutable(s::UnfrozenLittleSet{T}, ::Type{U}=T) where {T, U}
    LittleSet(Base.emptymutable(getfield(s, :data), U))
end
function Base.emptymutable(@nospecialize(s::FrozenLittleSet), ::Type{U}=eltype(s)) where {U}
    LittleSet(U[])
end

Base.copymutable(@nospecialize(s::FrozenLittleSet)) = LittleSet(eltype(s)[])
function Base.copymutable(s::UnfrozenLittleSet{T}) where {T}
    LittleSet{T}(Base.copymutable(getfield(s, :data)))
end

function Base.sort(s::UnfrozenLittleSet{T}; ks...) where {T}
    LittleSet{T}(sort(getfield(s, :data); ks...))
end
# HACK: this is a temporary hack to get around the lack of `sort` available for tuples
function Base.sort(s::FrozenLittleSet{T}; ks...) where {T}
    LittleSet{T}(sort(T[getfield(s, :data)...]; ks...))
end
function Base.sort!(s::UnfrozenLittleSet; ks...)
    sort!(getfield(s, :data); ks...)
    return s
end

function Base.replace(f::Union{Function, Type}, s::LittleSet{T}; count::Integer=typemax(Int)) where {T}
    newdata = replace(f, getfield(s, :data); count=count)
    if isa(s, LittleSet{T, Tuple{Vararg{T}}})
        T2 = eltype(newdata)
        return LittleSet{T2, Tuple{Vararg{T2}}}(newdata)
    else
        return LittleSet(newdata)
    end
end
function Base.replace(s::LittleSet{T}, old_new::Pair{F, S}...; count::Integer=typemax(Int)) where {T, F, S}
    newdata = replace(getfield(s, :data), old_new...; count=count)
    if isa(s, LittleSet{T, Tuple{Vararg{T}}})
        T2 = Union{T, S}
        return LittleSet{T2, Tuple{Vararg{T2}}}(newdata)
    else
        return LittleSet(newdata)
    end
end

Base.first(s::UnfrozenLittleSet, n::Integer) = LittleSet(first(getfield(s, :data), n))
function Base.first(s::FrozenLittleSet{T}, n::Integer) where {T}
    N = Int(n)
    N < 0 && throw(ArgumentError("Number of elements must be nonnegative"))
    data = getfield(s, :data)
    stop = nfields(data)
    if stop <= N
        # max number of elements is everything so it's equivalent to `copy`
        return s
    elseif isa(s, LittleSet{T, Tuple{Vararg{T}}})
        return LittleSet{T, Tuple{Vararg{T}}}(Tuple(@inbounds(collect(data)[1:N])))
    else
        return LittleSet{T}(ntuple(i->getfield(data, i), min(nfields(data), n)))
    end
end

Base.last(s::UnfrozenLittleSet, n::Integer) = LittleSet(last(getfield(s, :data), n))
function Base.last(s::FrozenLittleSet{T}, n::Integer) where {T}
    N = Int(n)
    N < 0 && throw(ArgumentError("Number of elements must be nonnegative"))
    data = getfield(s, :data)
    stop = nfields(data)
    offset = stop - N
    if offset < 1
        # offset less than one means each element is returned so it's equivalent to `copy`
        return s
    elseif isa(s, LittleSet{T, Tuple{Vararg{T}}})
        return LittleSet{T, Tuple{Vararg{T}}}(Tuple(@inbounds(collect(data)[1:N])))
    else
        return LittleSet{T}(ntuple(i->getfield(data, offset + i), N))
    end
end

function Base.union(x::FrozenLittleSet{T1}, y::FrozenLittleSet{T2}) where {T1, T2}
    xdata = getfield(x, :data)
    nx = nfields(xdata)
    ydata = getfield(y, :data)
    ny = nfields(ydata)
    if isa(x, LittleSet{T1, Tuple{Vararg{T1}}}) || isa(y, LittleSet{T2, Tuple{Vararg{T2}}})
        if nx < ny # nx is smaller so search for x items in y
            newdata = (filter(!in(y), x)..., ydata...)
        else # ny is smaller so search for y items in x
            newdata = (xdata..., getfield(filter(!in(x), y), :data)...)
        end
        T = Union{T1, T2}
        return LittleSet{T, Tuple{Vararg{T}}}(newdata)
    else
        if nx < ny # nx is smaller so search for x items in y
            return LittleSet((filter(!in(ydata), xdata)..., ydata...))
        else # ny is smaller so search for y items in x
            return LittleSet((xdata..., filter(!in(xdata), ydata)...))
        end
    end
end

function Base.filter(f, s::LittleSet{T}) where {T}
    if isa(s, LittleSet{T, Tuple{Vararg{T}}})
        return LittleSet{T, Tuple{Vararg{T}}}(Tuple(filter(f, collect(getfield(s, :data)))))
    else
        return LittleSet(filter(f, getfield(s, :data)))
    end
end

Base.filter!(f, s::UnfrozenLittleSet) = (filter!(f, getfield(s, :data)); return s)
function Base.push!(s::UnfrozenLittleSet, val)
    data = getfield(s, :data)
    if !in(val, data)
        push!(data, val)
    end
    return s
end

Base.pop!(s::UnfrozenLittleSet) = pop!(getfield(s, :data))
function Base.pop!(s::UnfrozenLittleSet, key)
    data = getfield(s, :data)
    index = findfirst(isequal(key), data)
    index === nothing && throw(KeyError(key))
    deleteat!(data, index)
    key
end
function Base.pop!(s::UnfrozenLittleSet, key, default)
    data = getfield(s, :data)
    index = findfirst(isequal(key), data)
    if index === nothing
        return default
    else
        deleteat!(data, index)
        return key
    end
end

Base.empty!(s::UnfrozenLittleSet) = (empty!(getfield(s, :data)); s)

function Base.delete!(s::UnfrozenLittleSet, key)
    data = getfield(s, :data)
    index = findfirst(isequal(key), data)
    if index !== nothing
        deleteat!(getfield(s, :data), index)
    end
    return s
end

