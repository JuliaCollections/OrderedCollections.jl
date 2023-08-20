
struct LittleSet{T, D<:StoreType{T}} <: AbstractSet{T}
    data::D

    global const UnfrozenLittleSet{T} = LittleSet{T, <:AbstractVector{T}}
    global const FrozenLittleSet{T} = LittleSet{T, <:Tuple}
    global const OpaqueLittleSet{T} = LittleSet{T, Tuple{Vararg{T}}}

    LittleSet{T, D}(data) where {T,D} = new{T, D}(data)
    function OpaqueLittleSet{T}(@nospecialize(data)) where {T}
        if isa(data, Tuple{Vararg{T}})
            new{T, Tuple{Vararg{T}}}(data)
        else
            new{T, Tuple{Vararg{T}}}(Tuple(data))
        end
    end
    function OpaqueLittleSet(@nospecialize(data))
        T = eltype(data)
        new{T, Tuple{Vararg{T}}}(data)
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

# in cases where tuple parameters have been intentionally made opaque (such as
# `FrozenLittleSet{T, Tuple{Vararg{T}}}`), these methods allow accessing data without exposing the
# underlying `data` field and unintentionally specializing downstream code on the exact type
# representation of a tuple.
@static if isdefined(Base, Symbol("@assume_effects"))
    Base.@assume_effects :nothrow function unsafe_getstate(x::FrozenLittleSet, state::Int)
        getfield(getfield(x, :data), state)
    end
else
    unsafe_getstate(x::FrozenLittleSet, state::Int) = @inbounds(getfield(getfield(x, :data), state))
end
unsafe_getstate(x::UnfrozenLittleSet, state::Int) = @inbounds(getfield(x, :data)[state])

# `data` should not be directly accessed
Base.propertynames(@nospecialize(x::LittleSet)) = ()
function Base.propertynames(@nospecialize(x::LittleSet), ::Symbol)
    throw(ArgumentError("LittleSet does not support public access to it's fields."))
end

Base.Tuple(s::FrozenLittleSet) = getfield(s, :data)
Base.Tuple(s::UnfrozenLittleDict) = Tuple(getfield(s, :data))

freeze(s::AbstractSet{T}) where {T} = LittleSet{T}(Tuple(s))

hash(s::LittleSet, h::UInt) = hash(getfield(s, :data), hash(orderedset_seed, h))

Base.length(s::UnfrozenLittleSet) = length(getfield(s, :data))
Base.length(@nospecialize(s::FrozenLittleSet)) = nfields(getfield(s, :data))

#region empty
Base.isempty(s::UnfrozenLittleSet) = isempty(getfield(s, :data))
Base.isempty(@nospecialize(s::FrozenLittleSet)) = nfields(getfield(s, :data)) === 0

function Base.empty(@nospecialize(s::FrozenLittleSet))
    if isa(s, OpaqueLittleSet)
        return OpaqueLittleSet{eltype(s)}(())
    else
        return LittleSet{eltype(s), Tuple{}}(())
    end
end
Base.empty(s::UnfrozenLittleSet{T}) where {T} = LittleSet{T}(empty(getfield(s, :data)))

function Base.emptymutable(s::UnfrozenLittleSet{T}, ::Type{U}=T) where {T, U}
    LittleSet(Base.emptymutable(getfield(s, :data), U))
end
function Base.emptymutable(@nospecialize(s::FrozenLittleSet), ::Type{U}=eltype(s)) where {U}
    LittleSet(U[])
end
#endregion empty

#region copy
# since `Base.copy` is a shallow copy on collections, an immutable collection like `Tuple` the same
Base.copy(@nospecialize(s::FrozenLittleSet)) = s
Base.copy(s::UnfrozenLittleSet{T}) where {T} = LittleSet{T}(copy(getfield(s, :data)))

Base.copymutable(@nospecialize(s::FrozenLittleSet)) = LittleSet(eltype(s)[])
function Base.copymutable(s::UnfrozenLittleSet{T}) where {T}
    LittleSet{T}(Base.copymutable(getfield(s, :data)))
end
#endregion copy

function Base.sizehint!(s::UnfrozenLittleSet, sz)
    sizehint!(getfield(s, :data), sz)
    return s
end
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

check_count(::Nothing) = nothing
function check_count(count::Integer)
    count < 0 && throw(DomainError(count, "`count` must not be negative (got $count)"))
    return min(count, typemax(Int)) % Int
end

function replace_state(f, s::FrozenLittleSet, state::Int, cnt::Int)
    if cnt > 0
        old_item = unsafe_getstate(s, state)
        new_item = f(old_item)
        if old_item == new_item
            return new_item, cnt
        else
            return new_item, cnt - 1
        end
    else
        unsafe_getstate(s, state), 0
    end
end
function replace_state(f, s::FrozenLittleSet, state::Int, ::Nothing)
    (f(unsafe_getstate(s, state)), nothing)
end

struct ReplacePairs{N, F, S} <: Function
    old_new::NTuple{N, Pair{F, S}}
end
_secondtype(::ReplacePairs{<:Any, <:Any, S}) where {S} = S

@inline function (rp::ReplacePairs)(item)
    for (old_item, new_item) in getfield(rp, :old_new)
        isequal(old_item, item) && return new_item
    end
    return item
end

function Base.replace(s::FrozenLittleSet, old_new::Pair...; count::Union{Integer, Nothing}=nothing)
    replace(ReplacePairs(old_new), s; count=count)
end
function Base.replace(f::Union{Function, Type}, s::FrozenLittleSet{T}; count::Union{Integer, Nothing}=nothing) where {T}
    data = getfield(s, :data)
    N = nfields(data)
    if N === 0
        newdata = ()
    elseif N === 1
        i1, cnt = replace_state(f, s, 1, check_count(count))
        newdata = (i1,)
    elseif N === 2
        i1, cnt = replace_state(f, s, 1, check_count(count))
        i2, cnt = replace_state(f, s, 2, cnt)
        newdata = (i1, i2)
    elseif N === 3
        i1, cnt = replace_state(f, s, 1, check_count(count))
        i2, cnt = replace_state(f, s, 2, cnt)
        i3, cnt = replace_state(f, s, 3, cnt)
        newdata = (i1, i2, i3)
    elseif N === 4
        i1, cnt = replace_state(f, s, 1, check_count(count))
        i2, cnt = replace_state(f, s, 2, cnt)
        i3, cnt = replace_state(f, s, 3, cnt)
        i4, cnt = replace_state(f, s, 4, cnt)
        newdata = (i1, i2, i3, i4)
    elseif N === 5
        i1, cnt = replace_state(f, s, 1, check_count(count))
        i2, cnt = replace_state(f, s, 2, cnt)
        i3, cnt = replace_state(f, s, 3, cnt)
        i4, cnt = replace_state(f, s, 4, cnt)
        i5, cnt = replace_state(f, s, 5, cnt)
        newdata = (i1, i2, i3, i4, i5)
    elseif N === 6
        i1, cnt = replace_state(f, s, 1, check_count(count))
        i2, cnt = replace_state(f, s, 2, cnt)
        i3, cnt = replace_state(f, s, 3, cnt)
        i4, cnt = replace_state(f, s, 4, cnt)
        i5, cnt = replace_state(f, s, 5, cnt)
        i6, cnt = replace_state(f, s, 6, cnt)
        newdata = (i1, i2, i3, i4, i5, i6)
    elseif N === 7
        i1, cnt = replace_state(f, s, 1, check_count(count))
        i2, cnt = replace_state(f, s, 2, cnt)
        i3, cnt = replace_state(f, s, 3, cnt)
        i4, cnt = replace_state(f, s, 4, cnt)
        i5, cnt = replace_state(f, s, 5, cnt)
        i6, cnt = replace_state(f, s, 6, cnt)
        i7, cnt = replace_state(f, s, 7, cnt)
        newdata = (i1, i2, i3, i4, i5, i6, i7)
    elseif N === 8
        i1, cnt = replace_state(f, s, 1, check_count(count))
        i2, cnt = replace_state(f, s, 2, cnt)
        i3, cnt = replace_state(f, s, 3, cnt)
        i4, cnt = replace_state(f, s, 4, cnt)
        i5, cnt = replace_state(f, s, 5, cnt)
        i6, cnt = replace_state(f, s, 6, cnt)
        i7, cnt = replace_state(f, s, 7, cnt)
        i8, cnt = replace_state(f, s, 8, cnt)
        newdata = (i1, i2, i3, i4, i5, i6, i7, i8)
    else
        newdata = Tuple(replace(f, collect(data); count=count))
    end
    if isa(s, LittleSet{T, Tuple{Vararg{T}}})
        Tnew = isa(f, ReplacePairs) ? Union{T, _secondtype(f)} : eltype(newdata)
        return LittleSet{Tnew, Tuple{Vararg{Tnew}}}(newdata)
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
    if isa(s, FrozenLittleSet)
        N = length(s)
        if N > 32
            newdata = Tuple(filter(f, collect(getfield(s, :data))))
        else
        end
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

