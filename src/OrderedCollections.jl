module OrderedCollections

import Base: <, <=, ==, convert, length, isempty, iterate, delete!,
             show, dump, empty!, getindex, setindex!, get, get!,
             in, haskey, keys, merge, copy, cat,
             push!, pop!, popfirst!, insert!,
             union!, delete!, empty, sizehint!,
             isequal, hash,
             map, map!, reverse,
             first, last, eltype, getkey, values, sum,
             merge, merge!, lt, Ordering, ForwardOrdering, Forward,
             ReverseOrdering, Reverse, Lt,
             union, intersect, symdiff, setdiff, setdiff!, issubset,
             searchsortedfirst, searchsortedlast, in,
             filter, filter!, ValueIterator, eachindex, keytype,
             valtype, lastindex, nextind,
             copymutable, emptymutable, dict_with_eltype

export OrderedDict, OrderedSet, LittleDict
export freeze

const EMPTY_SLOT = 0x00000000
const MAX_VALUES = typemax(UInt32) >> 1
const INT_SIZE = sizeof(Int) * 8
const StoreType = Union{<:Tuple, <:Vector}

include("HashSettings.jl")
include("dict_support.jl")
include("LittleSet.jl")
include("LittleDict.jl")
include("OrderedSet.jl")
include("OrderedDict.jl")
include("utils.jl")

"""
    isordered(::Type)

Property of associative containers, that is `true` if the container type has a
defined order (such as `OrderedDict` and `SortedDict`), and `false` otherwise.
"""
isordered(::Type{T}) where {T<:AbstractDict} = false
isordered(@nospecialize T::Type{<:OrderedDict}) = true
isordered(@nospecialize T::Type{<:OrderedSet}) = true
isordered(@nospecialize T::Type{<:LittleDict}) = true



end
