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
                 isless,
                 union, intersect, symdiff, setdiff, setdiff!, issubset,
                 searchsortedfirst, searchsortedlast, in,
                 filter, filter!, ValueIterator, eachindex, keytype,
                 valtype, lastindex, nextind,
                 copymutable, emptymutable, dict_with_eltype

    export OrderedDict, OrderedSet, LittleDict
    export freeze

    include("dict_support.jl")
    include("ordered_dict.jl")
    include("little_dict.jl")
    include("ordered_set.jl")
    include("dict_sorting.jl")

end
