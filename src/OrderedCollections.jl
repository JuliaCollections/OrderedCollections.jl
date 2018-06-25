__precompile__()

module OrderedCollections

    import Base: <, <=, ==, length, isempty, start, next, done, delete!,
                 show, dump, empty!, getindex, setindex!, get, get!,
                 in, haskey, keys, merge, copy, cat,
                 push!, pop!, insert!,
                 union!, delete!, similar, sizehint!,
                 isequal, hash,
                 map, reverse,
                 first, last, eltype, getkey, values, sum,
                 merge, merge!, lt, Ordering, ForwardOrdering, Forward,
                 ReverseOrdering, Reverse, Lt,
                 isless,
                 union, intersect, symdiff, setdiff, issubset,
                 searchsortedfirst, searchsortedlast, in

    export OrderedDict, OrderedSet

    import Base: eachindex, keytype, valtype

    include("dict_support.jl")
    include("ordered_dict.jl")
    include("ordered_set.jl")

    # include("dict_sorting.jl")

end
