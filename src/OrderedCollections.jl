__precompile__()

module OrderedCollections

    import Base: <, <=, ==, convert, length, isempty, iterate, delete!,
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
                 union, intersect, symdiff, setdiff, setdiff!, issubset,
                 searchsortedfirst, searchsortedlast, in,
                 filter, filter!, ValueIterator, eachindex, keytype,
                 valtype, lastindex, nextind


    export OrderedDict, OrderedSet

    include("dict_support.jl")
    include("ordered_dict.jl")
    include("ordered_set.jl")

end
