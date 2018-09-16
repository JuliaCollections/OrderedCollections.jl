# ordered sets

# This was largely copied and modified from Base

# TODO: Most of these functions should be removed once AbstractSet is introduced there
# (see https://github.com/JuliaLang/julia/issues/5533)

struct OrderedSet{T}
    dict::OrderedDict{T,Nothing}

    OrderedSet{T}() where {T} = new{T}(OrderedDict{T,Nothing}())
    OrderedSet{T}(xs) where {T} = union!(new{T}(OrderedDict{T,Nothing}()), xs)
end
OrderedSet() = OrderedSet{Any}()
OrderedSet(xs) = OrderedSet{eltype(xs)}(xs)


show(io::IO, s::OrderedSet) = (show(io, typeof(s)); print(io, "("); !isempty(s) && Base.show_comma_array(io, s,'[',']'); print(io, ")"))

isempty(s::OrderedSet) = isempty(s.dict)
length(s::OrderedSet)  = length(s.dict)

sizehint!(s::OrderedSet, sz::Integer) = (sizehint!(s.dict, sz); s)
eltype(s::OrderedSet{T}) where {T} = T

in(x, s::OrderedSet) = haskey(s.dict, x)

push!(s::OrderedSet, x) = (s.dict[x] = nothing; s)
pop!(s::OrderedSet, x) = (pop!(s.dict, x); x)
pop!(s::OrderedSet, x, deflt) = pop!(s.dict, x, deflt) == deflt ? deflt : x
delete!(s::OrderedSet, x) = (delete!(s.dict, x); s)

union!(s::OrderedSet, xs) = (for x in xs; push!(s,x); end; s)
setdiff!(s::OrderedSet, xs) = (for x in xs; delete!(s,x); end; s)
setdiff!(s::Set, xs::OrderedSet) = (for x in xs; delete!(s,x); end; s)

empty(s::OrderedSet{T}) where {T} = OrderedSet{T}()
copy(s::OrderedSet) = union!(empty(s), s)

empty!(s::OrderedSet{T}) where {T} = (empty!(s.dict); s)

# NOTE: manually optimized to take advantage of OrderedDict representation
function iterate(s::OrderedSet)
    s.dict.ndel > 0 && rehash!(s.dict)
    length(s.dict.keys) < 1 && return nothing
    return (s.dict.keys[1], 2)
end
function iterate(s::OrderedSet, i)
    length(s.dict.keys) < i && return nothing
    return (s.dict.keys[i], i+1)
end

pop!(s::OrderedSet) = pop!(s.dict)[1]

union(s::OrderedSet) = copy(s)
function union(s::OrderedSet, sets...)
    u = OrderedSet{Base.promote_eltype(s, sets...)}()
    union!(u,s)
    for t in sets
        union!(u,t)
    end
    return u
end

intersect(s::OrderedSet) = copy(s)
function intersect(s::OrderedSet, sets...)
    i = copy(s)
    for x in s
        for t in sets
            if !in(x,t)
                delete!(i,x)
                break
            end
        end
    end
    return i
end

function setdiff(a::OrderedSet, b)
    d = empty(a)
    for x in a
        if !(x in b)
            push!(d, x)
        end
    end
    d
end

==(l::OrderedSet, r::OrderedSet) = (length(l) == length(r)) && (l <= r)
<(l::OrderedSet, r::OrderedSet) = (length(l) < length(r)) && (l <= r)
<=(l::OrderedSet, r::OrderedSet) = issubset(l, r)

function filter!(f::Function, s::OrderedSet)
    for x in s
        if !f(x)
            delete!(s, x)
        end
    end
    return s
end
filter(f::Function, s::OrderedSet) = filter!(f, copy(s))

const orderedset_seed = UInt === UInt64 ? 0x2114638a942a91a5 : 0xd86bdbf1
function hash(s::OrderedSet, h::UInt)
    h = hash(orderedset_seed, h)
    s.dict.ndel > 0 && rehash!(s.dict)
    hash(s.dict.keys, h)
end


# Deprecated functionality, see
# https://github.com/JuliaCollections/DataStructures.jl/pull/180#issuecomment-400269803

function getindex(s::OrderedSet, i::Int)
    Base.depwarn("indexing is deprecated for OrderedSet, please rewrite your code to use iteration", :getindex)
    s.dict.ndel > 0 && rehash!(s.dict)
    return s.dict.keys[i]
end

function lastindex(s::OrderedSet)
    Base.depwarn("indexing is deprecated for OrderedSet, please rewrite your code to use iteration", :lastindex)
    s.dict.ndel > 0 && rehash!(s.dict)
    return lastindex(s.dict.keys)
end

function nextind(::OrderedSet, i::Int)
    Base.depwarn("indexing is deprecated for OrderedSet, please rewrite your code to use iteration", :lastindex)
    return i + 1  # Needed on 0.7 to mimic array indexing.
end

function keys(s::OrderedSet)
    Base.depwarn("indexing is deprecated for OrderedSet, please rewrite your code to use iteration", :lastindex)
    s.dict.ndel > 0 && rehash!(s.dict)
    return 1:length(s)
end
