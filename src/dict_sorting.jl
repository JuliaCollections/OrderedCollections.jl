# Sort for dicts
import Base: sort, sort!, issorted

function sort!(d::OrderedDict; byvalue::Bool=false, args...)
    if d.ndel > 0
        rehash!(d)
    end

    n = d.len
    keys = d.keys
    vals = d.vals
    # Only the live prefix 1:n is meaningful; the Memory tail is undef.
    data = view(byvalue ? vals : keys, 1:n)

    # Filter out the kwargs supported by issorted (notably, :alg needs to be removed)
    issorted_kw = NamedTuple(k => v for (k, v) in args if k in (:lt, :by, :rev, :order))
    issorted(data; issorted_kw...) && return d

    p = sortperm(data; args...)
    newkeys = similar(keys, n)
    newvals = similar(vals, n)
    @inbounds for i in 1:n
        newkeys[i] = keys[p[i]]
        newvals[i] = vals[p[i]]
    end
    @inbounds copyto!(keys, 1, newkeys, 1, n)
    @inbounds copyto!(vals, 1, newvals, 1, n)
    rehash!(d)
    return d
end

# Compared to just sorting the underlying OrderedDict, this method calls sort!
# directly on the keys (no need to sort d.vals::Memory{Nothing}). This saves
# the allocation of the permutation vector in sortperm.
function sort!(s::OrderedSet; kwargs...)
    d = s.dict
    d.ndel > 0 && rehash!(d)
    sort!(view(d.keys, 1:d.len); kwargs...)
    rehash!(d)
    return s
end

function sort!(d::UnfrozenLittleDict; byvalue::Bool=false, args...)
    if byvalue
        p = sortperm(d.vals; args...)
    else
        p = sortperm(d.keys; args...)
    end
    permute!(d.keys, p)
    permute!(d.vals, p)
    return d
end

sort(d::Union{OrderedDict,OrderedSet}; args...) = sort!(copy(d); args...)

@deprecate sort(d::Dict; args...) sort!(OrderedDict(d); args...)

function sort(d::LittleDict; byvalue::Bool=false, args...)
    if byvalue
        p = sortperm(d.vals; args...)
    else
        p = sortperm(d.keys; args...)
    end
    return LittleDict(d.keys[p], d.vals[p])
end

function issorted(d::LittleDict; byvalue::Bool=false, args...)
    if byvalue
        return issorted(d.vals; args...)
    else
        return issorted(d.keys; args...)
    end
end
