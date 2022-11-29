# Sort for dicts
import Base: sort, sort!

Base.sortperm(s::OrderedSet; kwargs...) = sortperm(_values(s); kwargs...)




function Base.sort!(d::OrderedDict; byvalue::Bool=false, kwargs...)
    ks = keys(d)
    vs = _values(d)
    perm = byvalue ? sortperm(vs; kwargs...) : sortperm(ks; kwargs...)
    @inbounds vs[perm] = vs[perm]
    ksvals = _values(ks)
    @inbounds ksvals[perm] = ksvals[perm]
    slots = _slots(ks)
    mask = length(slots) - 1
    _rehash!(ksvals, _slots(ks), length(ksvals), mask, mask, _settings(ks).max_probe)
    return d
end

sort(d::OrderedDict; args...) = sort!(copy(d); args...)
@deprecate sort(d::Dict; args...) sort!(OrderedDict(d); args...)

function sort(d::LittleDict; byvalue::Bool=false, args...)
    if byvalue
        p = sortperm(d.vals; args...)
    else
        p = sortperm(d.keys; args...)
    end
    return LittleDict(d.keys[p], d.vals[p])
end

