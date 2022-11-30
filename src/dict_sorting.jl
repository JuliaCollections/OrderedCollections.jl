# Sort for dicts
import Base: sort, sort!

function Base.sort!(d::OrderedDict; byvalue::Bool=false, kwargs...)
    ks = keys(d)
    vs = _values(d)
    perm = byvalue ? sortperm(vs; kwargs...) : sortperm(ks; kwargs...)
    _apply_sortperm!(ks, perm)
    @inbounds vs[:] = vs[perm]
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

