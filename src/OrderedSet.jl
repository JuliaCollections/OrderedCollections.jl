
### OrderedSet
struct OrderedSet{T,L,S} <: AbstractSet{T}
    values::Vector{T}
    slots::Vector{UInt32}
    settings::HashSettings{L,S}

    function OrderedSet{T}(hs::HashSettings{L,S}=HashSettings()) where {T,L,S}
        new{T,L,S}(Vector{T}(), zeros(UInt32, 16), hs)
    end
    OrderedSet{T}(xs) where {T} = union!(OrderedSet{T}(), xs)
    function OrderedSet{T}(s::OrderedSet{T,L,S}) where {T,L,S}
        new{T,L,S}(copy(_values(s)), copy(_slots(s)), copy(_settings(s)))
    end
    OrderedSet() = OrderedSet{Any}()
    OrderedSet(xs) = OrderedSet{eltype(xs)}(xs)
end

Base.collect(s::OrderedSet) = copy(_values(s))

_slots(s::OrderedSet) = getfield(s, :slots)
_values(s::OrderedSet) = getfield(s, :values)
_settings(s::OrderedSet) = getfield(s, :settings)

_max_probe(s::OrderedSet) = getfield(_settings(s), :max_probe)
_max_probe!(s::OrderedSet, mp::UInt8) = _max_probe!(_settings(s), mp)

_age(s::OrderedSet) = _age(_settings(s))
increment_age!(s::OrderedSet) = increment_age!(_settings(s))

function Base.propertynames(s::OrderedSet, private::Bool=false)
    private ? (:values, :slots, :settings) : ()
end

Base.firstindex(::OrderedSet) = 1
Base.lastindex(s::OrderedSet) = length(s)
Base.eachindex(s::OrderedSet) = Base.OneTo(length(s))

function Base.getindex(s::OrderedSet, i::Int)
    @boundscheck (1 <= i && i <= length(s)) || throw(BoundsError(s, i))
    unsafe_get(_values(s), i)
end

Base.sizehint!(s::OrderedSet, newsz) = sizehint!(s, Int(newsz))
function Base.sizehint!(s::OrderedSet, newsz::Int)
    if newsz !== length(s)
        age = _age(s)
        _sizehint!(s, newsz)
        @assert _age(s) === age "Multiple concurrent writes to OrderedSet detected!"
    end
    return s
end
function _sizehint!(s::OrderedSet, sz::Int)
    values = _values(s)
    nvalues = length(values)
    sizehint!(values, sz)
    hs = _settings(s)
    slots = _slots(s)
    nslots = length(slots)
    if sz > nvalues
        _maybe_grow_rehash!(hs, values, slots, nslots, nvalues)
    else
        _maybe_shrink_rehash!(hs, values, slots, nslots, nvalues)
    end
    return nothing
end

Base.copy(s::OrderedSet) = OrderedSet(s)

Base.empty(::OrderedSet{T}) where {T} = OrderedSet{T}()

Base.length(s::OrderedSet) = length(_values(s))

Base.isempty(s::OrderedSet) = length(s) === 0

Base.first(s::OrderedSet) = first(_values(s))
Base.last(s::OrderedSet) = last(_values(s))

function Base.iterate(s::OrderedSet)
    length(s) === 0 ? nothing : (unsafe_get(_values(s), 1), 2)
end
function Base.iterate(s::OrderedSet, i::Int)
    length(s) < i ? nothing : (unsafe_get(_values(s), i), i + 1)
end

Base.in(value, s::OrderedSet{T}) where {T} = in(try_convert(T, value), s)
Base.in(value::T, s::OrderedSet{T}) where {T} = find_index(s, value) !== -1

@assume_effects :terminates_locally function find_slot(s::OrderedSet, value)
    values = _values(s)
    slots = _slots(s)
    mask = length(slots) - 1
    mp = _max_probe(s)
    itr = 0x00
    i = (reinterpret(Int, hash(value)) & mask) + 1
    while true
        slot_i = unsafe_get(slots, i)
        slot_i !== EMPTY_SLOT && (unsafe_get(values, slot_i) == value) && return i
        itr === mp && return -1
        itr += 0x01
        i = (i & mask) + 1
    end
end
@assume_effects :terminates_locally function find_index(s::OrderedSet, value)
    values = _values(s)
    slots = _slots(s)
    mask = length(slots) - 1
    mp = _max_probe(s)
    itr = 0x00
    i = (reinterpret(Int, hash(value)) & mask) + 1
    while true
        slot_i = unsafe_get(slots, i)
        if slot_i !== EMPTY_SLOT
            index = Int(slot_i)
            unsafe_get(values, index) == value && return index
        end
        itr === mp && return -1
        itr += 0x01
        i = (i & mask) + 1
    end
end

function lookup(s::OrderedSet{T}, value) where {T}
    slots = _slots(s)
    mask = length(slots) - 1
    _lookup(_values(s), slots, _max_probe(s), mask, value, to_slot_index(value, mask))
end

function Base.pop!(s::OrderedSet)
    age = _age(s)
    values = _values(s)
    nvalues = length(values)
    nvalues > 0 || throw(ArgumentError("Cannot delete from empty set."))
    slots = _slots(s)
    nslots = length(slots)
    mask = nslots - 1
    key = unsafe_get(values, nvalues)
    unsafe_set!(slots, _find_slot(slots, key, nvalues, mask), EMPTY_SLOT)
    unsafe_delete_end!(values, 1)
    _maybe_shrink_rehash!(_settings(s), values, slots, nslots, nvalues-1)
    @assert _age(s) === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return key
end
function Base.popfirst!(s::OrderedSet)
    age = _age(s)
    values = _values(s)
    nvalues = length(values)
    nvalues > 0 || throw(ArgumentError("Cannot delete from empty set."))
    slots = _slots(s)
    nslots = length(slots)
    mask = nslots - 1
    key = unsafe_get(values, 1)
    unsafe_set!(slots, _find_slot(slots, key, 1, mask), EMPTY_SLOT)
    _add_slots!(values, slots, 1, nvalues, mask, -0x00000001)
    unsafe_delete_beg!(values, 1)
    _maybe_shrink_rehash!(_settings(s), values, slots, nslots, nvalues-1)
    @assert _age(s) === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return key
end
function Base.pop!(s::OrderedSet, key)
    age = _age(s)
    success, _ = try_delete!(s, key)
    success || throw(KeyError(key))
    @assert _age(s) === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return key
end
function Base.pop!(s::OrderedSet, key, default)
    age = _age(s)
    success, _ = try_delete!(s, key)
    @assert _age(s) === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    success ? key : default
end
function Base.delete!(s::OrderedSet, key)
    age = _age(s)
    try_delete!(s, key)
    @assert _age(s) === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return s
end

function Base.empty!(s::OrderedSet)
    age = _age(s)
    slots = _slots(s)
    for i in eachindex(slots)
        unsafe_set!(slots, i, EMPTY_SLOT)
    end
    empty!(_values(s))
    @assert _age(s) === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return s
end

function Base.filter!(f, s::OrderedSet)
    values = _values(s)
    nvalues = length(values)
    slots = _slots(s)
    nslots = length(slots)
    mask = nslots - 1
    kloc = 1
    age = _age(s)
    while kloc <= nvalues
        key = unsafe_get(values, kloc)
        #print(stdout, "index: $(kloc), length: $(nvalues), key: $(key)\n")
        if !return_bool(f(key))
            unsafe_set!(slots, _find_slot(slots, key, kloc, mask), EMPTY_SLOT)
            _add_slots!(values, slots, kloc, nvalues, mask, -0x00000001)
            unsafe_delete_at!(values, kloc, 1)
            nvalues -= 1
        else
            kloc += 1
        end
        #print(stdout, "index: $(kloc), length: $(nvalues), key: $(key)\n\n")
    end
    _maybe_shrink_rehash!(_settings(s), values, slots, nslots, nvalues)
    @assert _age(s) === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return s
end

function Base.push!(s::OrderedSet{T}, key) where {T}
    age = _age(s)
    try_push!(s, try_convert(T, key))
    @assert _age(s) === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return s
end

function try_push!(s::OrderedSet, key)
    values = _values(s)
    slots = _slots(s)
    hs = _settings(s)
    nvalues = length(values)
    nkplus = nvalues + 1
    nslots = length(slots)
    mask = nslots - 1
    flag, idx = try_insert_slot!(values, slots, hs, nvalues, nslots, mask, key, nkplus)
    #idx = try_insert_slot!(values, slots, hs, mask, key, nkplus)
    if flag === 0x02
        return false, idx
    else
        unsafe_grow_end!(values, 1)
        unsafe_set!(values, nkplus, key)
        flag === 0x01 && _maybe_grow_rehash!(hs, values, slots, nslots, nkplus)
        return true, nkplus
    end
end

function Base.insert!(s::OrderedSet{T}, i::Int, key) where {T}
    age = _age(s)
    try_insert!(s, i, try_convert(T, key))
    @assert _age(s) === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return s
end
function try_insert!(s::OrderedSet, i::Int, key)
    values = _values(s)
    slots = _slots(s)
    hs = _settings(s)
    nvalues = length(values)
    nslots = length(slots)
    mask = nslots - 1
    flag, idx = try_insert_slot!(values, slots, hs, nvalues, nslots, mask, key, i)
    if idx === 0
        unsafe_grow_at!(values, i, 1)
        unsafe_set!(values, i, key)
        _maybe_grow_rehash!(hs, values, slots, nslots, nvalues + 1)
        return true, index
    else
        return false
    end
end

function Base.pushfirst!(s::OrderedSet{T}, key) where {T}
    age = _age(s)
    try_pushfirst!(s, try_convert(T, key))
    @assert _age(s) === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return s
end
function try_pushfirst!(s::OrderedSet, key)
    values = _values(s)
    slots = _slots(s)
    hs = _settings(s)
    nvalues = length(values)
    nslots = length(slots)
    mask = nslots - 1
    idx = try_insert_slot!(values, slots, hs, mask, key, 1)
    if idx === 0
        unsafe_grow_beg!(values, 1)
        unsafe_set!(values, 1, key)
        _maybe_grow_rehash!(hs, values, slots, nslots, nvalues + 1)
        return true
    else
        return false
    end
end
function Base.union!(s::OrderedSet{T}, itr) where {T}
    age = _age(s)
    Base.haslength(itr) && _sizehint!(s, length(s) + return_int(length(itr)))
    for x in itr
        try_push!(s, try_convert(T, x))
        length(s) === MAX_VALUES && break
    end
    @assert _age(s) === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return s
end

# TODO intersect!
# intersect!(s::AbstractSet, itr) =
#    intersect!(s, union!(emptymutable(s, eltype(itr)), itr))

function Base.setdiff!(s::OrderedSet, itr)
    age = _age(s)
    for x in itr
        try_delete!(s, x)
    end
    @assert _age(s) === age "Multiple concurrent writes to OrderedSet detected!"
    increment_age!(s)
    return s
end

function Base.hash(s::OrderedSet, h::UInt)
    hash(_settings(s), hash(_slots(s), hash(_values(s), h)))
end

function Base.showarg(io::IO, s::OrderedSet, toplevel::Bool)
    !toplevel && print(io, "::")
    print(io, "OrderedSet{$(eltype(s))}")
end

function Base.show(io::IO, s::OrderedSet)
    Base.showarg(io, s, true)
    print(io, "(")
    !isempty(s) && Base.show_vector(io, s,'[',']')
    print(io, ")")
end

to_slot_index(key, mask::Int) = (reinterpret(Int, hash(key)) & mask) + 1
# we know kloc exists in slots and need to replace it with an empty slot value
@assume_effects :terminates_locally function _find_slot(
    slots::Vector{UInt32}, val, slot::UInt32, mask::Int
)
    index = (reinterpret(Int, hash(val)) & mask) + 1
    while true
        slot_i = unsafe_get(slots, index)
        slot_i === slot && return index
        index = (index & mask) + 1
    end
end
function _find_slot(slots::Vector{UInt32}, key, slot, mask::Int)
    _find_slot(slots, key, convert(UInt32, slot), mask)
end

# `(true, index)` : successfully deleted from `index`
# `(false, index)` : `value` not found. `slots[index]` was last attempt at finding it.
function try_delete!(s::OrderedSet, val)
    values = _values(s)
    nvalues = length(values)
    slots = _slots(s)
    nslots = length(slots)
    mask = nslots - 1
    Base.GC.@preserve values begin
        flag, slot, index = _lookup(values, slots, _max_probe(s), mask, val, (reinterpret(Int, hash(val)) & mask) + 1)
        if flag === 0x02
            unsafe_set!(slots, index, EMPTY_SLOT)
            _add_slots!(values, slots, Int(slot), nvalues, mask, -0x00000001)
            unsafe_delete_at!(values, slot, 1)
            out = (true, Int(slot))
        else
            out = (false, Int(slot))
        end
    end
    return out
end

Base.sortperm(s::OrderedSet; kwargs...) = sortperm(_values(s); kwargs...)
function Base.sort!(s::OrderedSet; kwargs...)
    _apply_sortperm!(s, sortperm(s; kwargs...))
    return s
end
function _apply_sortperm!(s::OrderedSet, perm::Vector{Int})
    values = _values(s)
    slots = _slots(s)
    mp = _max_probe(s)
    mask = length(slots) - 1
    inds = eachindex(perm)
    for i in inds
        value_i = unsafe_get(values, unsafe_get(perm, i))
        _, _, index = _lookup(values, slots, mp, mask, value_i, to_slot_index(value_i, mask))
        unsafe_set!(slots, index, i)
    end
    @inbounds values[inds] = values[perm]
    nothing
end

Base.copymutable(s::OrderedSet) = copy(s)
function Base.emptymutable(s::OrderedSet{T,L,S}, ::Type{U}=T) where {T,U,L,S}
    OrderedSet{U}(HashSettings{L,S}())
end

