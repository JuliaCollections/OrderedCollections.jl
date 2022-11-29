
const EMPTY_SLOT = 0x00000000
const MAX_VALUES = typemax(UInt32) >> 1
const INT_SIZE = sizeof(Int) * 8

# flags::UInt8
#
# `0x00` : exceeded max probe before finding value or candidate empty slot
# `0x01` : found empty slot but not value
# `0x02` : found slot corresponding to values index
# `0x03` : found empty slot after increasing max probe
# `0x04` : found empty slot after increasing number of slots

function unsafe_delete_end!(x::Vector, delta)
    ccall(:jl_array_del_end, Cvoid, (Any, UInt), x, delta)
end
function unsafe_delete_beg!(x::Vector, delta)
    ccall(:jl_array_del_beg, Cvoid, (Any, UInt), x, delta)
end
function unsafe_delete_at!(x::Vector, i, delta)
    ccall(:jl_array_del_at, Cvoid, (Any, Int, UInt), x, i-1, delta)
end

function unsafe_grow_beg!(x::Vector, delta)
    ccall(:jl_array_grow_end, Cvoid, (Any, UInt), x, delta)
end
function unsafe_grow_end!(x::Vector, delta)
    ccall(:jl_array_grow_end, Cvoid, (Any, UInt), x, delta)
end
function unsafe_grow_at!(x::Vector, i::Int, delta)
    ccall(:jl_array_grow_at, Cvoid, (Any, Int, UInt), x, i-1, delta)
end


# helps avoid invalidations
return_bool(x::Bool) = x
return_uint8(x::UInt8) = x
return_int(x::Int) = x

_get(x::Vector, i::Base.BitInteger) = _get(x, Int(i))
_get(x::Vector, i::Int) = Core.arrayref(false, x, i)
unsafe_get(x::Vector, i::UInt32) = Core.arrayref(false, x, Int(i))
unsafe_get(x::Vector, i::Int) = Core.arrayref(false, x, i)

# This copies the effects from `Base.infer_effects(Core.arrayref)` and adds `nothrow` b/c
# we only ever use it after bounds checking on defined indices (isassigned(x, i) == true)
_set!(x::Vector{T}, i::Base.BitInteger, v) where {T} = _set!(x, Int(i), convert(T, v))
function _set!(x::Vector{T}, i::Int, v::T) where {T}
    Core.arrayset(false, x, v, i)
end
unsafe_set!(x::Vector{T}, i::Int, v) where {T} = unsafe_set!(x, i, convert(T, v))
unsafe_set!(x::Vector{T}, i::Int, v::T) where {T} = Core.arrayset(false, x, v, i)

try_convert(::Type{T}, x::T) where {T} = x
function try_convert(::Type{T}, x0) where {T}
    x = convert(T, x0)
    return_bool(isequal(x, x0)) || throw(ArgumentError("$(x0) is not a valid key for type $T"))
    return x
end

# `(0x00, slot_i, index)` : did not find `value`. `index` was last attempt before exceeding max probe
# `(0x01, slot_i, index)` : did not find `value`. `index` is an empty slot where new index for `value` can be set
# `(0x02, slot_i, index)` : found `values[slots[index]] == value`
function _lookup(
    values::Vector{T}, slots::Vector{UInt32}, max_probe::UInt8, mask::Int, value, i0::Int
) where {T}
    itr = 0x00
    empty_index = 0
    i = i0
    flag = 0x00
    while true
        slot_i = unsafe_get(slots, i)
        if slot_i === EMPTY_SLOT
            if flag === 0x00
                empty_index = i
                flag = 0x01
            end
        else
            unsafe_get(values, Int(slot_i)) == value && return (0x02, slot_i, i)
        end
        itr === max_probe && return (flag, EMPTY_SLOT, flag === 0x01 ? empty_index : i)
        itr += 0x01
        i = next_slot_index(i, mask)
    end
end

function _lookup_shift_max_probe(
    slots::Vector{UInt32}, old_max_probe::UInt8, max_allowed::UInt8, mask::Int, i::Int,
)
    itr = old_max_probe
    while true
        slot_i = unsafe_get(slots, i)
        slot_i === EMPTY_SLOT && return (itr, i)
        itr === max_allowed && return (max_allowed, i)
        itr += 0x01
        i = next_slot_index(i, mask)
    end
end

# flags
#   `0x01` : `index` is 
# `(0x01, index)` : did not find `value`. `index` is an empty slot where new index for `value` can be set
# `(0x02, index)` : found `values[index] == value`
function try_insert_slot2!(
    values::Vector{T},
    slots::Vector{UInt32},
    hs::HashSettings{L,S},
    nvalues::Int,
    nslots::Int,
    mask::Int,
    value,
    kloc::Int
) where {T,L,S}
    mp = hs.max_probe
    flag, slot, index = _lookup(values, slots, mp, mask, value, to_slot_index(try_convert(T, value), mask))
    if flag === 0x00
        # If value is not present, may need to increase max probe to find empty slot
        max_allowed = UInt8(max(return_uint8(L), mask >> return_uint8(S)))
        new_max_probe, index = _lookup_shift_max_probe(slots, mp, max_allowed, mask, index)
        if new_max_probe === max_allowed
            newsz = next_slot_size(nslots)
            unsafe_grow_end!(slots, newsz - nslots)
            i = nslots + 1
            while i <= newsz
                unsafe_set!(slots, i, EMPTY_SLOT)
                i += 1
            end
            new_mask = newsz - 1
            Base.GC.@preserve values begin
                hs.max_probe = _rehash!(values, slots, nvalues, mask, new_mask, hs.max_probe)
            end
            try_insert_slot2!(values, slots, hs, nvalues, newsz, new_mask, value, kloc)
            return (0x04, kloc)
        else
            hs.max_probe = new_max_probe
            unsafe_set!(slots, index, kloc)
            return (0x03, kloc)
        end
    elseif flag === 0x01
        unsafe_set!(slots, index, kloc)
        return (0x01, kloc)
    else  # flag === 0x02
        return (0x02, Int(slot))
    end
end

# `(true, index)` : successfully deleted from `index`
# `(false, index)` : `value` not found. `slots[index]` was last attempt at finding it.
function _try_delete!(
    values::Vector, slots::Vector{UInt32}, value, mp::UInt8,
    nvalues::Int=length(values), nslots::Int=length(slots)
)
    mask = nslots - 1
    flag, slot, index = _lookup(values, slots, mp, mask, value, to_slot_index(value, mask))
    if flag === 0x02
        _set!(slots, index, EMPTY_SLOT)
        _add_slots!(values, slots, Int(slot), nvalues, mask, -0x00000001)
        unsafe_delete_at!(values, slot, 1)
        return (true, Int(slot))
    else
        return (false, Int(slot))
    end
end

prev_slot_size(sz::Int) = sz>>1
next_slot_size(sz::Int) = 1<<(INT_SIZE-leading_zeros(sz + sz))

# rehash if >= 3/4 full
should_grow(nvalues::Int, nslots::Int) = nvalues >= (nslots >> 1) + (nslots >> 2)
# rehash if > 3/4 deleted
should_shrink(nvalues::Int, nslots::Int) = nvalues <= (nslots >> 3) + (nslots >> 4)

# _add_slots!
#
# when pushing or deleting a value, all subsequent value's corresponding slots need to be
# added or subtracted by 1.
function _add_slots!(
    values::Vector, slots::Vector{UInt32}, previ::Int, stop::Int, mask::Int, x::UInt32
)
    i = previ + 1
    while i <= stop
        index = to_slot_index(unsafe_get(values, i), mask)
        while true
            slot_i = unsafe_get(slots, index)
            if slot_i == i
                unsafe_set!(slots, index, slot_i + x)
                break
            end
            index = next_slot_index(index, mask)
        end
        i += 1
    end
    nothing
end

function _rehash!(
    values::Vector, slots::Vector{UInt32}, nvalues::Int,
    old_mask::Int, new_mask::Int, oldmp::UInt8
)
    newmp = 0x00
    kloc = 1
    while kloc <= nvalues
        si0 = reinterpret(Int, hash(unsafe_get(values, kloc)))
        newmp = max(newmp, _move_slot!(slots, si0, kloc, old_mask, new_mask, oldmp))
        kloc += 1
    end
    return newmp
end

function _maybe_grow_rehash!(
    hs::HashSettings, values::Vector, slots::Vector{UInt32}, nslots::Int, nvalues::Int,
)
    if should_grow(nvalues, nslots)
        newsz = next_slot_size(nslots)
        unsafe_grow_end!(slots, newsz - nslots)
        i = nslots + 1
        while i <= newsz
            _set!(slots, i, EMPTY_SLOT)
            i += 1
        end
        Base.GC.@preserve values begin
            hs.max_probe = _rehash!(values, slots, nvalues, nslots-1, newsz - 1, hs.max_probe)
        end
    end
    nothing
end
function _maybe_shrink_rehash!(
    hs::HashSettings, values::Vector, slots::Vector{UInt32}, nslots::Int, nvalues::Int,
)
    if should_shrink(nvalues, nslots)
        newsz = prev_slot_size(nslots)
        Base.GC.@preserve values begin
            hs.max_probe = _rehash!(values, slots, nvalues, nslots-1, newsz - 1, hs.max_probe)
        end
        unsafe_delete_end!(slots, nslots - newsz)
    end
    nothing
end

function try_insert_slot!(
    values::Vector, slots::Vector{UInt32}, hs::HashSettings{L,S}, mask::Int, key, kloc::Int
) where {L,S}
    itr = 0x00
    sloc = to_slot_index(key, mask)
    mp = hs.max_probe
    while itr <= mp
        slot_i = unsafe_get(slots, sloc)
        if slot_i === EMPTY_SLOT
            unsafe_set!(slots, sloc, kloc)
            return 0
        end
        key == unsafe_get(values, slot_i) && return Int(slot_i)
        sloc = next_slot_index(sloc, mask)
        itr += 0x01
    end

    # If key is not present, may need to keep searching to find slot
    maxallowed = UInt8(max(return_uint8(L), mask >> return_uint8(S)))
    while true
        if unsafe_get(slots, sloc) === EMPTY_SLOT
            unsafe_set!(slots, sloc, kloc)
            hs.max_probe = itr
            return 0
        end
        sloc = next_slot_index(sloc, mask)
        itr === maxallowed && break
        itr += 0x01
    end
    _maybe_grow_rehash!(hs, values, slots, length(slots), length(values))
    return try_insert_slot!(values, slots, hs, length(slots) - 1, key, kloc)
end

