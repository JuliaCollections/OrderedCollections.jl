
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
        i = (i & mask) + 1
    end
end

function _lookup_shift_max_probe(
    slots::Vector{UInt32}, itr::UInt8, max_allowed::UInt8, mask::Int, i::Int,
)
    while true
        slot_i = unsafe_get(slots, i)
        slot_i === EMPTY_SLOT && return (itr, i)
        itr === max_allowed && return (max_allowed, i)
        itr += 0x01
        i = (i & mask) + 1
    end
end

# flags
#   `0x01` : `index` is 
# `(0x01, index)` : did not find `value`. `index` is an empty slot where new index for `value` can be set
# `(0x02, index)` : found `values[index] == value`
function try_insert_slot!(
    values::Vector{T},
    slots::Vector{UInt32},
    hs::HashSettings{L,S},
    nvalues::Int,
    nslots::Int,
    mask::Int,
    value,
    kloc::Int
) where {T,L,S}
    mp = _max_probe(hs)
    flag, slot, index = _lookup(values, slots, mp, mask, value, to_slot_index(try_convert(T, value), mask))
    if flag === 0x00
        # If value is not present, may need to increase max probe to find empty slot
        max_allowed = UInt8(max(return_uint8(L), mask >> return_uint8(S)))
        new_max_probe, index = _lookup_shift_max_probe(slots, mp, max_allowed, mask, index)
        if new_max_probe === max_allowed
            newsz = 1<<(INT_SIZE-leading_zeros(nslots + nslots))
            unsafe_grow_end!(slots, newsz - nslots)
            i = nslots + 1
            while i <= newsz
                unsafe_set!(slots, i, EMPTY_SLOT)
                i += 1
            end
            new_mask = newsz - 1
            Base.GC.@preserve values begin
                _max_probe!(hs, _rehash!(values, slots, nvalues, mask, new_mask, _max_probe(hs)))
            end
            try_insert_slot!(values, slots, hs, nvalues, newsz, new_mask, value, kloc)
            return (0x04, kloc)
        else
            _max_probe!(hs, new_max_probe)
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
            index = (index & mask) + 1
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
    if nvalues >= (nslots >> 1) + (nslots >> 2)  # rehash if >= 3/4 full
        newsz = 1<<(INT_SIZE-leading_zeros(nslots + nslots))
        unsafe_grow_end!(slots, newsz - nslots)
        i = nslots + 1
        while i <= newsz
            unsafe_set!(slots, i, EMPTY_SLOT)
            i += 1
        end
        new_mask = newsz - 1
        Base.GC.@preserve values begin
            _max_probe!(hs, _rehash!(values, slots, nvalues, nslots-1, new_mask, _max_probe(hs)))
        end
    end
    nothing
end
function _maybe_shrink_rehash!(
    hs::HashSettings, values::Vector, slots::Vector{UInt32}, nslots::Int, nvalues::Int,
)
    if nvalues <= (nslots >> 3) + (nslots >> 4)  # rehash if > 3/4 deleted
        newsz = nslots>>1
        new_mask = newsz - 1
        Base.GC.@preserve values begin
            _max_probe!(hs, _rehash!(values, slots, nvalues, nslots-1, new_mask, _max_probe(hs)))
        end
        unsafe_delete_end!(slots, nslots - newsz)
    end
    nothing
end

function unsafe_empty_slot!(slots::Vector{UInt32}, i::Int, slot, mp::UInt8, mask::Int)
    itr = 0x00
    while true
        if unsafe_get(slots, i) == slot
            unsafe_set!(slots, i, EMPTY_SLOT)
            break
        end
        i = (i & mask) + 1
        itr === mp && break
        itr += 0x01
    end
    return nothing
end

function _move_slot!(
    slots::Vector{UInt32}, si0::Int, kloc::Int, old_mask::Int, new_mask::Int, mp::UInt8
)
    # 1. Slot removal loop exits if:
    #   - slot corresponding to key location is found
    #   - exceeds maximum probe (meaning it was previously overwritten by `_move_slot!`
    #     on a lower key index.
    unsafe_empty_slot!(slots, (si0 & old_mask) + 1, kloc, mp, old_mask)
    # 2. Slot filling loop replaces `slot_i` if:
    #   - `slot_i` is empty
    #   - `slot_i` corresponds to a key index would be erased in subsequent iterations
    itr = 0x00
    i = (si0 & new_mask) + 1
    while true
        slot_i = unsafe_get(slots, i)
        if slot_i === EMPTY_SLOT || slot_i > kloc
            unsafe_set!(slots, i, kloc)
            break
        end
        i = (i & new_mask) + 1
        itr += 0x01
    end
    itr
end

