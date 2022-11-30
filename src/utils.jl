
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

unsafe_get(x::Vector, i::UInt32) = Core.arrayref(false, x, Int(i))
unsafe_get(x::Vector, i::Int) = Core.arrayref(false, x, i)

# This copies the effects from `Base.infer_effects(Core.arrayref)` and adds `nothrow` b/c
# we only ever use it after bounds checking on defined indices (isassigned(x, i) == true)
unsafe_set!(x::Vector{T}, i::Int, v) where {T} = unsafe_set!(x, i, convert(T, v))
unsafe_set!(x::Vector{T}, i::Int, v::T) where {T} = Core.arrayset(false, x, v, i)

try_convert(::Type{T}, x::T) where {T} = x
function try_convert(::Type{T}, x0) where {T}
    x = convert(T, x0)
    return_bool(isequal(x, x0)) || throw(ArgumentError("$(x0) is not a valid key for type $T"))
    return x
end

