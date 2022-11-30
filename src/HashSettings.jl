
mutable struct HashSettings{L,S}
    max_probe::UInt8
    age::UInt32

    function HashSettings{L,S}(max_probe::UInt8) where {L,S}
        new{L,S}(max_probe, 0x00000000)
    end
    HashSettings() = HashSettings{0x10,0x06}(0x00)
end

Base.copy(x::HashSettings{L,S}) where {L,S} = HashSettings{L,S}(getfield(x, :max_probe))

@inline function Base.getproperty(x::HashSettings{L,S}, s::Symbol) where {L,S}
    if s === :probe_limit
        return return_uint8(L)
    elseif s === :probe_shift
        return return_uint8(S)
    else
        return getfield(x, s)
    end
end

Base.propertynames(::HashSettings) = (:max_probe, :probe_limit, :probe_shift, :age)

