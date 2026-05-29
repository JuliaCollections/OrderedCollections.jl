# support functions

const orderedset_seed = UInt === UInt64 ? 0x2114638a942a91a5 : 0xd86bdbf1

struct NotFoundSentinel end  # Struct to mark not not found

@static if VERSION < v"1.11"
    const Memory = Vector
end
