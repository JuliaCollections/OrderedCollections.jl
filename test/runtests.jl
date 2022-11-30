using OrderedCollections
using Random
using Serialization
using Test


@testset "OrderedSet" begin
    include("OrderedSet.jl")
end
@testset "OrderedDict" begin
    include("OrderedDict.jl")
end
@testset "LittleDict" begin
    include("LittleDict.jl")
end



#=
@test isempty(detect_ambiguities(Base, Core, OrderedCollections))

tests = [
         "little_dict",
         "ordered_dict",
         "ordered_set",
        ]

if length(ARGS) > 0
    tests = ARGS
end

@testset "OrderedCollections" begin

for t in tests
    fp = joinpath(dirname(@__FILE__), "test_$t.jl")
    println("$fp ...")
    include(fp)
end

end # @testset
=#
