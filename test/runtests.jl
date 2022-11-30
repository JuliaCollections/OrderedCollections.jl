
using OrderedCollections
using Random
using Serialization
using Test

@test isempty(detect_ambiguities(Base, Core, OrderedCollections))

@testset "OrderedSet" begin
    include("OrderedSet.jl")
end

@testset "OrderedDict" begin
    include("OrderedDict.jl")
end

@testset "LittleDict" begin
    include("LittleDict.jl")
end

