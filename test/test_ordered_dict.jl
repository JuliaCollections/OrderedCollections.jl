using OrderedCollections, Test

@testset "OrderedDict" begin

    @testset "Constructors" begin
        @test isa(@inferred(OrderedDict{Int,Float64}(OrderedCollections.Memory{NTuple{16,UInt8}}(undef, 0), OrderedCollections.Memory{Int32}(undef, 0), Vector{Int}(), Vector{Float64}(), 0, false)), OrderedDict{Int,Float64})
        @test isa(@inferred(OrderedDict()), OrderedDict{Any,Any})
        @test isa(@inferred(OrderedDict([(1,2.0)])), OrderedDict{Int,Float64})
        @test isa(@inferred(OrderedDict([("a",1),("b",2)])), OrderedDict{String,Int})
        @test isa(@inferred(OrderedDict(Pair(1, 1.0))), OrderedDict{Int,Float64})
        @test isa(@inferred(OrderedDict(Pair(1, 1.0), Pair(2, 2.0))), OrderedDict{Int,Float64})
        @test isa(@inferred(OrderedDict{Int,Float64}(Pair(1, 1), Pair(2, 2))), OrderedDict{Int,Float64})
        @test isa(@inferred(OrderedDict(Pair(1, 1.0), Pair(2, 2.0), Pair(3, 3.0))), OrderedDict{Int,Float64})
        @test OrderedDict(()) == OrderedDict{Any,Any}()
        @test isa(@inferred(OrderedDict([Pair(1, 1.0), Pair(2, 2.0)])), OrderedDict{Int,Float64})
        @test_throws ArgumentError OrderedDict([1,2,3,4])
        iter = Iterators.filter(x->x.first>1, [Pair(1, 1.0), Pair(2, 2.0), Pair(3, 3.0)])
        @test @inferred(OrderedDict(iter)) == OrderedDict{Int,Float64}(2=>2.0, 3=>3.0)
        iter = Iterators.drop(1:10, 1)
        @test_throws ArgumentError OrderedDict(iter)
    end

    @testset "empty dictionary" begin
        d = OrderedDict{Char, Int}()
        @test length(d) == 0
        @test isempty(d)
        @test_throws KeyError d['c'] == 1
        d['c'] = 1
        @test !isempty(d)
        @test_throws KeyError d[0.01]
        @test isempty(empty(d))
        empty!(d)
        @test isempty(d)

        # access, modification
        for c in 'a':'z'
            d[c] = c - 'a' + 1
        end

        @test (d['a'] += 1) == 2
        @test 'a' in keys(d)
        @test haskey(d, 'a')
        @test get(d, 'B', 0) == 0
        @test getkey(d, 'b', nothing) == 'b'
        @test getkey(d, 'B', nothing) == nothing
        @test !('B' in keys(d))
        @test !haskey(d, 'B')
        @test pop!(d, 'a') == 2

        @test collect(keys(d)) == collect('b':'z')
        @test collect(values(d)) == collect(2:26)
        @test collect(d) == [Pair(a,i) for (a,i) in zip('b':'z', 2:26)]
    end

    @testset "convert" begin
        d = OrderedDict{Int,Float32}(i=>Float32(i) for i = 1:10)
        @test convert(OrderedDict{Int,Float32}, d) === d
        dc = convert(OrderedDict{Int,Float64}, d)
        @test dc !== d
        @test keytype(dc) == Int
        @test valtype(dc) == Float64
        @test keys(dc) == keys(d)
        @test collect(values(dc)) == collect(values(d))
    end

    @testset "Issue #60" begin
        od60 = OrderedDict{Int,Int}()
        od60[1] = 2

        ranges = [2:5, 6:9, 10:13]
        for range in ranges
            for i = range
                od60[i] = i+1
            end
            for i = range
                delete!( od60, i )
            end
        end
        od60[14]=15

        @test od60[14] == 15
    end

    @testset "Issue #87" begin
        od1 = OrderedDict(nothing => 1, 2 => 3)
        delete!(od1, nothing)
        @test OrderedDict(2 => 3) == OrderedDict(od1...)

        od2 = OrderedDict(2 => 0.1, nothing => 0.2, 3 => 0.5)
        delete!(od2, nothing)
        @test OrderedDict(2 => 0.1, 3 => 0.5) == OrderedDict(od2...)

        od3 = OrderedDict(2 => 0.1, 5 => 0.4, nothing => 0.03, 10 => 0.4)
        delete!(od3, nothing)
        @test OrderedDict(2 => 0.1, 5 => 0.4, 10 => 0.4) == OrderedDict(od3...)
    end


    ##############################
    # Copied and modified from Base/test/dict.jl

    # OrderedDict

    @testset "OrderedDict{Int,Int}" begin
        h = OrderedDict{Int,Int}()
        for i=1:10000
            h[i] = i+1
        end

        @test collect(h) == [Pair(x,y) for (x,y) in zip(1:10000, 2:10001)]

        for i=1:2:10000
            delete!(h, i)
        end
        for i=1:2:10000
            h[i] = i+1
        end

        for i=1:10000
            @test h[i]==i+1
        end

        for i=1:10000
            delete!(h, i)
        end
        @test isempty(h)

        h[77] = 100
        @test h[77]==100

        for i=1:10000
            h[i] = i+1
        end

        for i=1:2:10000
            delete!(h, i)
        end

        for i=10001:20000
            h[i] = i+1
        end

        for i=2:2:10000
            @test h[i]==i+1
        end

        for i=10000:20000
            @test h[i]==i+1
        end
    end

    @testset "OrderedDict{Any,Any}" begin
        h = OrderedDict{Any,Any}([("a", 3)])
        @test h["a"] == 3
        h["a","b"] = 4
        @test h["a","b"] == h[("a","b")] == 4
        h["a","b","c"] = 4
        @test h["a","b","c"] == h[("a","b","c")] == 4
    end

    @testset "KeyError" begin
        z = OrderedDict()
        get_KeyError = false
        try
            z["a"]
        catch _e123_
            get_KeyError = isa(_e123_, KeyError)
        end
        @test get_KeyError
    end

    @testset "filter" begin
        _d = OrderedDict([("a", 0)])
        v = [k for k in filter(x->length(x)==1, collect(keys(_d)))]
        @test isa(v, Vector{String})
    end

    @testset "from tuple/vector/pairs/tuple of pair 1" begin
        d = OrderedDict(((1, 2), (3, 4)))
        d2 = OrderedDict([(1, 2), (3, 4)])
        d3 = OrderedDict(1 => 2, 3 => 4)
        d4 = OrderedDict((1 => 2, 3 => 4))

        @test d[1] === 2
        @test d[3] === 4

        @test d == d2 == d3 == d4
        @test isa(d, OrderedDict{Int,Int})
        @test isa(d2, OrderedDict{Int,Int})
        @test isa(d3, OrderedDict{Int,Int})
        @test isa(d4, OrderedDict{Int,Int})
    end

    @testset "from tuple/vector/pairs/tuple of pair 2" begin
        d = OrderedDict(((1, 2), (3, "b")))
        d2 = OrderedDict([(1, 2), (3, "b")])
        d3 = OrderedDict(1 => 2, 3 => "b")
        d4 = OrderedDict((1 => 2, 3 => "b"))

        @test d2[1] === 2
        @test d2[3] == "b"

        ## TODO: tuple of tuples doesn't work for mixed tuple types
        # @test d == d2 == d3 == d4
        # @test isa(d, OrderedDict{Int,Any})
        @test d2 == d3 == d4
        @test isa(d2, OrderedDict{Int,Any})
        @test isa(d3, OrderedDict{Int,Any})
        @test isa(d4, OrderedDict{Int,Any})
    end

    @testset "from tuple/vector/pairs/tuple of pair 3" begin
        d = OrderedDict(((1, 2), ("a", 4)))
        d2 = OrderedDict([(1, 2), ("a", 4)])
        d3 = OrderedDict(1 => 2, "a" => 4)
        d4 = OrderedDict((1 => 2, "a" => 4))

        @test d2[1] === 2
        @test d2["a"] === 4

        ## TODO: tuple of tuples doesn't work for mixed tuple types
        # @test d == d2 == d3 == d4
        @test d2 == d3 == d4
        # @test isa(d, OrderedDict{Any,Int})
        @test isa(d2, OrderedDict{Any,Int})
        @test isa(d3, OrderedDict{Any,Int})
        @test isa(d4, OrderedDict{Any,Int})
    end

    @testset "from tuple/vector/pairs/tuple of pair 4" begin
        d = OrderedDict(((1, 2), ("a", "b")))
        d2 = OrderedDict([(1, 2), ("a", "b")])
        d3 = OrderedDict(1 => 2, "a" => "b")
        d4 = OrderedDict((1 => 2, "a" => "b"))

        @test d[1] === 2
        @test d["a"] == "b"

        @test d == d2 == d3 == d4
        @test isa(d, OrderedDict{Any,Any})
        @test isa(d2, OrderedDict{Any,Any})
        @test isa(d3, OrderedDict{Any,Any})
        @test isa(d4, OrderedDict{Any,Any})
    end

    @testset "first" begin
        @test_throws ArgumentError first(OrderedDict())
        @test first(OrderedDict([(:f, 2)])) == Pair(:f,2)
    end

    @testset "last" begin
        @test last(OrderedDict([(:f, 2)])) == Pair(:f,2)
    end

    @testset "Issue #1821" begin
        d = OrderedDict{String, Vector{Int}}()
        d["a"] = [1, 2]
        @test_throws MethodError d["b"] = 1
        @test isa(repr(d), AbstractString)  # check that printable without error
    end

    @testset "Issue #2344" begin
        bestkey(d, key) = key
        bestkey(d::AbstractDict{K,V}, key) where {K<:AbstractString,V} = string(key)
        bar(x) = bestkey(x, :y)
        @test bar(OrderedDict([(:x, [1,2,5])])) == :y
        @test bar(OrderedDict([("x", [1,2,5])])) == "y"
    end

    @testset "isequal" begin
        @test  isequal(OrderedDict(), OrderedDict())
        @test  isequal(OrderedDict([(1, 1)]), OrderedDict([(1, 1)]))
        @test !isequal(OrderedDict([(1, 1)]), OrderedDict())
        @test !isequal(OrderedDict([(1, 1)]), OrderedDict([(1, 2)]))
        @test !isequal(OrderedDict([(1, 1)]), OrderedDict([(2, 1)]))

        @test isequal(OrderedDict(), sizehint!(OrderedDict(),96))

        # Here is what currently happens when dictionaries of different types
        # are compared. This is not necessarily desirable. These tests are
        # descriptive rather than proscriptive.
        @test !isequal(OrderedDict([(1, 2)]), OrderedDict([("dog", "bone")]))
        @test isequal(OrderedDict{Int,Int}(), OrderedDict{AbstractString,AbstractString}())
    end

    @testset "data_in" begin
        # Generate some data to populate dicts to be compared
        data_in = [ (rand(1:1000), randstring(2)) for _ in 1:1001 ]

        # Populate the first dict
        d1 = OrderedDict{Int, String}()
        for (k,v) in data_in
            d1[k] = v
        end
        data_in = collect(d1)
        # shuffle the data
        for i in 1:length(data_in)
            j = rand(1:length(data_in))
            data_in[i], data_in[j] = data_in[j], data_in[i]
        end
        # Inserting data in different (shuffled) order should result in
        # equivalent dict.
        d2 = OrderedDict{Int, AbstractString}()
        for (k,v) in data_in
            d2[k] = v
        end

        @test  isequal(d1, d2)
        d3 = copy(d2)
        d4 = copy(d2)
        # Removing an item gives different dict
        delete!(d1, data_in[rand(1:length(data_in))][1])
        @test !isequal(d1, d2)
        # Changing a value gives different dict
        d3[data_in[rand(1:length(data_in))][1]] = randstring(3)
        !isequal(d1, d3)
        # Adding a pair gives different dict
        d4[1001] = randstring(3)
        @test !isequal(d1, d4)
    end

    @testset "get!" begin
        # get! (get with default values assigned to the given location)
        f(x) = x^2
        d = OrderedDict(8 => 19)

        @test get!(d, 8, 5) == 19
        @test get!(d, 19, 2) == 2

        @test get!(d, 42) do  # d is updated with f(2)
            f(2)
        end == 4

        @test get!(d, 42) do  # d is not updated
            f(200)
        end == 4

        @test get(d, 13) do   # d is not updated
            f(4)
        end == 16

        @test d == OrderedDict(8=>19, 19=>2, 42=>4)
    end

    @testset "Issue #5886" begin
        d5886 = OrderedDict()
        for k5886 in 1:11
            d5886[k5886] = 1
        end
        for k5886 in keys(d5886)
            # undefined ref if not fixed
            d5886[k5886] += 1
        end
    end

    @testset "Issue #216" begin
        @test OrderedCollections.isordered(OrderedDict{Int, String})
        @test !OrderedCollections.isordered(Dict{Int, String})
    end

    @testset "Test merging" begin
        a = OrderedDict("foo"  => 0.0, "bar" => 42.0)
        b = OrderedDict("フー" => 17, "バー" => 4711)
        @test isa(merge(a, b), OrderedDict{String,Float64})
    end

    @testset "Issue #9295" begin
        d = OrderedDict()
        @test push!(d, 'a'=> 1) === d
        @test d['a'] == 1
        @test push!(d, 'b' => 2, 'c' => 3) === d
        @test d['b'] == 2
        @test d['c'] == 3
        @test push!(d, 'd' => 4, 'e' => 5, 'f' => 6) === d
        @test d['d'] == 4
        @test d['e'] == 5
        @test d['f'] == 6
        @test length(d) == 6
    end

    @testset "Serialization" begin
        s = IOBuffer()
        od = OrderedDict{Char,Int64}()
        for c in 'a':'e'
            od[c] = c-'a'+1
        end
        serialize(s, od)
        seek(s, 0)
        dd = deserialize(s)
        @test isa(dd, OrderedCollections.OrderedDict{Char,Int64})
        @test dd == od
        close(s)
    end

    @testset "Issue #148" begin
        d148 = OrderedDict(
            :gps => [],
            :direction => 1:8,
            :weather => 1:10
        )

        d148_2 = OrderedDict(
            :time => 1:10,
            :features => OrderedDict(
                :gps => 1:5,
                :direction => 1:8,
                :weather => 1:10
            )
        )
    end

    @testset "Issue #400" begin
        @test filter(p->first(p) > 1, OrderedDict(1=>2, 3=>4)) isa OrderedDict
    end

    @testset "Issue #30" begin
        d = OrderedDict(:a=>1, :b=>2)
        d1 = OrderedDict(k=>v for (k,v) in d)
        @test keytype(d1) == keytype(d)
        @test valtype(d1) == valtype(d)
    end

    @testset "Sorting" begin
        ks = [4, 8, 1, 7, 9, 3, 10, 2, 6, 5]
        d = OrderedDict(i=>Char(123-i) for i in ks)

        sd = sort(d)
        @test collect(keys(d)) == ks    # verify d is not changed by sort()
        @test collect(keys(sd)) == 1:10
        @test collect(values(sd)) == collect('z':-1:'q')
        @test sort(sd) == sd

        sdv = sort(d; byvalue=true)
        @test collect(keys(d)) == ks    # verify d is not changed by sort()
        @test collect(keys(sdv)) == 10:-1:1
        @test collect(values(sdv)) == collect('q':'z')

        sort!(d)
        @test collect(keys(d)) == 1:10
        @test collect(values(d)) == collect('z':-1:'q')
        @test sort(d) == d == sd

        sort!(d; byvalue=true)
        @test collect(keys(d)) == 10:-1:1
        @test collect(values(d)) == collect('q':'z')
        @test sort(d) == d == sd
    end

    @testset "Test that OrderedDict merge with combiner returns type OrderedDict" begin
        @test merge(+, OrderedDict(:a=>1, :b=>2), OrderedDict(:b=>7, :c=>4)) == OrderedDict(:a=>1, :b=>9, :c=>4)
        @test merge(+, OrderedDict(:a=>1, :b=>2), Dict(:b=>7, :c=>4)) isa OrderedDict
    end

    @testset "Test that OrderedDict mergewith returns type OrderedDict" begin
        @test mergewith(+, OrderedDict(:a=>1, :b=>2), OrderedDict(:b=>7, :c=>4)) == OrderedDict(:a=>1, :b=>9, :c=>4)
        @test mergewith(+, OrderedDict(:a=>1, :b=>2), Dict(:b=>7, :c=>4)) isa OrderedDict
    end

    @testset "map!(f, values(OrderedDict))" begin
        testdict = OrderedDict(:a=>1, :b=>2)
        map!(v->v-1, values(testdict))
        @test testdict[:a] == 0
        @test testdict[:b] == 1
    end

    @testset "Issue #47" begin
        @test eltype(OrderedDict(String => :string, SubString => :substring)) == Pair{Type,Symbol}
        @test eltype(OrderedDict(:string => String, :substring => SubString)) == Pair{Symbol,Type}
        @test eltype(OrderedDict(String => String, SubString => SubString)) == Pair{Type,Type}

        @test eltype(OrderedDict(tuple(String => :string, SubString => :substring))) == Pair{Type,Symbol}
        @test eltype(OrderedDict(tuple(:string => String, :substring => SubString))) == Pair{Symbol,Type}
        @test eltype(OrderedDict(tuple(String => String, SubString => SubString))) == Pair{Type,Type}
    end

    @testset "Issue #71" begin
        od = OrderedDict(Dict(i=>0 for i=1:158))
        sort!(od)
        @test od[158] == 0
    end

    @testset "Issue #71b" begin
        # This is actually a simplified version of #60, which was triggered while fixing #71
        # It doesn't actually fail on previous versions of OrderedCollections
        od = OrderedDict{Int,Int}(13=>13)
        delete!( od, 13 )
        od[14]=14
        @test od[14] == 14
    end

    @testset "Issue #65" begin
        x = OrderedDict{OrderedDict, Int}()
        x[x] = 0  # There's no reason to ever do this, but it shouldn't overflow the stack
        @test length(deepcopy(x)) == 1

        # Small numbers of deletes and inserts should not cause the table to grow.
        # (Internals differ from the original probe-based design, but the user-visible
        # invariant — rehashes don't fire on light churn — is the same.)
        od = OrderedDict{Int,Int}(i=>i for i in 1:5)
        nslots_before = OrderedCollections._current_nslots(od)
        for i in 1:4
            pop!(od, i)
        end
        od[6] = 6
        @test OrderedCollections._current_nslots(od) == nslots_before

        for i in 7:14
            od[i] = i
        end
        for i in 5:13
            pop!(od, i)
        end
        # End state: only key 14 remains live; ndel reflects the deleted ones.
        @test length(od) == 1
        @test haskey(od, 14)
        # Final insert should still leave the dict in a consistent state.
        od[15] = 15
        @test length(od) == 2
        @test od[14] == 14 && od[15] == 15
    end

    @testset "Issue #86" begin
        counter = 0
        expensive_function(k) = (counter += 1; k > 2 && error("too large!"))
        @test_throws ErrorException OrderedDict(k => expensive_function(k) for k in 1:3)
        if VERSION >= v"1.11"
            @test counter == 3
        else
            @test_broken counter == 3  # gives 6 instead
        end
    end

    @testset "ordered access" begin
        od = OrderedDict(:a=>1, :b=>2, :c=>3)
        @test popfirst!(od) == (:a => 1)
        @test :a ∉ keys(od)
        @test pop!(od) == (:c => 3)
        @test :c ∉ keys(od)
    end

    @testset "lazy reverse iteration" begin
        ks = collect('a':'z')
        vs = collect(0:25)
        od   = OrderedDict(k=>v for (k,v) in zip(ks, vs))
        pass = true
        for (n,(k,v)) in enumerate(Iterators.reverse(od))
            pass &= reverse(ks)[n] == k
            pass &= reverse(vs)[n] == v
        end
        @test pass
    end

end # @testset OrderedDict

@testset "Swiss-table primitives: hash split + group selection" begin
    import OrderedCollections: split_hash, group_for, slot_in_group, next_group_idx

    for k in (1, "abc", :foo, 1.5, (1,2), nothing)
        h1, h2 = split_hash(k)
        @test h1 isa UInt
        @test h2 isa UInt8
        @test h2 <= 0x7F
    end

    for ngroups in (1, 2, 4, 16, 1024)
        for trial in 1:50
            h1 = rand(UInt)
            g = group_for(h1, ngroups)
            @test 1 <= g <= ngroups
        end
    end

    @test slot_in_group(1, 0) == 1
    @test slot_in_group(1, 15) == 16
    @test slot_in_group(2, 0) == 17
    @test slot_in_group(5, 3) == 68

    for ngroups in (1, 2, 4, 8, 16, 32)
        visited = Set{Int}()
        g = 1
        step = 1
        for _ in 1:ngroups
            push!(visited, g)
            g = next_group_idx(g, step, ngroups)
            step += 1
        end
        @test length(visited) == ngroups
    end
end

@testset "Swiss-table primitives: group-scan ops" begin
    import OrderedCollections: match_byte, match_empty_or_deleted, CTRL_EMPTY, CTRL_DELETED

    g_empty = ntuple(_ -> CTRL_EMPTY, Val(16))
    @test match_byte(g_empty, 0x42) == 0
    @test match_byte(g_empty, CTRL_EMPTY) == 0xFFFF
    @test match_empty_or_deleted(g_empty) == 0xFFFF

    g_del = ntuple(_ -> CTRL_DELETED, Val(16))
    @test match_byte(g_del, 0x42) == 0
    @test match_byte(g_del, CTRL_EMPTY) == 0
    @test match_empty_or_deleted(g_del) == 0xFFFF

    g_full = ntuple(_ -> 0x42, Val(16))
    @test match_byte(g_full, 0x42) == 0xFFFF
    @test match_byte(g_full, 0x43) == 0
    @test match_byte(g_full, CTRL_EMPTY) == 0
    @test match_empty_or_deleted(g_full) == 0

    bytes = fill(0x10, 16)
    bytes[1] = 0x05
    bytes[8] = CTRL_EMPTY
    bytes[13] = CTRL_DELETED
    g_mix = ntuple(i -> bytes[i], Val(16))
    @test match_byte(g_mix, 0x05) == 0x0001
    @test match_byte(g_mix, 0x10) == 0xEF7E
    @test match_byte(g_mix, CTRL_EMPTY) == 0x0080
    @test match_empty_or_deleted(g_mix) == 0x1080
end

@testset "Swiss-table primitives: ctrl I/O" begin
    import OrderedCollections: load_group, set_ctrl_byte!, _empty_group, Memory

    @test all(b -> b == 0x80, _empty_group)

    ngroups = 2
    ctrl = Memory{NTuple{16,UInt8}}(undef, ngroups)
    fill!(ctrl, _empty_group)
    g1 = load_group(ctrl, 1)
    @test all(b -> b == 0x80, g1)

    set_ctrl_byte!(ctrl, 5, 0x42)
    g1 = load_group(ctrl, 1)
    @test g1[5] == 0x42
    @test g1[4] == 0x80 && g1[6] == 0x80

    set_ctrl_byte!(ctrl, 17, 0x33)
    g2 = load_group(ctrl, 2)
    @test g2[1] == 0x33

    set_ctrl_byte!(ctrl, 1, 0x77)
    @test load_group(ctrl, 1)[1] == 0x77
end

# --- Swiss-table test helper types (module scope) ---

struct OneGroupKey
    v::Int
end
Base.hash(k::OneGroupKey, h::UInt) = h
Base.isequal(a::OneGroupKey, b::OneGroupKey) = a.v == b.v
Base.:(==)(a::OneGroupKey, b::OneGroupKey) = a.v == b.v

struct H2Twin
    v::Int
    h::UInt
end
Base.hash(k::H2Twin, h::UInt) = k.h
Base.isequal(a::H2Twin, b::H2Twin) = a.v == b.v
Base.:(==)(a::H2Twin, b::H2Twin) = a.v == b.v

@testset "Swiss: group-boundary stress" begin
    d = OrderedDict{OneGroupKey,Int}()
    for i in 1:32
        d[OneGroupKey(i)] = i * 100
    end
    @test length(d) == 32
    for i in 1:32
        @test d[OneGroupKey(i)] == i * 100
    end
    @test [k.v for (k, _) in d] == collect(1:32)

    for i in 2:2:32
        delete!(d, OneGroupKey(i))
    end
    @test length(d) == 16
    @test [k.v for (k, _) in d] == collect(1:2:31)
    for i in 1:2:31
        @test d[OneGroupKey(i)] == i * 100
    end
end

@testset "Swiss: h2 fingerprint collisions" begin
    top = UInt(0x42) << (8*sizeof(UInt) - 7)
    a = H2Twin(1, top | UInt(1))
    b = H2Twin(2, top | UInt(2))

    d = OrderedDict{H2Twin,String}()
    d[a] = "alpha"
    d[b] = "beta"

    @test d[a] == "alpha"
    @test d[b] == "beta"
    @test length(d) == 2
    @test !haskey(d, H2Twin(3, top | UInt(3)))
end

@testset "Swiss: tombstone vs empty after delete" begin
    d = OrderedDict{Int,Int}()
    for i in 1:50
        d[i] = i * 10
    end
    for i in 1:5:50
        delete!(d, i)
    end
    @test length(d) == 50 - 10
    for i in 1:50
        if i % 5 == 1
            @test !haskey(d, i)
        else
            @test d[i] == i * 10
        end
    end
    for i in 100:130
        d[i] = i
    end
    for i in 100:130
        @test d[i] == i
    end
end

@testset "Swiss: operations on empty dict" begin
    d = OrderedDict{Int,Int}()
    @test length(d.ctrl) == 0
    @test length(d.idx) == 0
    @test length(d) == 0
    @test isempty(d)
    @test !haskey(d, 1)
    @test get(d, 1, -1) == -1
    @test_throws KeyError d[1]
    @test_throws KeyError pop!(d, 1)
    @test pop!(d, 1, :nope) === :nope
    delete!(d, 1)
    @test length(d) == 0
    @test collect(d) == Pair{Int,Int}[]

    d[1] = 10
    @test length(d.ctrl) > 0
    @test d[1] == 10
end

@testset "Swiss: large table grows correctly" begin
    d = OrderedDict{Int,Int}()
    N = 200_000
    for i in 1:N
        d[i] = i
    end
    @test length(d) == N
    for i in 1:N
        @test d[i] == i
    end
    @test [k for (k, _) in d] == collect(1:N)
end
