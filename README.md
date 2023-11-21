[![Travis Build Status](https://travis-ci.org/JuliaCollections/OrderedCollections.jl.svg?branch=master)](https://travis-ci.org/JuliaCollections/OrderedCollections.jl)

[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/5gw9xok4e58aixsv?svg=true)](https://ci.appveyor.com/project/kmsquire/datastructures-jl)
[![Test Coverage](https://codecov.io/github/JuliaCollections/OrderedCollections.jl/coverage.svg?branch=master)](https://codecov.io/github/JuliaCollections/OrderedCollections.jl?branch=master)

[![Documentation](https://img.shields.io/badge/docs-latest-blue.svg)](https://juliacollections.github.io/OrderedCollections.jl/latest)

OrderedCollections.jl
=====================

Note: v2.0 removes deprecations present in v1.6.2 (for `sort!(::Dict)` and `similar(::Union{OrderedDict, OrderedSet})`), but is otherwise identical.

This package implements OrderedDicts and OrderedSets, which are similar to containers in base Julia.
However, during iteration the Ordered* containers return items in the order in which they were added to the collection.
It also implements `LittleDict` which is a ordered dictionary, that is much faster than any other `AbstractDict` (ordered or not) for small collections.

This package was split out from [DataStructures.jl](https://github.com/JuliaCollections/DataStructures.jl).

Resources
---------

-   **Documentation**: https://juliacollections.github.io/OrderedCollections.jl/latest
