var documenterSearchIndex = {"docs": [

{
    "location": "index.html#",
    "page": "OrderedCollections.jl",
    "title": "OrderedCollections.jl",
    "category": "page",
    "text": ""
},

{
    "location": "index.html#OrderedCollections.jl-1",
    "page": "OrderedCollections.jl",
    "title": "OrderedCollections.jl",
    "category": "section",
    "text": "This package implements associative containers that preserve the order of insertion:OrderedDict\nOrderedSet"
},

{
    "location": "index.html#Contents-1",
    "page": "OrderedCollections.jl",
    "title": "Contents",
    "category": "section",
    "text": "Pages = [\n    \"index.md\",\n    \"ordered_containers.md\",\n]"
},

{
    "location": "ordered_containers.html#",
    "page": "OrderedDicts and OrderedSets",
    "title": "OrderedDicts and OrderedSets",
    "category": "page",
    "text": ""
},

{
    "location": "ordered_containers.html#OrderedDicts-and-OrderedSets-1",
    "page": "OrderedDicts and OrderedSets",
    "title": "OrderedDicts and OrderedSets",
    "category": "section",
    "text": "OrderedDicts are simply dictionaries whose entries have a particular order. For OrderedDicts (and OrderedSets), order refers to insertion order, which allows deterministic iteration over the dictionary or set:d = OrderedDict{Char,Int}()\nfor c in \'a\':\'e\'\n    d[c] = c-\'a\'+1\nend\ncollect(d) # => [(\'a\',1),(\'b\',2),(\'c\',3),(\'d\',4),(\'e\',5)]\n\ns = OrderedSet(π,e,γ,catalan,φ)\ncollect(s) # => [π = 3.1415926535897...,\n           #     e = 2.7182818284590...,\n           #     γ = 0.5772156649015...,\n           #     catalan = 0.9159655941772...,\n           #     φ = 1.6180339887498...]All standard Associative and Dict functions are available for OrderedDicts, and all Set operations are available for OrderedSets.Note that to create an OrderedSet of a particular type, you must specify the type in curly-braces:# create an OrderedSet of Strings\nstrs = OrderedSet{AbstractString}()"
},

]}
