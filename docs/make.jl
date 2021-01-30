using Documenter
using OrderedCollections


makedocs(
    format = Documenter.HTML(
        prettyurls = get(ENV, "CI", nothing) == "true"
    ),
    sitename = "OrderedCollections.jl",
    pages = [
        "index.md",
        "ordered_containers.md",
    ]
)

deploydocs(
    repo = "github.com/JuliaCollections/OrderedCollections.jl.git",
    target = "build",
    deps = nothing,
    make = nothing,
    push_preview = true,
)
