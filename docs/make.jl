using Documenter
using OrderedCollections

makedocs(
	modules = [OrderedCollections],
	sitename = "OrderedCollections.jl",
    pages = [
        "index.md",
        "ordered_containers.md",
    ]
)

deploydocs(
    repo = "github.com/JuliaCollections/OrderedCollections.jl.git"
)
