using Markdown

const MDLike = Union{String,Markdown.MD}

# Markdown-aware show helper
_show(x::AbstractString) = println(x)
_show(x::Markdown.MD) = display(x)
_show(x) = display(x)