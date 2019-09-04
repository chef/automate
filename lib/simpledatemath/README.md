# Simple DateMath

The goal of this library is to support a *very* small subset of Elastic's
Date Math expressions in Go. The use case for this library is to keep APIs
that allow users to express relative time ranges consistent regardless of
the backend technology.

## Why Elasticsearch DateMath?
We are starting in the situation of having some APIs that pass values
directly into an Elastic Date Math expression, so maintaining
compatibility means that using a subset of Elasticsearch Date Math is the
easiest option.

