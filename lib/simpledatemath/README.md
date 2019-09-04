# Simple DateMath

The goal of this library is to support a *very* small subset of elastic's
Date Math expressions in Go. The use case for this library is to keep APIs
that allow users to express relative time ranges consistent regardless of
the backend technology.

## Why elasticsearch DateMath?
We are starting in the situation of having some APIs that pass values
directly into an elastic Date Math expression, so maintaining
compatibility means that using a subset of elasticsearch Date Math is the
easiest option.

## Why not `time.ParseDuration`?

Golang provides a means of parsing a string into a duration via
`time.ParseDuration`, but it doesn't support any units larger than hours.
For our use case, days is often the most natural unit and we expose it in
our web interface already.

## Resources

* Date Math reference: https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#date-math
* Golang `time.ParseDuration`: https://golang.org/src/time/format.go?s=40400:40446#L1370
