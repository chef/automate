# OPA decision engine

## [`policy.go`](policy.go)

This file is generated from `policy/*.rego`.

The generation is done using go-bindata, from `go generate`. Run this from `components/authz-service`:

```
$ go generate ./...
$ # no output on success
```

It requires that go-bindata is installed.

TODO (sr): ensure this works properly in habitat and studio env. Other projects also use go-bindata, IIRC.
