# Package `pcmp`

`pcmp` and the related `passert` and `prequire` provide equality checking to
replace `reflect.DeepEqual`, which no longer works for protocol buffer
messages.

tl;dr: if you're using `assert.Equal(t, expectedMessage, actualMessage)` in
your tests and it doesn't work, using `passert.Equal(...)` instead should fix
it.

## Longer Version

The newest generated code for protobufs a function pointer in the proto message
structs in order to make all comparisons between the objects fail, and that
breaks `reflect.DeepEqual`. `Equal` in the testify packages is implemented with
`DeepEqual`, so it no longer works for proto messages. There is
`github.com/google/go-cmp/cmp` as an alternative for `DeepEqual` use cases but
it doesn't provide any useful functionality out of the box, you have to provide
some magic incantations to make it fix the proto comparison breakage. And to
use that in tests you need to reimplement some of testify's code. That's what
this package does.

There is also "google.golang.org/protobuf/testing/protocmp" which is supposed
to help with this but I didn't find it helpful. You could maybe reimplement the
functionality here by using that package if needed.
