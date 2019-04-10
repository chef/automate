# `protoc-gen-grpc-mock`

This is a `protoc-gen-*` binary to be used for generating GRPC server mock implementations
in a convenient and type-safe way.

There is two different variants: for example, with `UsersMgmtServer`, you can chose either

```
mockLocalUser := local_users_api.NewUsersMgmtServerMock()
```

or
```
mockLocalUser := local_users_api.NewUsersMgmtServerMockWithoutValidation()
```

The former will **attempt** to validate incoming requests in its mock handler functions:
if the request satisfies the interface

```
Validate() error
```

it will call this function, and return a status with `codes.InvalidArgument` if it exits non-nil.

This interface is what [PGV](https://github.com/lyft/protoc-gen-validate) gives us, which is currently used for API validation in authz-service.

## Generate mock code

Use it via a component's `grpc.sh` script, after having installed it
(the studio takes care of this for you).

The relevant snippet that goes into the components `scripts/grpc.sh` looks like this:

```bash
protoc -I. \
  -I$GOPATH/src \
  -Ivendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
  --grpc-mock_out=$GOPATH/src \
  path/to/your/*.proto
```

Note that the script is meant to be run from the top-level a2 directory in the studio,
using its `compile_go_protobuf_component` helper.
For example, generating the mocks for authz-service:

```bash
[0][default:/src:0]# compile_go_protobuf_component authz-service
```

For developing this, or to manually install this, run

```bash
install_go_tool github.com/chef/automate/components/automate-grpc/protoc-gen-grpc-mock
```
