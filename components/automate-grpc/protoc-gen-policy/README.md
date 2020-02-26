# Proto-file-based Policy Mappings

These are the (ordinary, garden-variety) protobuf definitions used
to annotate _other_ protobuf definitions with policy information.

Example (from `components/automate-gateway/api/iam/v2/teams.proto`):

```go
  rpc CreateTeam (CreateTeamReq) returns (CreateTeamResp) {
    option (google.api.http) = {
      post: "/iam/v2/teams"
      body: "*"
    };
    option (chef.automate.api.policy).resource = "auth:teams";
    option (chef.automate.api.policy).action = "create";
    option (chef.automate.api.iam.policy).resource = "iam:teams";
    option (chef.automate.api.iam.policy).action = "iam:teams:create";
  };
```

After modifying proto files in *this* directory, rebuild the `*.pb.go` files
using `compile_go_protobuf_component automate-grpc` in the top-level directory in hab studio
and include those regenerated files in your commit.

Next, to update the `*.pb.policy.go` files in the gateway, which rely on these generated files,
run `compile_go_protobuf_component automate-gateway` in the top-level directory in hab studio
and include those regenerated files in your commit.

See also the [AuthZ readme](../../authz-service/README.md) for more on policies.

## Under The Covers -- For Development Only

For convenience/expediency, you can also rebuild the `*.pb.policy.go` files
individually--and without the overhead of hab studio.
While OK to do this while iterating during rapid development cycles,
_do not commit any files generated this way_. There is no guarantee that your
version of `protoc` and the production-version we use are the same
so subtle, hard-to-find issues may crop up down the line.

That said, here are the steps:

1. Install `protoc`.

```bash
brew install protobuf
```

1. Install `protoc-gen-policy`, a dependency of `protoc`.

_Note that you need to re-run this step should you update files in this directory._

```bash
go install github.com/chef/automate/components/automate-grpc/protoc-gen-policy
```

1. Execute `protoc` from your `a2` root directory.

In this example, it is regenerating
teams.pb.policy.go from teams.proto, which was annotated with some policy information.

```bash
protoc -I$GOPATH/src -I$PWD/vendor -I$PWD/vendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis \
-I. --policy_out=v=4,logtostderr=true:$GOPATH/src $PWD/components/automate-gateway/api/iam/v2/teams.proto`
```

The `--policy_out` switch is what triggers `protoc` to call `protoc-gen-policy`
(which you've installed above)
and thus regenerate the policy-related proto files in particular.
