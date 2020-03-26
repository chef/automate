# How Automate Ensures Integrity of Protobuf Files

It is vital that API proto files are annotated consistently, correctly, and completely for Automate. There are safeguards in place to ensure that both creating and modifying proto files is done in a way to ensure this.

The first hurdle is that when a proto file is edited one needs to regenerate code files from that proto file. There is a unit test that monitors that, so if one attempts to commit files without that step, buildkite will fail that step.

The first case below shows the execution of that unit test for this fail-to-regenerate scenario. The remaining use cases show the result of the same unit test, plus the result of the regeneration step itself.
(That overlaps in some checks, but provides even more consistency checking for semantic notions as well.)

All of the following apply equally to v1 and v2 policies.

## Edit a proto file without regeneration

``` console
$ go test ./components/automate-gateway/api
TestGeneratedProtobufUpToDate
--- FAIL: TestGeneratedProtobufUpToDate (0.06s)
    --- FAIL: TestGeneratedProtobufUpToDate/components/automate-gateway/api/iam/v2/teams.proto (0.00s)
--- FAIL: TestGeneratedPolicyUpToDate (1.42s)
    --- FAIL: TestGeneratedPolicyUpToDate/components/automate-gateway/api/iam/v2/teams.proto (1.30s)
```

## Missing ALL actions/resource

// just reports on first failure found

``` console
$ go test ./components/automate-gateway/api
--- FAIL: TestAllProtoFilesAnnotated (0.13s)
    --- FAIL: TestAllProtoFilesAnnotated/components/automate-gateway/api/iam/v2/teams.proto (0.01s)
        api_test.go:54: components/automate-gateway/api/iam/v2/teams.proto:chef.automate.api.iam.policy:Teams.GetVersion: no annotations found

# compile_go_protobuf_component automate-gateway
GEN: components/automate-gateway/api/iam/v2/teams.proto
[policy] couldn't apply v2 template: method GetVersion has no extension of type chef.automate.api.iam.policy
```

## Missing action OR resource

``` console
$ go test ./components/automate-gateway/api
--- FAIL: TestAllProtoFilesAnnotated (0.13s)
    --- FAIL: TestAllProtoFilesAnnotated/components/automate-gateway/api/iam/v2/teams.proto (0.01s)
        api_test.go:54: components/automate-gateway/api/iam/v2/teams.proto:chef.automate.api.iam.policy:Teams.GetVersion: no resource found in "action:\"system:serviceVersion:get\" "

# compile_go_protobuf_component automate-gateway
GEN: components/automate-gateway/api/iam/v2/teams.proto
[policy] couldn't apply v2 template: GetVersion: policy info '&{system:serviceVersion:get }' is missing Resource
```

## Missing action AND resource

``` console
$ go test ./components/automate-gateway/api
--- FAIL: TestAllProtoFilesAnnotated (0.13s)
    --- FAIL: TestAllProtoFilesAnnotated/components/automate-gateway/api/iam/v2/teams.proto (0.01s)
        api_test.go:54: components/automate-gateway/api/iam/v2/teams.proto:chef.automate.api.iam.policy:Teams.GetVersion: no annotations found

# compile_go_protobuf_component automate-gateway
GEN: components/automate-gateway/api/iam/v2/teams.proto
[policy] couldn't apply v2 template: method GetVersion has no extension of type chef.automate.api.iam.policy
```

## Invalid syntax for Action or Resource

``` console
$ go test ./components/automate-gateway/api
--not checked--

# compile_go_protobuf_component automate-gateway
GEN: components/automate-gateway/api/iam/v2/teams.proto
[policy] couldn't apply v2 template: GetTeam: Resource 'iam:te*ams:dummy' does not match regex pattern "^[a-z][^:*]*(?::[^:*]+)*$"
```

## Resource referencing invalid input field

``` console
$ go test ./components/automate-gateway/api
--not checked--

# compile_go_protobuf_component automate-gateway
GEN: components/automate-gateway/api/iam/v2/teams.proto
[policy] couldn't apply v2 template: unknown field name(s) in pattern: id_other
```

## Path referencing invalid input field

``` console
$ go test ./components/automate-gateway/api
--not checked--

# compile_go_protobuf_component automate-gateway
GEN: components/automate-gateway/api/iam/v2/teams.proto
--grpc-gateway_out: no field "name" found in GetTeamReq
```
