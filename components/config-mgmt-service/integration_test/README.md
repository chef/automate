# gRPC Integration Tests
Welcome the integration tests for the config-mgmt service.

# How to run the tests
Below is the steps to run the tests:
```
$ hab studio enter
[1][default:/src:0]# start_deployment_service
[2][default:/src:0]# chef-automate dev deploy-some chef/automate-elasticsearch
[3][default:/src:0]# config_mgmt_integration
```

This will run the integration tests by leveraging the helper `go_test`

# How do I run a single test
To run a single test you have to step into the Go Workspace inside your habitat studio, you can use the
helper method `enter_go_workspace`
```
$ hab studio enter
[1][default:/src:0]# enter_go_workspace
=> Configuring Go Workspace. [please wait]
=> Available scaffolding variables:
$scaffolding_go_gopath        - /hab/cache/src/go
$scaffolding_go_workspace_src - /hab/cache/src/go/src
$scaffolding_go_pkg_path      - /hab/cache/src/go/src/github.com/chef/automate/components/config-mgmt-service
[3][default:/hab/cache/src/go/src/github.com/chef/automate/components/config-mgmt-service:0]#
```

This helper will automatically configure the Go workspace (by leveraging the `scaffolding-go` hooks) and it
will also position yourself inside the config-mgmt-service directory where you can run any command like `go`,
`dep`, etc. natively.

Before running any test manually, make sure you bring up elasticsearch by running `start_deployment_service` then `chef-automate dev deploy-some chef/automate-elasticsearch`,
otherwise you will see the following error when running any of them:
```
Could not create elasticsearch client from 'http://elasticsearch:9200': health check timeout: no Elasticsearch node available
exit status 1
FAIL	github.com/chef/automate/components/config-mgmt-service/integration_test	6.016s
```

Lets assume you are creating a rpc function for a new functionality called `Foo()`, to test it you would want to
create a test function called (for example) `TestFooEmptyRequest()`, in order to run just that single test you
can execute:
```
[3][default:/hab/cache/src/go/src/github.com/chef/automate/components/config-mgmt-service:0]# go test -v github.com/chef/automate/components/config-mgmt-service/integration_test -run TestFooEmptyRequest
=== RUN   TestFooEmptyRequest
--- PASS: TestFooEmptyRequest (0.00s)
PASS
ok  	github.com/chef/automate/components/config-mgmt-service/integration_test	2.904s
```

_NOTE: Remember that you have to enter the go workspace to run these commands. (use `egw` alias to do so)_

Now lets imagine that you continue writing more test cases for the same new functionality you just added (called
`Foo()` remember) so you add the following test functions:
* `TestFooSimpleRequest()`
* `TestFooWrongRequestReturnError()`
* `TestFooWithPagination()`
* `TestFooWithSorting()`
* `TestFooWithFilters()`
* `TestFooTableDriven()`

If you would like to run all these tests at once, you can just provide a regex that matches the tests names,
for example you can use `TestFoo` and it will run all tests that starts with that pattern:
```
[3][default:/hab/cache/src/go/src/github.com/chef/automate/components/config-mgmt-service:0]# go test -v github.com/chef/automate/components/config-mgmt-service/integration_test -run TestFoo
```

Follow this [link](https://golang.org/pkg/testing/#hdr-Subtests_and_Sub_benchmarks) to know more about available regex to match tests or sub-tests.
