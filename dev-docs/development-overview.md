# Development

## Go Dependencies

We're using Go's modules now. For an introduction, see:
https://blog.golang.org/using-go-modules

### How to add a new dependency

When you want to use a new module in your code, just add an `import` statement for the module and use it:

```go
import "rsc.io/quote"
. . .
func Hello() string {
    return quote.Hello()
}
```

The next time you build, the module will be automatically downloaded and your go.mod/go.sum files updated.
Be sure to commit the go.mod/go.sum changes.

You can check that the imported module is in place with, for example:

```shell
go list -m rsc.io/q...
```

### How to remove a dependency

It is important to be a good steward and clean up after yourself when you remove a dependency.
Using `go build` can easily tell when something is missing and needs to be added, but not when it can safely be removed.
The `go mod tidy` command is provided to do this extra work, but you must run it explicitly after you have removed uses of the no-longer-needed dependency from your code.
If you run `go list -m all` before and after the tidy command, you should see that it has removed the unused dependency from both Go's cache and your go.mod/go.sum files.
Be sure to commit the go.mod/go.sum changes.

### Components/Services

Each component is self-contained and needs to ensure its stability. Think about a component that could also be an external solution. Each component has its own contracts like API or CLI interface. Those need to be tested with special care since any change will impact users of this components. A user may also be another component. Therefore it is essential that each component has strong interfaces and testing for those.

On a component level we test the following:

- licenses
- functional tests
- lint
- unit
- performance tests (on component level)

To achieve the goal, we have a wide range of tools available:

- [License Scout](https://github.com/chef/license_scout)
- [Unit tests for Golang](https://golang.org/pkg/testing/)
- [Karma](https://karma-runner.github.io/1.0/index.html)
- [Go Lint](https://github.com/golang/lint)
- [Go Vet](https://golang.org/cmd/vet/)
- [InSpec](https://www.inspec.io/)
- [Testify](https://github.com/stretchr/testify)
- [chakram](http://dareid.github.io/chakram/)
- [JMeter](http://jmeter.apache.org/)

The list of tools is not exclusive and is only mentioned to provide some examples about tools we already use.

**Example: Authentication**

Our authentication framework Dex (OpenID Connect) needs to verify that it works with SAML, OIDC or LDAP/AD. Since this project is an open source project, we want to ensure that all supported cases by Dex have their tests located in its [repository](https://github.com/coreos/dex).

### Acceptance

The acceptance environment is for testing interactions between services, not to test services individually. At this level we expect that a component has been tested already. Therefore we can focus on the following tests:

- integration testing
- migration testing
- regression testing
- performance tests (on integration level)

To achieve the goal, we have some tools already in use:

- [InSpec](https://www.inspec.io/)
- [JMeter](http://jmeter.apache.org/)

This is a category where we need to get stronger at. Historically, we focussed more on unit testing.

**Example: Authentication**

We already know, that Dex is able to login via LDAP and SAML. Therefore we focus on the service interactions. A sample test would look like: Can we use a token from Dex to request data from our reporting api?

## Pipeline

The below diagram outlines how changes flow through the A2 pipeline.

![A2 Change Flow](./diagrams/a2-change-flow.png)
