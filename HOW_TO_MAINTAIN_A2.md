# Maintaining Automate

The automate code base needs continual maintenance to remain robust and fresh.

Who owns this task? YOU.
Every engineer should own this; taking the initiative to either fix something or at least report something to those who can, when issues arise.

There are two main aspects to this:

- Proactive: keeping up with third-party library releases -- covered in this document.
- Reactive: addressing system or pipeline issues that come up due to system changes, library changes, environment changes, etc. -- this is covered in the separate  HOW_THE_PIPELINE_WORKS.md document.

## Third-party Dependency Update Monitoring

Proactively one should monitor releases of third-party libraries and re-sync our code base with them as soon as scheduling allows.
Automate uses two main languages, Go (back-end) and TypeScript (front-end), each having their own complex ecosystem.

### Back-end Updates

#### Back-end Dependencies

There is no central place to check on all dependencies.
The following, though, lists some of the key Golang dependencies--this is NOT a complete list by any mean.
One can set up a "watch" on each of these repositories to be automatically notified of new releases that can then be scheduled as technical debt items to be done in a timely fashion.

- https://github.com/golang/go/releases
- https://github.com/grpc/grpc-go/releases
- https://github.com/golang/protobuf/releases
- https://github.com/open-policy-agent/opa/releases
- https://github.com/grpc-ecosystem/grpc-gateway/releases
- https://github.com/golang/protobuf/protoc-gen-go/releases
- https://github.com/stretchr/testify/releases
- https://github.com/lib/pq/releases
- https://github.com/dexidp/dex/releases
- https://github.com/nginx/nginx/releases

Note that some of these may be pinned for various reasons; those reasons should be re-examined periodically to see if the issue requiring the pinning can be or has been resolved. Example: at the time of writing, `dex` is pinned at 2.19.0 as indicated here: https://github.com/chef/automate/blob/4b6d53641d3687bcf0f15303eed5b1dcd7eb251c/go.mod#L132-L133

#### Other Back-end Issues

Separate from keeping an eye on version numbers becoming out-of-date are how either updating a package or NOT updating a package may still cause problems to appear.
When you enter habitat studio, the .studiorc initialization script runs all the scripts in the .studio folder, so there is a lot of potential for issues to arise. The startup outputs a couple screenfuls of text when you start up and that text should be monitored for any errors or anomalies.
There was such an error fixed in [this PR](https://github.com/chef/automate/pull/4727/), as an example.

Similarly, when you run commands in habitat studio that generate a lot of output, you should review that output for any errors or anomalies.
One example: there was a deprecation warning fixed in [this PR](https://github.com/chef/automate/pull/4727/).

### Front-end Updates

#### Front-end Dependencies

All of the following occurs in the context of components/automate-ui.
One can quickly identify any outdated packages using `npm outdated` on the command-line (from the automate-ui directory).
There are some packages that we deliberately pin (in automate-ui/package.json) for compatibility reasons.
These are documented in the "Dependency Management" section of automate-ui/README.md.
Be sure to review and update that document whenever updating any dependencies.

Notable in front-end updates are two things:

- Angular -- follow the releases at https://update.angular.io/ and follow the methodology outlined in https://github.com/chef/automate/pull/3082.
- Node -- follow the release at https://nodejs.org/en/about/releases/ and follow the methodology outlined in https://github.com/chef/automate/pull/2579.

#### Other Front-end Issues

Separate from keeping an eye on version numbers becoming out-of-date are how either updating a package or NOT updating a package may still cause problems to appear.
Some such problems generate warnings rather than errors (when you run unit tests or lint, for example).
But warnings do not fail the build so often these would go unresolved or even completely unnoticed for quite some time.
We therefore added a mechanism to treat warnings as errors and thus fail the build on any warnings.
This is in place for running both unit tests and lint in the UI (look for instances of build.sh in components/automate-ui/package.json).
But, alas, there are other problems that are not even reported as warnings (deprecations and such), and do NOT cause the build to fail.
Because of that, it is necessary for certain tasks to be checked periodically, specifically `make lint`, `make unit`, and `make e2e`.
At the time of writing, all of these are reporting issues that need to be resolved:

Here is an example from running `make lint` ([issue 4700](https://github.com/chef/automate/issues/4700)):

```text
  - This does not fail the build:
      TSLint's support is discontinued and we're deprecating its support in Angular CLI.
      To opt-in using the community driven ESLint builder, see: https://github.com/angular-eslint/angular-eslint#migrating-from-codelyzer-and-tslint.
```

Here is an example from running `make unit` ([issue 4701](https://github.com/chef/automate/issues/4701)):

```text
  - This does not fail the build:
      'karma-coverage-istanbul-reporter' usage has been deprecated since version 11.
      Please install 'karma-coverage' and update 'karma.conf.js.' For more info, see https://github.com/karma-runner/karma-coverage/blob/master/README.md
```

Here is an example from running `make e2e` ([issue 4702](https://github.com/chef/automate/issues/4702)):

```text
  - This fails the build when run locally, but not in Buildkite:
      spawn Unknown system error -86
      References:
        https://stackoverflow.com/questions/65618558/osx-fix-selenium-chromedriver-launch-error-spawn-unknown-system-error-86-bad-cp
        https://github.com/angular/webdriver-manager/issues/476

```
