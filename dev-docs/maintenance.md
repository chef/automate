# Maintaining Automate

The automate code base needs continual maintenance to remain robust and fresh.

Who owns this task? YOU.
Every engineer should own this; taking the initiative to either fix something or at least report something to those who can, when issues arise.

There are two main aspects to this:

- Proactive: keeping up with third-party library releases
- Reactive: addressing issues that come up due to system changes (which may include third-party libraries as well as changes in environments and so forth).

## Third-party Dependency Update Monitoring

Proactively one should monitor releases of third-party libraries and re-sync our code base with them as soon as scheduling allows.
Automate uses two main languages, Go and TypeScript, each having their own complex ecosystem.

### Backend Updates

There is no central place to check on all dependencies.
The following, though, lists some of the key Golang dependencies. One can set up a "watch" on each of these repositories to be automatically notified of new releases that can then be scheduled as technical debt items to be done in a timely fashion.

- https://github.com/golang/go/releases
- https://github.com/grpc/grpc-go/releases
- https://github.com/golang/protobuf/releases
- https://github.com/open-policy-agent/opa/releases
- https://github.com/grpc-ecosystem/grpc-gateway/releases
- https://github.com/golang/protobuf/protoc-gen-go/releases
- https://github.com/stretchr/testify/releases
- https://github.com/lib/pq/releases
- core/dex
- core/nginx

### Frontend Updates

One can quickly identify any outdated packages using `npm outdated` on the command-line (from the automate-ui directory).
There are some packages that we deliberately pin (in automate-ui/package.json) for compatibility reasons.
These are documented in the "Dependency Management" section of automate-ui/README.md.
Be sure to review and update that document whenever updating any dependencies.

Notable in front-end updates are two things:

- Angular -- follow the releases at https://update.angular.io/ and follow the methodology outlined in https://github.com/chef/automate/pull/3082.
- Node -- follow the release at https://nodejs.org/en/about/releases/ and follow the methodology outlined in https://github.com/chef/automate/pull/2579.

Separate from keeping an eye on version numbers becoming out-of-date are how either updating a package or NOT updating a package may still cause problems to appear.
Some such problems generate warnings rather than errors (when you run unit tests or lint, for example).
But warnings do not fail the build so often these would go unresolved or even completely unnoticed for quite some time.
We therefore added a mechanism to treat warnings as errors and thus fail the build on any warnings.
This is in place for running both unit tests and lint in the UI (look for instances of build.sh in components/automate-ui/package.json).
But, alas, there are other problems that are not even reported as warnings (deprecations and such), and do NOT cause the build to fail.
Because of that, it is necessary for certain tasks to be checked periodically, specifically `make lint`, `make unit`, and `make e2e`.
At the time of writing, all of these are reporting issues that need to be resolved:

```text
make lint:
  - CURRENT PROBLEM--does not fail build:
      TSLint's support is discontinued and we're deprecating its support in Angular CLI.
      To opt-in using the community driven ESLint builder, see: https://github.com/angular-eslint/angular-eslint#migrating-from-codelyzer-and-tslint.

make unit:
  - CURRENT PROBLEM--does not fail build:
      'karma-coverage-istanbul-reporter' usage has been deprecated since version 11.
      Please install 'karma-coverage' and update 'karma.conf.js.' For more info, see https://github.com/karma-runner/karma-coverage/blob/master/README.md

make e2e:
  - CURRENT PROBLEM--does not fail build:
      spawn Unknown system error -86
      References:
        https://stackoverflow.com/questions/65618558/osx-fix-selenium-chromedriver-launch-error-spawn-unknown-system-error-86-bad-cp
        https://github.com/angular/webdriver-manager/issues/476

```

## Pipeline Monitoring

There are a handful of processing pipelines that work together to achieve our continuous integration environment,
covering everything from validating a pull request to releasing code to customers.
These pipelines need to be monitored on a DAILY basis for any anomalies.

### Monitoring for FAILED Runs

There are two dashboards that show these pipelines in the browser.
The rightmost bar in each spark chart indicates the latest status.
Any that are red indicate a failure.
Some of these failures, though, are routine and require no action from someone monitoring them.
For example, the verify/private pipeline (https://buildkite.com/chef/chef-automate-master-verify-private) will report failing unit tests on a branch for an individual pull request, indicating only that the author has a bit more work to finish their PR.
But if that same pipeline fails on `master` that may indicate a problem elsewhere that needs remediation.
At the time of writing, only one pipeline is under "chef-oss"; all the rest are under "chef":

- Chef pipelines: https://buildkite.com/chef?filter=chef%2Fautomate%3A
- Chef OSS pipelines: https://buildkite.com/chef-oss?filter=chef%2Fautomate

### Monitoring for BLOCKED Runs

The dashboards mentioned above indicate the success or failure of each run; they do NOT indicate any pipelines that are blocked.
Thus, either you have to check builds one-by-one, or run the following shell function to check them all.
Run `pipelines --blocked` after sourcing scripts/pipeline_info.sh and satisfying its prerequisites.

Example output:

```json
{"branch":"Amol/project_and_roles_changes","created_at":"2021-01-28T09:38:49.345Z","state":"passed","pr":"4393","pipeline":"[chef/automate:master] verify_private","message":"Added Id testcase for policy.\n\nSigned-off-by: samshinde <ashinde@chef.io>"}
{"branch":"Himanshi/client_UI_changes","created_at":"2021-01-28T08:42:51.369Z","state":"passed","pr":"4661","pipeline":"[chef/automate:master] verify_private","message":"Addedd CSS changes in client details.\n\nSigned-off-by: Himanshi Chhabra <himanshi.chhabra@msystechnologies.com>"}
```

Occasionally you will see a false positive if someone resolved the blockage by just starting a new build instead of unblocking the original blocked build.
Example: https://buildkite.com/chef/chef-automate-master-verify-private/builds?branch=Himanshi/client_UI_changes

### Monitoring for STALE Runs

The dashboards mentioned above indicate the success or failure of each run; they do NOT indicate any pipelines that have stopped running.
Thus, either you have to check the date on each pipeline's last build one-by-one, or run the following shell function to check them all.
Run `pipelines --stale` after sourcing scripts/pipeline_info.sh and satisfying its prerequisites.
The output itemizes all pipelines that have not been run in the last day.
That does not, however, mean each has a problem; some pipelines are needed to run less frequently.

Example output:

```json
{"name":"[chef/automate:master] post-promote","createdAt":"2021-02-01T09:00:52.228Z"}
{"name":"[chef/automate:master] deploy/ui-library","createdAt":"2020-12-23T04:04:33.035Z"}
{"name":"[chef/automate:master] habitat/build","createdAt":"2021-02-01T19:46:53.664Z"}
{"name":"[chef/automate:master] deploy/acceptance","createdAt":"2021-01-30T16:58:23.482Z"}
```

`Post-promote` and `habitat/build`, for instance, only run after PRs that create a new deliverable.
This therefore excludes PRs that involve files that are just touching internal docs or build files, etc.
So if there are no PRs at all in a given day, or only PRs that update e.g. internal docs, then that pipeline will not run that day.
Another example: `deploy/acceptance` will only run on days that we promote code to acceptance.

So this list is a starting point of things you might need to check, compared to the whole list (run `pipelines` with no arguments).

### Monitoring Other Pipeline Issues

Some issues will show up in notifications the `#a2-notify` slack channel.

- Problems reported by Buildkite itself will show a "thumbs-down" indicator if there is a problem needing attention.
- Other problems, like those reported by Netlify, will require reading the text, e.g. "Deploy did not complete for chef-automate (deploy preview 4676)".
- Still others, like those from Semgrep, report only problems, so each one requires attention. (Well, in the Semgrep case, it depends: Semgrep reports issues on master, yes, but it also reports issues in open PRs; the latter are up to the developer to resolve).
