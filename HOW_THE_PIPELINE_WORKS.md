# Automate 2 Build and Test Pipeline

This document describes the testing and build infrastructure for the
Automate repository. The goal of this document is to provide a useful
starting point for developers of Automate trying to solve a pipeline
issue.

## General Pipeline Shape and Systems

The following is a high-level outline of the various stages of our
build and test pipeline.  It is not complete but lists the major
steps. For a more complete diagram see RELEASE_PROCESS.md:

```text
                          +----------------+
       +----------+       | Buildkite      |       +-----------+       +-----------+      +----------+    +-------------+     +---------------+
       | Pull     |       |   Verify       |       |   Pull    |       | User and  |      | Habitat  |    | Packages    |     | deploy/dev    |
       |  Request +------>------------------------>+  Request  +------>+ API docs  +----->| Packages +--->+ and manifest|-----+ pipline runs  |
       |   Opened |       |  Buildkite     |       |   Merged  |       | build     |      | Build    |    | upload      | |   |               |
       +----------+       |    Verify      |       +-----------+       +-----------+      +----------+    +-------------+ |   +---------------+
                          |      Private   |            |                                                                 |
                          +----------------+            |                                                             +---V--------------+
                                                        v                                                             |automate.chef.io  |
                                                   +--------------+                                                   |pipeline runs     |
                                                   | Buildkite    |                                                   +------------------+
                                                   | runs again   |
                                                   | from master  |
                                                   +--------------+
```

The build and test pipline is composed of a few key parts:

- [Buildkite](https://buildkite.com/chef): A hosted service similar to
  TravisCI. We have a number of "Buildkite pipelines" that are
  triggered at different points in our release process. Most
  importantly, the pre-merge verification tests and post merge deploy
  tests are Buildkite pipelines.

- Buildkite Agents: While Buildkite is a hosted service, Buildkite jobs
  run on agents that are hosted in our infrastructure.

- EC2 Hosts: The Buildkite Agents run on EC2 instances that are
  started as part of AWS Autoscaling groups. Multiple agents run on a
  single host to make better use of system resources.

- [Expeditor](https://expeditor.chef.io): Expeditor sits at the center
  of our build and test process. Expeditor is in-house software
  developed by releng that manages the release of our software. We
  define post-merge and post-promote actions in our Expeditor config
  (`.expeditor/config.yml`) and Expeditor runs the actions at the
  appropriate times. Expeditor also allows us to respond to other
  events such as changes in upstream software.

## Verify and Verify-Private Buildkite Pipelines

Our "pre-merge verification tests" are Buildkite pipelines named
`verify` and `verify-private`. These are the tests that show up in
GitHub PRs.

We have 2 pipelines because some tests need access to Chef-only
resources. `verify-private` has access to such resources.

These pipeline runs different sets of tests, and both pipelines must
past before merging a pull request.

Each pipeline is defined in a configuration file:

- `verify`: `.expeditor/verify.pipeline.yml`
- `verify-private`: `.expeditor/verify_private.pipeline.yml`

The core of these configuration files are the "steps" (we sometimes
call these "jobs"). Each step runs on a different buildkite-agent and
defines a test that we would like to run.

Jobs can be separated by a special step called `- wait` which waits
for all preceding steps to complete before continuing. We sometimes
refer to each side of these `wait` steps as different "phases".

### Verify

The verify pipeline has unit tests and a few integration tests. We use
"unit tests" to mean anything that can run without us needing to build
and start actual Automate Habitat packages.

These tests comes in two different flavors:

#### Studio Tests

```yaml
  - label: "[unit] secrets-service"
    command:
      - hab studio run "source .studiorc && go_component_unit secrets-service && go_component_static_tests secrets-service"
    timeout_in_minutes: 10
    retry:
      automatic:
        limit: 1
    expeditor:
      executor:
        docker:
          privileged: true
          environment:
            - HAB_STUDIO_SUP=false
            - HAB_NONINTERACTIVE=true
```

To understand these tests, you need to read the related studio
functions. The dev studio is defined in a series of shell functions in
`.studiorc` and the `.studio` directory.

#### "Makefile" tests

```yaml
  - label: "[unit] automate lib"
    command:
      - scripts/install_golang
      - cd lib
      - make lint unit
    timeout_in_minutes: 10
    retry:
      automatic:
        limit: 1
    expeditor:
      executor:
        docker:
```

To understand these tests you need to read the Makefile in the
directory that the test `cd`s into.

### Verify Private

The verify private pipeline is currently where the majority of our
integration tests run. In the first phase of this pipeline, we build
all of the packages that changed as part of the pull request. This
happens in the build step:

```yaml
  - label: build
    command:
      - scripts/verify_build
    timeout_in_minutes: 45
    env:
      ALLOW_LOCAL_PACKAGES: true
      HAB_STUDIO_SUP: false
      HAB_NONINTERACTIVE: true
    expeditor:
      executor:
        linux:
          privileged: true
```

The build step runs `scripts/verify_build` which uses package
dependency information combined with the git diff to decide what to
build. It also has special case logic to build more packages when the
`dev` Habitat depot channel is out of date since any packages we don't
build in this step are taken from `dev` during most integration tests.

Tests that run after the `- wait` step in this pipeline can access the
packages built during the `build` step by downloading and unpacking
them using the `buildkite-agent artifact` command.

The tests in this pipeline come in two primary flavors:

- *Studio Tests*

These tests use the Habitat dev studio. They look like this:

```yaml
  - label: "[integration] ingest-service"
    command:
      - . scripts/verify_setup
      - hab studio run "source scripts/verify_studio_init && start_deployment_service && chef-automate dev deploy-some chef/ingest-service --with-deps && ingest_integration"
    timeout_in_minutes: 20
    retry:
      automatic:
        limit: 1
    expeditor:
      executor:
        docker:
          privileged: true
          environment:
            - HAB_STUDIO_SUP=false
            - HAB_NONINTERACTIVE=true
            - REST_SERVICE=https://localhost:10122
```

To understand these tests, read the Studio function they are calling
and then any related code that those functions may call.

- *Integration Test Framework*

These tests use our integration test framework. They look like this:

```yaml
  - label: "[integration] deep upgrades"
    command:
      - integration/run_test integration/tests/deep_upgrade.sh
    timeout_in_minutes: 20
    expeditor:
      executor:
        linux:
          privileged: true
```

These tests run against a running A2 instance. To understand these
tests, read the test definition that is being run (in the above case
`integration/tests/deep_upgrade`). For more information about the
integration test framework see: `integration/README.md`

### OK, but, where are the tests?

We use a variety of testing tools. Here are the most common ones you
are likely to encounter:

- Go tests: Most of our Go projects write a portion of their tests
  using stanard Go testing tooling. Most of the "unit tests" for go
  services are written in this style.  The output of these kind of
  tests typically looks something like:

- InSpec tests: Some tests are written using InSpec and can be found
  in the `inspec` folder. Some of the "Integration Test
  Framework"-style tests run these tests as do our post-build
  deploy/dev pipeline jobs.

- Diagnostics tests: Some tests are written in our "diagnostics"
  framework. These can be found in
  `components/automate-cli/pkg/diagnostics/integration`.

  These tests are written in Go. The diagnostics tests allows you to
  separate test state creation from test assertions, which means we
  can create test data, run a backup and restore, and then run the
  test assertion as a validation of our backup and restore
  process. Many of the "Integration Test Framework" tests run these
  tests.

- Ruby GRPC tests: The compliance service tests its GRPC API via
  Ruby-based that can be found in `components/compliance-service/api/tests`

- Shell scripts: Some tests are written in shell in
  `integration/helpers` and `integration/tests`.

- More shell scripts: The "studio based integration tests" typically
  call shell functions defined in `.studio/`. These tests then
  typically call either Go tests or custom shell functions.

- Cypress tests: [cypress](https://www.cypress.io/) is an end to end testing framework
  that we are currently using for UI e2e tests as well as some API integration testing.

### Debugging Tips and Tricks for Buildkite

#### Accessing Logs

The Buildkite job output shows anything written to standard output or
standard error during the job. This can often be hard to read in the
web interface. There is a "download" button for the output that you
can use to inspect it locally.

For many of the integration tests, you may also need the systemd or
Habitat supervisor logs. The tests using the integration framework and
many of the studio tests try to upload these logs as Buildkite
artifacts. They are available in the "Artifacts" tab.

#### Iterating on failing tests

All of the tests can be run locally. For most cases, to run the test
locally, you can run same commands listed in the Expeditor
configuration for the job.

Some problems, however, have to do with the specifics of the Buildkite
environment itself. In such cases, often the most expedient path is to
use Buildkite itself to iterate on a build problem by pushing a pull
request with additional debugging output. In this case, you can speed
up your test iteration by commenting out all of the steps but the one
you are working on. If the failure you are investigating is in the
verify_private pipeline, you'll likely need to leave the `build` and
`wait` steps un-commented as well.

#### Access to the underlying host [Chef employees only]

Occasionally you may find you need information about the underlying
buildkite-agents and their hosts to understand a problem. We restrict
access to the underlying buildkite-agents. However, you can:

- Ask questions in #a2-team, many members of the A2 team can likely
  help get you the information you need.

- Ask questions in #releng-support. Release engineering deals with
  many different pipelines, so it is helpful to provide as much
  information as possible including at least a link to the Buildkite
  job you are debugging.

- Log into the AWS chef-cd account using via Okta. You can then get
  high level information from AWS about the underlying AWS
  machines. The underlying AWS instance ID can be found in the
  "Environment" tab of a given step. From here you can get high level
  information like monitoring data on the machine in question.

## Post-Merge build pipeline

After merge, Expeditor will trigger the 'habitat/build' pipeline:

https://buildkite.com/chef/chef-automate-master-habitat-build

This pipeline builds any packages that have changed in the most recent
commit. The buildkite pipeline definition itself is autogenerated
using the data found in `.bldr.toml`. `.bldr.toml` is generated by

```shell
    go run tools/bldr-config-gen
```

These packages are then be uploaded to the Habitat depot and promoted
into the dev channel.

We also publish a "manifest". The manifest is a list of all the
packages that compose "Automate" and their required version. This
manifest is used by the deployment-service to install and upgrade
Automate.

The manifest is created using the `.expeditor/create-manifest.rb`
script.

Expeditor posts the outcome of this build and upload process into
`#a2-notify`. In the case of failure, Expeditor will
include a link to the log file that contains the full output of the
action that failed.

## Post-Build deploy/dev pipeline

After the build is complete, Expeditor will trigger the deploy/dev
pipeline. This pipeline runs another set of tests on the newly
promoted set of packages. These tests are a combination of new
installations and upgrades of long-lived environments under different
configurations.

These tests are defined by terraform configuration in

`terraform/test-environments/scenarios`

### Debugging deploy/dev test failures

These tests report their failures in Buildkite, but the Buildkite
output is the output of the `terraform apply`.  In many cases this
simply shows a deploy timing out or a test failing to run and you will
need to log into the machine.  The hostnames of each of the various
environments can be seen from the dashboard

http://a2-dev.cd.chef.co/

Chef employees can log into those machines using their chef username
and ssh key and do any investigation required to debug the issue.

## Nightly Pipelines

In addition to the pipelines described above, we have a pipeline that
is run nightly. Failures in this pipelines do not block merges or
deploys; however, failures here are still taken seriously and
investigated.

Our nightly pipeline is defined in `.expeditor/nightly.pipeline.yml`

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

## Ancillary Monitoring

Hand-in-hand with pipeline monitoring is monitoring of other crucial aspects of the health of the codebase.

### Dependabot

GitHub provides built-in support for Dependabot security updates.
Prerequisites to have a repository scanned are detailed [here](https://docs.github.com/en/github/managing-security-vulnerabilities/configuring-dependabot-security-updates#supported-repositories).
The Chef Automate repository satisfies the prerequisites and has Dependabot updates enabled; configuration settings are available [here](https://github.com/chef/automate/settings/security_analysis), though only admins have access to that link.
You can also view [past alerts](https://github.com/chef/automate/security/dependabot) on the dashboard as well as [current and past PRs](https://github.com/chef/automate/pulls?q=is%3Apr+dependabot+) automatically generated by Dependabot.

Periodically you will see pull requests automatically generated by Dependabot--these need to be reviewed and merged like any other PR, on a timely basis.

### Semgrep

Semgrep (short for _Semantic Grep_) is a static code analyzer for both security issues as well as best practice issues.
This is integrated into the Chef Automate workflows and runs:
(a) a differential scan for every PR when opened (a task in the [verify_private pipeline](https://buildkite.com/chef/chef-automate-master-verify-private))
(b) a differential scan for every PR when merged (same task)
(c) a full code base scan every day (a task in the [nightly pipeline](https://buildkite.com/chef/chef-automate-master-nightly))

Semgrep findings are detailed in the output log of those Buildkite tasks every time it runs.
Most significantly, any new findings generate slack notifications in the `#a2-notify` channel, so the channel should be monitored closely for any such reports.
Semgrep also provides a [dashboard](https://semgrep.dev/manage/findings?repo=chef%2Fautomate-nightly&ref_type=branch&ref=master&state_type=new&tab=findings) where you can see not only all current issues on master, but trends and past history on all branches.
Note that, at the time of writing there are about 21 open findings; issues that need to be resolved. But it would be counter-productive to generate slack notifications for each of those every day--no one would pay any attention. That is why notifications occur only for new findings.

Semgrep is available (through several make file entries) to be run locally as well, either on the whole code base or on individual components within the code base.
More details on this will be documented in the near future.
