# Metadata Collection for Policyfile Change Rollouts

Created: June 26, 2020

**Problem Statement:**

We are building a feature in Chef Automate to track Chef Infra code changes in
order to provide users with visibility into which systems have run which
versions of Chef Infra code and to help users detect and respond to any defects
in their Chef Infra code.

This feature is based on collecting metadata about the code when the user
pushes a new version of the code to their distribution system (e.g., Chef
Server). This metadata is then matched with corresponding Chef Infra Client run
data to give the user information about the impact of the new code.

We believe that the change tracking feature will be much more usable and
valuable if we collect more metadata about new code changes than our existing
systems collect today. Some examples:
* Collect a description of the change from the user initiating the change. This
  would enable the system to display meaningful information about the intent of
  the code change instead of only displaying machine generated identifiers or
  diffs.
* Link the code change to the user's source control system and source code
  host. The system could create links to commits, pull requests, code diffs,
  etc. This would help users to efficiently locate code defects.
* Link the code change to the user's Ci/CD system. These systems are often
  responsible for coordinating changes among multiple systems and may host data
  about which humans initiated or approved changes.

# Solution Goals:

* Make it easy for users to understand what their changes are "at a glance"
* Make it easy for users to investigate failures in new code changes
* Try very hard not to drop new changes.
* Integrate into existing workflows as best we can. Try not to introduce extra
  steps for users.
* Supporting other Chef Infra use-cases, including architectures without Chef
  Server, should be possible without fully re-architecting. It's ok if some
  enabling integration code must be written to support these use cases.

# Proposed Solution

Automate learns about new Chef Infra code changes via two main sources. One is
a notification-type event from the primary content store, and the other is from
a client-side metadata collector.

The event stream from the primary content store gives us basic information
about a code change, but does not include most of the metadata we wish to make
available. What we will implement for now is code in Chef Automate that will
create rollout records in response to Policyfile action events. In the future,
if we wish to enable change tracking for Effortless architectures, we use
a notification capability in the depot or implement a poller.

In order to get rich metadata about code changes, we create a client-side
metadata collector. This will extract data from the SCM system, Ci/CD system
(if applicable), and the user then forward it to Automate. This will be
implemented as a separate CLI tool that the primary CLI will call as a hook.

## User Interactions with Proposed Solution:

**Installation/Setup**

The metadata collector is downloaded from Chef Automate. We provide a stable
URL and also links in the UI. It's single-file go binary. From the downloads
folder, the user can put it in PATH (/usr/local/bin) or `~/.chef/bin`.

The user configures authentication credentials for each Automate server they
use on an OS-user basis. E.g, a file in `~/.chef`. Ideally we provide the
ability to download a correct file with a minimally-permissioned token in the
UI.

(Maybe) For each policy repo, the user adds a configuration to make the change
tracking hook required. This has the automate URL in it. We also respect it if
this config is in `~/.chef`

When either config is present, the `chef` bin will automatically execute the
hook upon execution of `chef push`. Presence of the hook will enable the
command line option to supply a description for each `chef push` invocation.

**Day-to-Day: Upload a Change to A Chef Server from a Workstation**

Several options: 
- The user has configured the metadata collector to use the last git commit as
  the description; the user types `chef push policies/Thing.rb production` as
  they do now. No extra effort.
- The user types `chef push policies/Thing.rb production --description "foo"`
- The user types `chef push policies/Thing.rb production` and edits a message
  in their text editor.

**Metadata Collector Upgrades**

Use the same auto-updater capability we have in the `chef-automate` CLI. If
auto-update is disabled in some way, the user repeats the install steps
manually. We can give them a nag to do it when we run the auto-update check.

**If the Metadata Collector Is Not Installed**

E.g., this is what happens when a user gets a new laptop and forgets to install
the metadata collector hook, or a new person joins their team and starts
working without setting up the collector.

If we have some part of the metadata collector configuration live in the users'
git repos, then the config will be present when they work on the repo, so the
chef bin can detect this case and return an error when attempting a push.

**How Effortless Would Work**

The interactions affected are:
- `hab pkg upload`
- `hab pkg promote`

If users upload from the `results` directory, we may be able to traverse paths
to in-repo config, but all bets are off for `hab pkg promote`. This means we
should expect machine-user-account-based config for habitat/effortless.

# Solution Alternatives Considered

## Chef Server Actions Pipeline Only

If we register changes for tracking only from the Chef Server actions pipeline
in Automate with the least modifications possible and don't implement
a metadata collector CLI:

This option doesn't allow for adding the metadata we think is required to
provide context to a code change. If this is the only way record a code change
for tracking, then other Chef Infra architectures are excluded as use-cases.

## Include Metadata In the Policyfile Lock JSON

If we put some or all of the metadata we want into the lock JSON file:

The git metadata would fit quite nicely in the lock JSON file, but Ci/CD
metadata would not make sense there (if the lockfile is used by Ci/CD but not
produced there). A description also would not make sense there: we expect the
user to often make multiple builds of the lock JSON between pushes, and likely
to push to dev several times for each production push; the context for a push
is relative to the code being replaced.

Unfortunately, though the git data would make a good conceptual fit, it will be
really annoying to anyone who checks their lock file into git. Even with some
special case logic, you need to commit twice for this to work. For example, you
update a cookbook, this changes the lockfile. You commit this change in commit
A. Now you need to record that the last commit to change the lock file was
commit A... in the lock file. So now you have a changed lock file and need to
commit that (commit B).

There's ways around this but it's getting pretty complicated for not a ton of
benefit.

## Implement the Metadata Collector in the `chef` bin

If we wrote the metadata collector in the `chef` binary:

This is good because there's no additional installation step. The configuration
steps could all be the same as I've described so far. All the user interactions
described thus far depend on changes to the `chef` bin, thus people cannot use
them until they upgrade to whatever version we ship with hooks support.

Downsides of doing this:
- after the initial release, it would be nicer if any new features that rely on
  an updated metadata collector can just ship when the relevant version of
  Automate ships
- It's easier to integration test: 
  - run CLI that ships with Automate against Automate API
  - we can make a local/test mode for the metadata collector for Chef bin to
    test with
- effortless workflows use the `hab` command, it's nicer if `hab` can implement
  a hook instead of reimplementing the same metadata collector


