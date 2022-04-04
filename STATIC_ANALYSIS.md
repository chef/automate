# Static Analysis

Besides all the various linters for Go, TypeScript, and other languages included in system builds and in VSCode via plugins,
we also have dedicated static analysis tools.

## Semgrep

[Semgrep](https://semgrep.dev/) is a fast, lightweight static analysis tool focussed on securing your code base, eliminating bugs, and enforcing code standards.
It is more flexible to use than its competitors: run on the command-line, in Buildkite, and in the VSCode editor.
It applies a large set of standard rules out-of-the box but, unlike competitors, also makes adding custom rules easy, with a shallow learning curve and NO arcane syntax (search patterns look just like your code).
It provides an intuitive dashboard useful to both managers looking for trends as well as to engineers looking into the details, letting you drill down from findings directly into relevant lines in your source code.
Semgrep was created by [r2c](https://r2c.dev/), a lean and relatively new startup in the field.
But Semgrep has evolved quickly; it doesn't just hold its own against competitors, it surpasses them:
take a look at this [competitive analysis](https://docs.google.com/spreadsheets/d/1Cjn5JQflRzO3damhcVOWg1SOtKoJ3BeSDut3aW1rj4U/edit?usp=sharing) done in February 2021 by Michael Sorens.

### Concepts of Semgrep in Two Minutes

Want to find where the string "Automate" appears in our code base?
Run `grep Automate` (grep is the standard Linux tool for file searching) and you will get each line of code containing the string.
Ah, but what if you want to find "Chef Automate" where there may or may not be a space embedded?
Instead of a string, pass a regular expression: run `grep 'Chef ?Automate'` (notice the question mark in there that looks for an optional space).
But grep -- and any other such search tool you care to try -- works on one line at a time.
If "Chef" is at the end of a line and "Automate" at the beginning of the next, you need Semgrep.
Semgrep (short for *semantic grep*) cares about what you care about: semantics.
Do you care if there is a carriage return instead of a space between "Chef" and "Automate" when you want to see where it occurs?
Of course not.
Neither does Semgrep.

And while you can use Semgrep to search for plain text like "Chef Automate", it was designed for looking at your code.
You can, for example, search for functions that take four arguments.
Or the use of a certain function call that should always be after another function call (perhaps a `BeginTransaction` and sometime later an `EndTransaction`).
It is natural, given what has been stated thusfar, to assume that we are still talking about looking for pieces of code literally.
But this is where Semgrep really shines -- it does not match token for token (a "token" being each thing in your code: variable, operator, statement terminator, etc.).
It truly matches by semantics.

Consider that in TypeScript, there are two ways to define a function.
In the code fragment shown, we define first an anonymous function and assign it a name.
The second function, using the more traditional style, defines the function with a name, then its body.
But notice that at the end of the code fragment, both are called identically because semantically they are equivalent.
Just to spice it up even further, in the first function we have type specifications for the `a` and `b` parameters, but the second function omits those type specifications entirely.

```TypeScript
    AnonymousFunc =
      (a: string, b: int) =>  { return foo(a, b); };

    NamedFunc(p1, p2) {
      return p1 + p2;
    }

    AnonymousFunc('abc', 12);
    NamedFunc('abc', 12);
```

Now here is where the magic happen.
We ask Semgrep to search for a function that takes two parameters and contains a single line that returns an expression.
The pattern is simply this:

```TypeScript
$FUNC($PARAM1, $PARAM2) { return $EXPR }
```

Semgrep will match the definitions of both `AnonymousFunc` and `NamedFunc` because they are semantically equivalent.

But wait!
Look at this anonymous function:

```TypeScript
    OtherFunc =
      (a: string, b: int) =>  foo(a, b);
```

That function does not even use the `return` keyword at all--but it is still semantically equivalent TypeScript.
So that SAME Semgrep pattern above will match this function, too!

Don't believe it? Or just from Missouri (the "show me" state)?
See it for yourself in the [Semgrep playground](https://semgrep.dev/s/msorens:match-function-variants).

> The preceding brief introduction to Semgrep is just to provide an appreciation of how Semgrep works.
> You really only need to know about such details if you will write your own custom rules.
> But it is presented here to instill confidence that Semgrep rules--whether you use predefined rules or write your own--will Do The Right Thing (TM) and will not inadvertently ignore crucial code blocks due to cosmetic syntactic variations.

### The Semgrep Dashboard

The [Semgrep dashboard](https://semgrep.dev/manage/) is your window into both viewing results and defining CI configurations.
You will need to login with your Chef credentials.
Once logged in, you connect to the "chef" organization for access: look for the `(add organization)` dropdown next to your profile icon in the upper right of your screen.
That will take you to GitHub where it should list your own personal GitHub organization and (ideally) the Chef organization.
Select "chef" itself (rather than the "configure" link) and that should reveal the dashboard to you.

> Caveat: At the time of writing, the Semgrep dashboard is missing one important element: configurable access levels.
> Whoever logs in has *full access* to modify any settings.
> Which you probably do not want to do.
> Fear not, however, since it is unlikely you will change something accidentally: you have to modify something then deliberately select Save.
> The dashboard should soon have better access control. though.

### Rulesets and Policies

Normally, we use out-of-the-box rulesets provided in the [Semgrep rule repository](https://semgrep.dev/explore).
Since Automate primarily uses Go and TypeScript, we want to include all the rulesets that cover those languages.

Once you are on the [Semgrep dashboard](https://semgrep.dev/manage/) select the `Policies` tab and you will see, at a minimum, these policies:

* Chef-01 -- used for each PR
* Chef-nightly -- used by the nightly pipeline

Selecting either of them displays the details of that policy, which includes a few bookkeepping items and, notably, a list of all the rulesets included in the policy.
You can add more rules from that page via the `Add` button, or if you navigate to the `Rules` tab, you will find an `Add to Policy` button on rules and rulesets.

A single ruleset may contain any number of rules. The ones included for Automate range from 10 or so up to 100 or so per ruleset.
Notice that some of the rulesets in each policy include disabled rules.
Those have been disabled either because they are not appropriate for our code base or, for some, because they do return dubious results.
Rulesets do not just come from Semgrep; anyone can contribute rulesets.
Most (possibly all) rulesets from Semgrep itself begin with the company name as a prefix: `r2c`.
But you will also see, for instance, the `dgryski.semgrep-go` ruleset, which is a good size set of Go rules contributed by a user.
However, a few of those rules were excluded because they do not work very well in our environment.

(One other nice-to-have feature may come along in the near future: being able to add a note when excluding a rule so you know *why* for posterity.
Right now, there is no way to know unless you try it out.)

### PRs vs. Nightly Checks

The previous section noted there were different policies used for pull requests and for the nightly pipeline.

#### Pull Requests

When one posts a pull request, Buildkite includes among its many tasks, a Semgrep scan using the *differential* mode; it scans only the files changed in the PR rather than the entire code base.
This makes the scan very quick.
But an even more important benefit: it lets you introduce new rules any time you like without having to worry about how many findings it will trigger in the entire code base.
You can then, as time allows to address that technical debt, proceed at your leisure to review any such findings in the rest of the code base.

The `Chef-01` policy specifies all the rulesets, with exceptions, that are used for this differential scan.
The policy is attached to a project, which is essentially the name of a GitHub repository, in this case `automate`.
Navigate to the `Projects` tab in the dashboard, filter by `automate` and scroll/page until you find `chef/automate`.
There you will see the `Chef-01` policy attached, with info on the latest number of findings.

#### Nightly Pipeline

There are a variety of checks in the nightly pipeline, including a *full* scan by Semgrep (in contrast with PRs, where Semgrep does a *differential* scan).
This full scan is just a way to keep tabs on any on-going technical debt.

In order to reduce noise in the output, however, this nightly scan uses a fictitious repository name for the sole purpose of attaching a different Semgrep policy.
The [Chef-nightly policy](https://semgrep.dev/manage/policy/chef-nightly) is a strict subset of [Chef-01 policy](https://semgrep.dev/manage/policy/chef-01): it includes all the same rulesets but excludes a few additional rules in order to generate less noisy, and therefore more useful, output.
It ignores some legacy findings that are of no consequence and do not need to be addressed.
All remaining findings being reporting warrant either fixing or marking explicitly to ignore; this tech debt should be handled promptly!

### Viewing Results

#### High Level

From the Semgrep dashboard, you can peruse the [Overview](https://semgrep.dev/manage/overview) or the [Analytics](https://semgrep.dev/manage/findings) views.
The Analytics view is much more helpful because you can filter projects, policies, and branches.
With the [unfiltered view](https://semgrep.dev/manage/findings) you will see an aggregate of all projects, policies, and branches.
But a better view is this [filtered view](https://semgrep.dev/manage/findings?repo=chef%2Fautomate-nightly&ref_type=branch&ref=master) showing just the `Chef-nightly` run on master.
The reason is that a single finding might, on occasion, be visible in several PRs plus the nightly scan.
And if you are viewing results for all branches, the totals will be inflated because each duplicate counts.
So by filtering to just master and just the nightly scan, there are no duplicates.

That Analytics view has two different tabs. `Summary` is the default view showing counts over time.
But if you select `Findings` you then get a navigable list of findings with links to go to the flagged rule and to go to the errant line of code itself.

The Analytics view also, at the bottom, provides graphical and tabular information about trends in findings,
detailing which rules are flagged the most, and notably how they are handled.
Are they fixed or are they muted?
One can choose to mute (ignore) items to prevent them from being flagged by subsequent Semgrep scans.

One other set of data of interest: what rules are there per language?
The simplest way to see that is to go to the [Rules view](https://semgrep.dev/r) under the `Rules` tab, then unselect all languages, and reselect the language of interest.

#### Low Level

You can always see the raw results in Buildkite, for either an individual PR (the `Custom` and `Security` tasks in the `verify_private` pipeline) or the nightly run (the `Semgrep` task in the `nightly` pipeline).

New findings are also reported in the `#a2-notify` channel in Slack.
Pay attention to the word "new" there.
To reduce noise, Semgrep only notifies in Slack on findings that are actually new.
This is great at eliminating noise.
Consider that, at the time of writing, there are about 20 or so legacy findings that show up in every nightly scan.
But these were only reported in Slack once, the first time they were seen.
They do not continue to show up day after day there.

There are several ways to see raw results in Buildkite:

1. navigate through the PR through the verify pipeline
2. select the project link from the project page on the Semgrep dashboard
3. select the link in the Slack notification

See the exploded illustrations in [this PR](https://github.com/chef/automate/pull/4618) for details on (2) and (3).

#### Muting Findings

If circumstances reveal that a particular finding is not a problem at that specific point in our code, a developer may elect to mute (ignore) it.
That is done by annotating the line of code with `# nosemgrep: <rule-name>`.
Alternately, if there are multiple findings on a single line of code and all of them should be ignored (which is common), drop the rule name argument, so it would just be `# nosemgrep`.
Note that the first token in each of those is the comment-to-end-of-line token appropriate for the language.
Thus, those examples would work for Go.
TypeScript, for instance, would use `// nosemgrep: <rule-name>` and `// nosemgrep`, respectively.
Search the Automate codebase for the few muted items.
And note that there are very few--try very hard not to mute items to avoid the technical debt!
Only mute items that are actually safe to be ignored permanently.

### How Semgrep Runs

Pull requests in Automate (and perhaps most other Chef repositories) are set up so that there are two primary, top-level
check suites: `buildkite/chef-automate-master-verify-private` and `buildkite/chef-automate-master-verify`.
Each of those is a suite of Buildkite tasks; that is where all the unit tests, linters, integration tests, setup/teardown tests, etc., are exercised.
So among that suite of Buildkite tasks seemed the most appropriate place to add Semgrep scans as well.
Note that Semgrep could easily be integrated with GitHub actions--that would then make it a top-level task, which is OK, but a slightly different convention.
(Actually, it would be simpler to connect via GitHub actions, as those are fully supported by Semgrep's web UI, but we have it working in Buildkite now, so all is well.)

For PRs, Semgrep needs to run in the private pipeline (`buildkite/chef-automate-master-verify-private`) only because it needs to use secrets.

There are two Semgrep tasks, `Custom` and `Security`, for PRs.
The latter (`Security`) uses the `Chef-01` policy discussed earlier, which is almost equivalent (again, as described earlier) to the `Semgrep` task in the `nightly` pipeline, that uses the `Chef-nightly` policy (yep, discussed earlier).

The other task (`Custom`), by contrast, runs a set of custom Semgrep rules that are included in the `.semgrep` directory of the Automate repository.
There are only a dozen or so rules there, mostly to cover best practices with the way we manage rxjs in the front-end.
Those results are not wired, and do not show up, on the Semgrep dashboard.

### When Semgrep Runs

There are three ways to trigger a Semgrep scan.

* Trigger a build manually in Buildkite via the `New Build` button:

```text
  manual build on 'ms/semgrep-base-adjust' branch;
      using merge-base of master as base (22545b9d39d4a575e21e7c738a63076fb178cff6)
```

* Trigger a build by opening a PR or pushing to an open PR (like anything else that triggers Buildkite):

```text
  PR build on 'ms/semgrep-base-adjust' branch;
      base is 22545b9d39d4a575e21e7c738a63076fb178cff6 (merge-base of 'master')
```

* Trigger a build by merging to master (again, standard Buildkite behavior):

```text
  build on master; using 'master~1' as base branch
```

See the exploded illustrations in [this PR](https://github.com/chef/automate/pull/4597) for details.

The reason those are mentioned here is so you are aware of the hooks for differential scans.
The base for a differential scan needs to adjust depending upon which of the three methods above are used.
The goal, though is the same: in each case we want to look at only the files that are on your branch.

### Semgrep Findings and Buildkite

On the `Policies` tab in the Semgrep dashboard you can direct Semgrep to fail your build or not when it has findings to report.
Just by ticking/unticking the "Blocking?" checkbox, any projects using the given policy will honor that setting.
If you examine the two policies in use `Chef-01` for PRs and `Chef-nightly` for the nightly scan, both have that box ticked.
That makes sense, because if there are findings, of course we want the build to fail, right?

Well, for PRs, the answer is unequivocally yes.
If a developer adds or modifies code that causes a finding we do not want that merged to master.

But how about for the nightly scan?
Remember, the nightly scan is where legacy issues reside, things that we would fix immediately if things like priorities and deadlines did not interfere with keeping on top of all technical debt.
But in the real world, we are not going to fix every issue immediately.
So does that mean the "Blocking?" box should be unticked?
No!
If you look at any [nightly scan on master](https://buildkite.com/chef/chef-automate-master-nightly/builds?branch=master)
you will see the Semgrep task marked in red--meaning it failed--even when the build as a whole passed.
How is that possible, you wonder?
Well, that is where the power of Buildkite comes in.
Notice the little warning icon in the red Semgrep pill on that page?
That indicates that though the individual task failed, we treat it as a "soft fail", and do NOT fail the build as a whole.

So to sum up about the nightly scan, the preferred settings are to tell Semgrep to fail ("Blocking?" ticked) but tell Buildkite to let it slide ("soft fail").
If instead we had just unticked the "Blocking?" checkbox--which we could have done--then the Semgrep task would always appear green and there would thus be no reminder that there is tech debt to follow up on.

### Running Semgrep Locally

There are many variations available for running Semgrep locally.

From the root of the automate repository you have these `make` targets that will run against the entire repository:

* semgrep -- runs differential scan (same as is done in open PRs)
* semgrep-all -- runs full scan but filters out inconsequential (legacy) issues (same as nightly scan)
* semgrep-legacy -- same as `semgrep-all` but reveals legacy issues, too

Note that these targets all run Semgrep exactly the way Buildkite does, using a Docker container and using the .semgrepignore file for things to exclude.
Before you can use the above targets, though, you need to define `SEMGREP_TOKEN` and `SEMGREP_ID` environment variables; the values for these come from the `id` and `token` defined here in [Vault](https://vault.ps.chef.co/ui/vault/secrets/secret/show/semgrep?namespace=root).

* semgrep-test/`<ruleset-name>` -- useful for trying out new rulesets

For this last one, say you want to add a new ruleset to our suite, e.g. [r2c-bug-scan](https://semgrep.dev/p/r2c-bug-scan).
(This one is already added now.)
Before adding it to CI, it is a good idea to do a trial run and see how voluminous the findings are.
Do this locally with `make semgrep-test/rc2-bug-scan` on the command-line.

### Running Semgrep While Writing Code

For fast turnaround, Semgrep is available in the component Makefiles as well.
Just run `make semgrep` in any Go project or in the `automate-ui` project and it will run Semgrep locally,
approximating what you would see in CI.
It is approximate because it is fundamentally a different run.
CI uses the `.semgrepignore` file for the list of things to skip; running this command, on the other hand,
invokes Semgrep locally passing it an explicit list of things to ignore (and the list is different in Go projects vs. the `automate-ui` project).

Besides `make semgrep` there is also the handy `make semgrep-and-fix` command that will apply auto-fixes for those rules written to support it.

To summarize, the `make` targets at the component level are:

* semgrep -- runs full scan on component
* semgrep-and-fix -- runs full scan on component and applies any auto-fixes available

Last, and certainly not least, there is a VSCode plugin that runs Semgrep on the file you are editing whenever you save it.
It will underline any findings with a squiggle as well as report it in the Problems pane.
This provides a great user experience, notifying you just as you are writing the code if there is an issue that needs attention!
