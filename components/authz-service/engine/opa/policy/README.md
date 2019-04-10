# OPA Authz Policy

This folder contains our OPA policy definitions.
For information on OPA see:

- [open-policy-agent on github](https://github.com/open-policy-agent/opa/)
- [OPA main site](https://www.openpolicyagent.org/)

## Terminology Caveat

When it comes to OPA, _its policies_ are different from _`authz-service`'s policies_:

1. The latter--ours--are the first class entities `authz-service` works with: documents containing a name, metadata, a subject, as well as set of (resource, action, effect) tuples (or "statements").
2. The former--OPA policies--are the static Rego-code documents driving policy decisions.

## Using the REPL

Load your rego and policies upon invocation of the OPA REPL utility.
[Download OPA](https://www.openpolicyagent.org/docs/get-started.html#prerequisites) or install it via homebrew (`brew install opa`).

You can also just install OPA in the studio and you should get the correct version:

```
hab pkg install --binlink core/opa
```

### REPL with V1

NB: Assumes running from this directory; just adjust all paths if you want to start in a different directory.

```opa
$ opa run -w authz.rego common.rego policies:../example/policies.json
OPA 0.9.2 (commit 9fbff4c3, built at 2018-09-24T16:12:26Z)

> data.authz.authorized
false
> data.authz.authorized with input as { "resource": "automate:nodes", "subjects": ["team:local:admins"], "action": "node:read" }
true
> data.authz.authorized with input as { "resource": "automate:nodes", "subjects": ["team:local:other"], "action": "node:read" }
false
>
```

Note that the `-w` switch in the command invocation above makes the REPL watch and reload the files as changes occur.
This mode makes it easy to iterate on editing OPA files.

To get more insights into the execution, use `trace` to toggle tracing, then run a command.
(Watch out--the output is voluminous!)

### REPL with V2

NB: Assumes running from this directory; just adjust all paths if you want to start in a different directory.
Note that the input still uses subjects as the field instead of members.

```opa
$ opa run -w authz_v2.rego common.rego policies:../example_v2/policies.json
OPA 0.9.2 (commit 9fbff4c3, built at 2018-09-24T16:12:26Z)

> data.authz_v2.authorized
false
# This matches against an action/resource from a statement in a policy.
> data.authz_v2.authorized with input as { "subjects": [ "team:local:admins" ], "action": "iam:teams:create", "resource": "iam:teams" }
true
> data.authz_v2.authorized with input as { "subjects": [ "team:local:admins" ], "action": "iam:teams:create", "resource": "iam:users" }
false
# This matches against an action/resource from a statement in a policy.
> data.authz_v2.authorized with input as { "subjects": [ "team:local:admins" ], "action": "infra:nodes:delete", "resource": "infra:nodes" }
true
>
```

#### Project Mappings

```
opa run -w rule_mappings.rego rules:../example_v2/rule_mappings.json
> data.rule_mappings.rules_for_project with input as { "project_id":"project2" }
[
  [
    {
      "type": "ChefOrgs",
      "values": [
        "Org1",
        "Org2"
      ]
    },
    {
      "type": "ChefServers",
      "values": [
        "chef-server-3",
        "chef-server-4",
        "chef-server-5"
      ]
    }
  ]
]
```

## Running OPA Unit Tests

From the current directory, run V1 unit tests with:

```console
$ opa test authz.rego common.rego authz_test.rego
```

From the current directory, run V2 unit tests with:

```console
$ opa test authz_v2.rego common.rego authz_v2_test.rego
```

That will print just names of failing tests
along with a summary of pass/fail results.
To explicitly show all tests, even if they pass, just add the verbose flag, e.g.:

```console
$ opa test -v authz_v2.rego common.rego authz_v2_test.rego
```

## Using the VSCode Plugin

You can also work on OPA policies within VSCode directly with the "Open Policy Agent" plugin.
Unlike the command line, which takes essentially no setup, using the plugin does require a bit of setup,
but it is worthwhile for those that prefer a GUI interface.
First follow the suggested setup on the plugin details page; the following notes are specific to our AuthZ file structure.

1. Your workspace root matters. Open a VSCode workspace rooted in *this* directory
   (as opposed to the a2 directory that you might typically start from),
   because the output just comes up empty when you attempt to evaluate rego expressions
   if rooted in the a2 directory.
2. OPA-the-plugin reads **all** your rego and JSON files.
   So if you skip the next steps when you attempt to evaluate an expression you will see merge errors.

### VSCode with V1

- copy the example dir here: `cp -r ../example policies`
- move the input file from that new dir here: `mv policies/input.json input.json`
- open `authz.rego` in VSCode

With your cursor in the authz.rego file run `OPA: Evaluate package` to see everything evaluated.
A successful setup will yield something like this, showing everything in the `authz` package
evaluated (which means authz.rego **and** authz_test.rego).

```json
// Evaluated package in 3.17557ms.
{
  "allow": true,
  "authorized": true,
  "has_action": [
    "a3e1556f-190b-4ad0-a356-a6bed9618e8c",
    "651589f0-4f98-4a33-910e-db4011601381"
  ],
  "has_resource": [
    "a3e1556f-190b-4ad0-a356-a6bed9618e8c"
  ],
  "has_subject": [
    "a3e1556f-190b-4ad0-a356-a6bed9618e8c",
    "651589f0-4f98-4a33-910e-db4011601381"
  ],
  "introspection": {
    "allowed_pair": [],
    "authorized_pair": [],
    "denied_pair": [],
    "pair_matches_action": [],
    "pair_matches_resource": []
  },
  "test_action_matches_direct_match": true,
  "test_action_matches_wildcard": true,
  "test_allow_matches_all_properties_and_effect_allow": true,
  "test_deny_matches_all_properties_and_effect_deny": true,
  . . . and the rest of the unit tests
}
```

With just a particular rule name selected in authz.rego (e.g. `allow` or `authorized`)
run `OPA: Evaluate selection` to see just that rule evaluated, e.g.:

```json
[
  [
    true
  ]
]
```

Similarly, and saving the best to last, you can run an individual unit test by selecting
a unit test name in auth_test.rego and running `OPA: Evaluate selection` to see that test evaluated.

### VSCode with V2

- copy the example dir here: `cp -r ../example_v2 policies`
- copy one of the input files from that new dir here: `cp policies/input_from_inline.json input.json`
- open `authz_v2.rego` in VSCode

(Using `input_from_statement.json` is done in a similar fashion.)

With your cursor in the authz_v2.rego file run `OPA: Evaluate package` to see everything evaluated.
A successful setup will yield something like this, showing everything in the `authz_v2` package
evaluated (which means authz_v2.rego **and** authz_v2_test.rego).

```json
// Evaluated package in 8.229989ms.
{
  "allow": true,
  "authorized": true,
  "has_action": [
    [
      "9acbe920-d977-4c4d-a482-f125fe83a95a",
      "7bca8de2-665b-4843-aa2e-c48850bbfe83"
    ]
  ],
  "has_member": [
    "9acbe920-d977-4c4d-a482-f125fe83a95a"
  ],
  "has_resource": [
    [
      "9acbe920-d977-4c4d-a482-f125fe83a95a",
      "7bca8de2-665b-4843-aa2e-c48850bbfe83"
    ]
  ],
  "introspection": {
    "allowed_pair": [],
    "authorized_pair": [],
    "denied_pair": [],
    "pair_matches_action": [],
    "pair_matches_resource": [],
    "test_pair_matches_action_picks_up_INLINE_action": true,
    "test_pair_matches_action_picks_up_ROLE_action": true
  },
  "test_action_matches_direct_match": true,
  "test_action_matches_service_match": true,
  "test_action_matches_service_type_match": true,
  "test_action_matches_verb_match": true,
  "test_action_matches_wildcard_match": true,
  "test_not_authorized_when_only_not_matching_policies_with_effect_allow_are_present": true
  . . . and the rest of the unit tests
}
```

With just a particular rule name selected in authz_v2.rego (e.g. `allow` or `authorized`)
run `OPA: Evaluate selection` to see just that rule evaluated, e.g.:

```json
[
  [
    true
  ]
]
```

Similarly, and saving the best to last, you can run an individual unit test by selecting
a unit test name in auth_v2_test.rego and running `OPA: Evaluate selection` to see that test evaluated.
