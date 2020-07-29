package opa_test

import (
	"context"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"
	"testing"
	"time"

	"github.com/chef/automate/components/authz-service/engine/opa"
	"github.com/mitchellh/mapstructure"
	"github.com/open-policy-agent/opa/ast"
	"github.com/open-policy-agent/opa/rego"
	"github.com/open-policy-agent/opa/storage/inmem"
	"github.com/open-policy-agent/opa/topdown"
	"github.com/pmezard/go-difflib/difflib"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

/************ ************ ************ ************ ************ ************
 * NOTE: These tests are low-level unit tests for the OPA engine,            *
 * so should be the first line of attack when crafting new rego code.        *
 *                                                                           *
 * These tests do NOT actually invoke the functions in opa.go! Rather,       *
 * they connect to OPA with direct calls (see `resultSet` in this file).     *
 *                                                                           *
 * Once these tests pass, move on to the conformance_test.go                 *
 * that actually do test the functions in opa.go.                            *
 ************ ************ ************ ************ ************ ************/

// Note: what this tests will likely be moved into tests that use the service's
// interface -- like what's happening in engine/conformance. For now, however,
// the gap between the GRPC API and the OPA engine hasn't been closed, so we'll
// have some tests here, closely aligned with the Rego code.

// This checks that the policy/*.rego files match their compiled-in versions
// so we don't accidentally forget updating the bindata file.
// (All rego files are compiled into the single policy.bindata.go file.)
// This also allows us to use the file in tests.
// Test cycles are hence faster, since they don't
// require you to call `go generate ./...` or a Makefile target.
func TestCompiledInOPAPolicy(t *testing.T) {
	// authz.rego, common.rego, introspection.rego
	an := opa.AssetNames()
	assert.Equal(t, 3, len(an), "expecting 3 assets")

	for _, asset := range opa.AssetNames() {
		compiled := opa.MustAsset(asset)

		onDisk, err := ioutil.ReadFile(asset)
		require.NoErrorf(t, err, "read asset %v from disk", asset)

		diff := difflib.UnifiedDiff{
			A:        difflib.SplitLines(string(onDisk)),
			B:        difflib.SplitLines(string(compiled)),
			FromFile: asset,
			FromDate: "on disk",
			ToFile:   asset,
			ToDate:   "generated",
			Context:  3,
		}
		text, err := difflib.GetUnifiedDiffString(diff)
		require.NoError(t, err, "error generating diff")
		if text != "" {
			t.Error("expected no difference, got diff:\n" + text)
		}
	}
}

func TestAuthorizedWithStatements(t *testing.T) {

	// Turns out providing the data as JSON is simpler than creating
	// a nested map[string]interface{} construction.
	// PAY ATTENTION: it has to be proper JSON. Any missing " or , will not be
	// tolerated; also, there's no way to comment.
	//
	// This data is what our rego code expects to find in the store.
	//
	// NOTE the the statements have a similar "id => data" layout as used with
	// policies; that's done to avoid the state explosion issues
	// we had thought with v1 policies when there's an exponential runtime (in the
	// number of policies).
	// It might make sense to adapt our storage layer to expose IDs there -- if we
	// end up using them internally anyways, we might as well use those in OPA,
	// instead of generating new ones on-the-fly.
	//
	// It should be considered a playground more so than a definite schema.
	// Since this test affects all the different matching things, members,
	// resources, and actions, it's quite brittle. So, this is just a playground.
	data := `{
  "policies": {
    "9acbe920-d977-4c4d-a482-f125fe83a95a": {
      "name": "pol01",
      "description": "we will perhaps not feed this into the OPA store -- what is the use?",
      "members": [
        "team:local:admins",
        "user:local:*"
      ],
      "statements": {
        "900e12b7-8625-4881-985f-8c460b79f142": {
          "effect": "allow",
          "resources": [
            "infra:nodes",
            "compliance:profiles:*"
          ],
          "actions": [
            "infra:nodes:delete",
            "compliance:profiles:create"
          ]
        },
        "7bca8de2-665b-4843-aa2e-c48850bbfe83": {
          "effect": "allow",
          "resources": [ "iam:teams" ],
          "actions": [ "iam:teams:create" ]
        }
      }
    }
  }
}`

	cases := map[string]map[string]interface{}{
		"exact match": {
			"subjects": []string{"team:local:admins"},
			"action":   "iam:teams:create",
			"resource": "iam:teams",
		},
		"subject wildcard": {
			"subjects": []string{"user:local:alice"},
			"action":   "iam:teams:create",
			"resource": "iam:teams",
		},
		"one of multiple actions": {
			"subjects": []string{"team:local:admins"},
			"action":   "infra:nodes:delete",
			"resource": "infra:nodes",
		},
		"one of multiple resources (wildcard)": {
			"subjects": []string{"team:local:admins"},
			"action":   "compliance:profiles:create",
			"resource": "compliance:profiles:yadda",
		},
	}

	query := "data.authz.authorized"

	for descr, input := range cases {
		t.Run(descr, func(t *testing.T) {
			rs := resultSet(t, input, strings.NewReader(data), query)

			require.Equal(t, 1, len(rs), "expected one result")
			require.Equal(t, 1, len(rs[0].Expressions), "expected one result expression")
			b, ok := rs[0].Expressions[0].Value.(bool)
			require.True(t, ok, "result value is a boolean")
			assert.Truef(t, b, "expected %q to be true", query)
		})
	}

	// Next, we re-run the same tests again, but with a second policy having
	// statements that deny what had just been authorized:
	data = `{
  "policies": {
    "9acbe920-d977-4c4d-a482-f125fe83a95a": {
      "name": "pol01",
      "description": "we will perhaps not feed this into the OPA store -- what is the use?",
      "members": [
        "team:local:admins",
        "user:local:*"
      ],
      "statements": {
        "900e12b7-8625-4881-985f-8c460b79f142": {
          "effect": "allow",
          "resources": [
            "infra:nodes",
            "compliance:profiles:*"
          ],
          "actions": [
            "infra:nodes:delete",
            "compliance:profiles:create"
          ]
        },
        "7bca8de2-665b-4843-aa2e-c48850bbfe83": {
          "effect": "allow",
          "resources": [ "iam:teams" ],
          "actions": [ "iam:teams:create" ]
        }
      }
    },
    "8ed84d95-400c-454d-8e70-50171c8a7543": {
      "name": "pol2",
      "members": [
        "team:local:admins",
        "user:local:*"
      ],
      "statements": {
        "900e12b7-8625-4881-985f-8c460b79f142": {
          "effect": "deny",
          "resources": [
            "infra:nodes",
            "compliance:profiles:*"
          ],
          "actions": [
            "infra:nodes:delete",
            "compliance:profiles:create"
          ]
        },
        "7bca8de2-665b-4843-aa2e-c48850bbfe83": {
          "effect": "deny",
          "resources": [ "iam:teams" ],
          "actions": [ "iam:teams:create" ]
        }
      }
    }
  }
}`

	for descr, input := range cases {
		t.Run(descr+" (DENY)", func(t *testing.T) {
			rs := resultSet(t, input, strings.NewReader(data), query)

			require.Equal(t, 1, len(rs), "expected one result")
			require.Equal(t, 1, len(rs[0].Expressions), "expected one result expression")
			b, ok := rs[0].Expressions[0].Value.(bool)
			require.True(t, ok, "result value is a boolean")
			assert.Falsef(t, b, "expected %q to be false", query)
		})
	}
}

func TestAuthorizedProjects(t *testing.T) {

	data := `{
  "policies": {
    "9acbe920-d977-4c4d-a482-f125fe83a95a": {
      "name": "pol01",
      "description": "we will perhaps not feed this into the OPA store -- what is the use?",
      "members": [
        "team:local:admins",
        "user:local:*"
      ],
      "statements": {
        "900e12b7-8625-4881-985f-8c460b79f142": {
          "effect": "allow",
          "projects": [ "p1", "p2" ],
          "resources": [
            "infra:nodes",
            "compliance:profiles:*"
          ],
          "actions": [
            "infra:nodes:delete",
            "compliance:profiles:create"
          ]
        },
        "7bca8de2-665b-4843-aa2e-c48850bbfe83": {
          "effect": "allow",
          "projects": [ "p3", "p4", "p5" ],
          "resources": [ "iam:teams" ],
          "actions": [ "iam:teams:create" ]
        }
      }
    }
  }
}`

	cases := map[string]map[string]interface{}{
		"exact match": {
			"subjects": []string{"team:local:admins"},
			"projects": []string{"p1", "p3", "p4"},
			"action":   "iam:teams:create",
			"resource": "iam:teams",
		},
	}

	query := "data.authz.authorized_project"

	for descr, input := range cases {
		t.Run(descr, func(t *testing.T) {
			rs := resultSet(t, input, strings.NewReader(data), query)

			require.Equal(t, 1, len(rs), "expected one result")
			require.Equal(t, 1, len(rs[0].Expressions), "expected one result expression")
			projects, ok := rs[0].Expressions[0].Value.([]interface{})
			require.True(t, ok, "result value is an array")
			expectedProjects := []string{"p4", "p3"}
			assert.ElementsMatch(t, expectedProjects, projects, "expected %q to return %v", query, expectedProjects)
		})
	}
}

func TestIntrospection(t *testing.T) {
	data := `{
  "policies": {
    "9acbe920-d977-4c4d-a482-f125fe83a95a": {
      "name": "pol01",
      "description": "we will perhaps not feed this into the OPA store -- what is the use?",
      "members": [
        "team:local:admins",
        "user:local:*"
      ],
      "statements": {
        "900e12b7-8625-4881-985f-8c460b79f142": {
          "effect": "allow",
          "resources": [
            "cfgmgmt:nodes",
            "compliance:profiles:*"
          ],
          "actions": [
            "cfgmgmt:delete",
            "compliance:upload"
          ]
        },
        "7bca8de2-665b-4843-aa2e-c48850bbfe83": {
          "effect": "allow",
          "resources": [ "auth:teams" ],
          "actions": [ "admin:create" ]
        }
      }
    }
  }
}`

	cases := map[string]map[string]interface{}{
		"exact match": {
			"subjects": []string{"team:local:admins"},
			"pairs":    []map[string]string{{"action": "admin:create", "resource": "auth:teams"}},
		},
		"subject wildcard": {
			"subjects": []string{"user:local:alice"},
			"pairs":    []map[string]string{{"action": "admin:create", "resource": "auth:teams"}},
		},
		"one of multiple actions": {
			"subjects": []string{"team:local:admins"},
			"pairs":    []map[string]string{{"action": "cfgmgmt:delete", "resource": "cfgmgmt:nodes"}},
		},
		"one of multiple resources (wildcard)": {
			"subjects": []string{"team:local:admins"},
			"pairs":    []map[string]string{{"action": "compliance:upload", "resource": "compliance:profiles:yadda"}},
		},
		"multiple matches (all of the above)": {
			"subjects": []string{"team:local:admins"},
			"pairs": []map[string]string{
				{"action": "cfgmgmt:delete", "resource": "cfgmgmt:nodes"},
				{"action": "admin:create", "resource": "auth:teams"},
				{"action": "compliance:upload", "resource": "compliance:profiles:yadda"},
			},
		},
	}

	query := "data.authz.introspection.authorized_pair[pair]"

	for descr, input := range cases {
		t.Run(descr, func(t *testing.T) {
			rs := resultSet(t, input, strings.NewReader(data), query)

			require.NotZero(t, len(rs), "expected at least one result")
			for _, result := range rs {
				bs := map[string]map[string]string{}
				err := mapstructure.Decode(result.Bindings, &bs)
				require.NoError(t, err, "decode result bindings")

				// The output always is a subset of the inputs. In our case, the input
				// only contains pairs we want to get out again, so, we match directly:
				assert.Contains(t, input["pairs"].([]map[string]string), bs["pair"])
			}
		})
	}
}

func TestActionsMatching(t *testing.T) {
	// This test, which checks a function's outputs given certain inputs,
	// does not depend on the OPA input or OPA data -- since the function
	// doesn't.
	input := map[string]interface{}{}
	data := "{}"

	// we're testing action_matches(in, stored)
	in := "iam:users:update" // "in" argument

	// expectedSuccess => "stored" argument
	classes := map[bool][]string{
		true: {
			"iam:users:update",
			"iam:users:*",
			"iam:*",
			"*",
			"iam:*:update",
			"*:update",
		},
		false: {
			"infra:nodes:delete",
			"infra:nodes:*",
			"infra:*",
			"infra:*:delete",
			"*:delete",
			"*:users:update", // it's only "*:VERB" that's allowed
			"*:iam:update",
			"*:*:update",
			"*:*:*",
			"iam:users:up*", // prefix
			"iam:us*:update",
			"ia*:users:update",
			"*am:users:update", // suffix
			"iam:*ers:update",
			"iam:users:*ate",
			"iam:users:update ", // space matters
			" iam:users:update",
		},
	}

	for expectedSuccess, actions := range classes {
		t.Run(fmt.Sprintf("%v", expectedSuccess), func(t *testing.T) {
			for _, stored := range actions {
				t.Run(stored, func(t *testing.T) {

					// We ask directly for what's used in the definition of
					// data.authz.has_action: the action_matches(in, stored) function
					query := fmt.Sprintf("data.authz.action_matches(%q, %q)", in, stored)
					rs := resultSet(t, input, strings.NewReader(data), query)
					if expectedSuccess {
						require.Equal(t, 1, len(rs))
						require.Equal(t, 1, len(rs[0].Expressions), "expected one result expression")
						result, ok := rs[0].Expressions[0].Value.(bool)
						require.True(t, ok, "expected result expression value to be boolean")
						assert.True(t, result)
					} else {
						require.Equal(t, 0, len(rs), "no result expected")
					}
				})
			}
		})
	}
}

func TestSubjectsMatching(t *testing.T) {
	// This test, which checks a function's outputs given certain inputs,
	// does not depend on the OPA input or OPA data -- since the function
	// doesn't.
	input := map[string]interface{}{}
	data := "{}"

	// we're testing subjets_matches(in, stored)
	in := "user:local:someid" // "in" argument

	// expectedSuccess => "stored" argument
	classes := map[bool][]string{
		true: {
			"user:local:someid",
			"user:local:*",
			"user:*",
			"*",
		},
		false: {
			"user:*:someid",
			"*:someid",
			"user:local:wrongid",
			"user:ldap:*",
			"user:local:SOMEID",
			"user:local:someid ",
			" user:local:someid",
			"USER:SOMEID",
			"user:local:someotherid",
			"user:local:some*",
			"user:l*",
			"use*",
			"team:local:some*",
			"team:l*",
			"tea*",
			"team:*",
			"team:local:*",
			"team:local:someid",
			"token:*",
			"tok*",
			"token:someid",
			"token:some*",
			"*:*",
			"*:*:*",
		},
	}

	for expectedSuccess, actions := range classes {
		t.Run(fmt.Sprintf("%v", expectedSuccess), func(t *testing.T) {
			for _, stored := range actions {
				t.Run(stored, func(t *testing.T) {

					query := fmt.Sprintf("data.common.subject_matches(%q, %q)", in, stored)
					rs := resultSet(t, input, strings.NewReader(data), query)
					if expectedSuccess {
						require.Equal(t, 1, len(rs))
						require.Equal(t, 1, len(rs[0].Expressions), "expected one result expression")
						result, ok := rs[0].Expressions[0].Value.(bool)
						require.True(t, ok, "expected result expression value to be boolean")
						assert.True(t, result)
					} else {
						require.Equal(t, 0, len(rs), "no result expected")
					}
				})
			}
		})
	}
}

// TestHasAction is focusing on finding the matching action in a policy (with
// one or more statements)

// Note: the policy evaluation logic this tests is the same used by
// data.authz.has_resource[[pol_id, statement_id]]. However, having the exact
// same set of tests for both of those paths, with only the word "action[s]"
// replaced by "resource[s]" doesn't feel right. Let's take this as another
// potentially useful way to test our policies, and decide how we want to
// achieve our desired assurances best.
func TestHasAction(t *testing.T) {
	// These tests set up different data layouts (policies), but always use the
	// same id for the matching policy/statement:
	polID, statementID := "86d88515-5f41-400d-8c2b-237bad00ff81", "7ae5ea03-eb67-4935-8387-1eafb4dffd78"

	query := "data.authz.has_action[[pol_id, statement_id]]"
	expectedBinding := map[string]string{
		"pol_id":       polID,
		"statement_id": statementID,
	}
	action, otherAction := "iam:users:update", "iam:users:delete"
	otherPolID, otherStatementID := "c525817a-6ded-426e-92e3-e9e5184da9a9", "b263a2cf-8b25-4f46-a00f-5c771594742c"
	input := map[string]interface{}{"action": action}

	// description => data JSON string containing policy setup
	cases := map[string]string{
		"one policy, only statement, only one action": fmt.Sprintf(
			`{"policies": {%q: {"statements": {%q: {"actions": [%q]}}}}}`, polID, statementID, action),
		"one policy, only statement, first action": fmt.Sprintf(
			`{"policies": {%q: {"statements": {%q: {"actions": [%q, %q]}}}}}`, polID, statementID, action, otherAction),
		"one policy, only statement, second action": fmt.Sprintf(
			`{"policies": {%q: {"statements": {%q: {"actions": [%q, %q]}}}}}`, polID, statementID, otherAction, action),
		"one policy, two statements, only one action": fmt.Sprintf(
			`{"policies": {%q: {"statements": {
        %q: {"actions": [%q]},
        %q: {"actions": [%q]}}}}}`, polID, otherStatementID, otherAction, statementID, action),
		"one policy, two statements, first action": fmt.Sprintf(
			`{"policies": {%q: {"statements": {
        %q: {"actions": [%q]},
        %q: {"actions": [%q, %q]}}}}}`, polID, otherStatementID, otherAction, statementID, action, otherAction),
		"one policy, two statements, second action": fmt.Sprintf(
			`{"policies": {%q: {"statements": {
        %q: {"actions": [%q]},
        %q: {"actions": [%q, %q]}}}}}`, polID, otherStatementID, otherAction, statementID, otherAction, action),
		"two policies, each with only one statement, only one action": fmt.Sprintf(
			`{"policies": {
        %q: {"statements": {%q: {"actions": [%q]}}},
        %q: {"statements": {%q: {"actions": [%q]}}}}}`, otherPolID, otherStatementID, otherAction,
			polID, statementID, action),
		"two policies, mixed statements, multiple action": fmt.Sprintf(
			`{"policies": {
        %q: {"statements": {%q: {"actions": [%q, %q]}}},
        %q: {"statements": {%q: {"actions": [%q, %q]}}}}}`, otherPolID, otherStatementID, otherAction, otherAction,
			polID, statementID, otherAction, action),
	}

	for desc, data := range cases {
		t.Run(desc, func(t *testing.T) {
			rs := resultSet(t, input, strings.NewReader(data), query)
			require.Equal(t, 1, len(rs))
			actualBinding := map[string]string{}
			err := mapstructure.Decode(rs[0].Bindings, &actualBinding)
			require.NoError(t, err, "error decoding bindings")
			assert.Equal(t, expectedBinding, actualBinding)
		})
	}
}

func TestHasProject(t *testing.T) {
	// These are smoke tests -- just to see that the rego code is accessible from Go code.
	// The `has_project` rego function has a rich set of rego unit tests for validation.
	polID, statementID := "86d88515-5f41-400d-8c2b-237bad00ff81", "7ae5ea03-eb67-4935-8387-1eafb4dffd78"
	project, otherProject := "project-9", "(unassigned)"
	input := map[string]interface{}{"projects": []string{project}}

	query := "data.authz.has_project[[project, pol_id, statement_id]]"
	expectedBinding := map[string]string{
		"project":      project,
		"pol_id":       polID,
		"statement_id": statementID,
	}

	// description => data JSON string containing policy setup
	cases := map[string]string{
		"one policy, only statement, one project": fmt.Sprintf(
			`{"policies": {%q: {"statements": {%q: {"projects": [%q]}}}}}`,
			polID, statementID, project),
		"one policy, only statement, two projects": fmt.Sprintf(
			`{"policies": {%q: {"statements": {%q: {"projects": [%q, %q]}}}}}`,
			polID, statementID, otherProject, project),
	}

	for desc, data := range cases {
		t.Run(desc, func(t *testing.T) {
			rs := resultSet(t, input, strings.NewReader(data), query)
			require.Equal(t, 1, len(rs))
			actualBinding := map[string]string{}
			err := mapstructure.Decode(rs[0].Bindings, &actualBinding)
			require.NoError(t, err, "error decoding bindings")
			assert.Equal(t, expectedBinding, actualBinding)
		})
	}
}

// Helper functions

func resultSet(t *testing.T,
	input map[string]interface{},
	data io.Reader,
	query string,
) rego.ResultSet {
	t.Helper()
	var tracer *topdown.BufferTracer
	// ⓘ DEBUG note: to see what's happening during policy execution in OPA,
	// uncomment the following line then execute some tests in this file.
	// tracer = topdown.NewBufferTracer()

	r := rego.New(
		rego.Query(query),
		rego.Compiler(compiler(t)),
		rego.Store(inmem.NewFromReader(data)),
		rego.Tracer(tracer),
		rego.Input(input),
	)

	ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
	defer cancel()

	rs, err := r.Eval(ctx)
	if err != nil {
		t.Fatalf("OPA eval failed: %s", err)
	}

	if tracer.Enabled() {
		topdown.PrettyTrace(os.Stderr, *tracer)
	}
	return rs
}

func compiler(t *testing.T) *ast.Compiler {
	t.Helper()
	return compilerWithModules(t, map[string]string{
		"authz.rego":         "../opa/policy/authz.rego",
		"introspection.rego": "../opa/policy/introspection.rego",
		"common.rego":        "../opa/policy/common.rego",
	})
}

func compilerWithModules(t *testing.T, modules map[string]string) *ast.Compiler {
	t.Helper()
	compiler := ast.NewCompiler()
	parsedModules := map[string]*ast.Module{}

	for name, path := range modules {
		moduleData, err := ioutil.ReadFile(path)
		require.NoErrorf(t, err, "could not read module %q", name)
		parsed, err := ast.ParseModule(name, string(moduleData))
		require.NoErrorf(t, err, "could not parse module %q", name)

		parsedModules[name] = parsed
	}

	compiler.Compile(parsedModules)
	require.Falsef(t, compiler.Failed(), "compile policies: %s", compiler.Errors)

	return compiler
}
