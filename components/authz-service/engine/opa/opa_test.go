package opa_test

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"testing"
	"time"

	"github.com/mitchellh/mapstructure"
	"github.com/open-policy-agent/opa/ast"
	"github.com/open-policy-agent/opa/rego"
	"github.com/open-policy-agent/opa/storage"
	"github.com/open-policy-agent/opa/storage/inmem"
	"github.com/open-policy-agent/opa/topdown"
	"github.com/pmezard/go-difflib/difflib"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/authz-service/engine/opa"
	"github.com/chef/automate/lib/stringutils"
)

// This checks that the policy/*.rego files match their compiled-in versions
// so we don't accidentally forget updating the bindata file.
// (All rego files are compiled into the single policy.bindata.go file.)
// This also allows us to use the file in tests.
// Test cycles are hence faster, since they don't
// require you to call `go generate ./...` or a Makefile target.
func TestCompiledInOPAPolicy(t *testing.T) {
	an := opa.AssetNames()
	assert.Equal(t, 5, len(an), "expecting 6 assets")

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

// Re: OPA unit tests
//
// The OPA CLI tool, `opa`, has support for testing. This works by adding rules
// that start with `test_`, like
//
//  test_get_user_allowed {
//    allow with input as {"path": ["users", "bob"], "method": "GET", "user_id": "bob"}
//  }
//
// However, that doesn't allow you to specify _data_. Our tests, below, are
// concerned about our _matching rules_, i.e., the logic required to figure out
// if a policy (our policy, which is data as far as OPA is concerned) matches
// the input. Hence, we need to be able to control both input and data to state
// interesting test cases.
//
// Also note that "data mocking" is on OPA's roadmap. So, in the future, we
// could be able to do without all the scaffolding here, and convert our tests
// to opa-native tests.
//
// For now, however, this is the best we can do.

// ANATOMY OF A TEST CASE
// Consider this canonical test case, copied from the live code:
//
//     {"read", map[string]string{"match": "*", "nomatch": "foo"}, defaultCheck1},
//
// That line creates two AuthZ policies, effectively "action: *" and "action: foo".
// Those two policies are being given names "match" and "nomatch", respectively,
// strictly for testing purposes here.
//
// So now we can talk about how in this test the first policy (named "match")
// will match the action "read" -- the first argument on the line.
// And clearly the policy "nomatch" (with value "foo") will *not* match "read".
// Thus there will be exactly one policy matching, and that policy will be named "match";
// those are precisely the checks done by the defaultCheck1 criteria.

// testcase can be used for enumerating test cases below
type testcase struct {
	input string
	// name -> policy field (which field is determined by the usage)
	policyData map[string]string
	checks     []checkFunc
}

func (tc *testcase) name() string {
	return fmt.Sprintf("%s/%v", tc.input, tc.policyData)
}

// subjectsTestcase can be used for enumerating subjects test cases below
type subjectsTestcase struct {
	input []string
	// name -> subjects field
	policyData map[string][]string
	checks     []checkFunc
}

func (tc *subjectsTestcase) name() string {
	return fmt.Sprintf("%v/%v", tc.input, tc.policyData)
}

func TestActionMatching(t *testing.T) {
	tests := map[string][]testcase{
		"positive": {
			{"read", map[string]string{"match": "read"}, defaultCheck1},
			{"read", map[string]string{"match": "*"}, defaultCheck1},
			{"read", map[string]string{"match": "*", "nomatch": "foo"}, defaultCheck1},
			{"read", map[string]string{"match": "read", "anothermatch": "*"}, defaultCheck2},
			{"read", map[string]string{"anothermatch": "*", "match": "read"}, defaultCheck2},
			{"read", map[string]string{"nomatch": "write", "match": "read", "nomatch2": "execute"}, defaultCheck1},
		},
		"negative": {
			{"read", map[string]string{}, noMatch},
			{"read", map[string]string{"simple_different": "write"}, noMatch},
			{"read", map[string]string{"simple_different": "write", "also_different": "execute"}, noMatch},
		},
	}

	for class, cases := range tests {
		t.Run(class, func(t *testing.T) {
			for _, tc := range cases {
				t.Run(tc.name(), func(t *testing.T) {
					rs := actionResultSet(t, tc.input, tc.policyData)
					for _, check := range tc.checks {
						check(t, rs)
					}
				})
			}
		})
	}
}

func TestResourceMatching(t *testing.T) {
	tests := map[string][]testcase{
		"positive": {
			// exact matching cases
			{"compliance", map[string]string{"match": "compliance"}, defaultCheck1},
			{"compliance:profiles:foobee", map[string]string{"match": "compliance:profiles:foobee"}, defaultCheck1},
			{"cfgmgmt:nodes:nodeId:runs", map[string]string{"match": "cfgmgmt:nodes:nodeId:runs"}, defaultCheck1},
			{"cfgmgmt:nodes:nodeId:runs:runId", map[string]string{"match": "cfgmgmt:nodes:nodeId:runs:runId"}, defaultCheck1},
			// wildcard namespace cases
			{"compliance", map[string]string{"match": "*"}, defaultCheck1},
			{"compliance", map[string]string{"match": "*", "nomatch": "foo"}, defaultCheck1},
			{"compliance:jobs", map[string]string{"match": "*"}, defaultCheck1},
			{"compliance:jobs:foobear", map[string]string{"match": "*"}, defaultCheck1},
			// wildcard name cases
			{"compliance:profiles", map[string]string{"match": "compliance:*"}, defaultCheck1},
			{"compliance:jobs:foobug", map[string]string{"match": "compliance:*"}, defaultCheck1},
			{"cfgmgmt:nodes:nodeId:runs:runId", map[string]string{"match": "cfgmgmt:nodes:nodeId:runs:*"}, defaultCheck1},
			{"cfgmgmt:nodes:nodeId:runs:runId", map[string]string{"match": "cfgmgmt:nodes:*"}, defaultCheck1},
			// wildcard property cases
			{"compliance:profiles:foobun", map[string]string{"match": "compliance:profiles:*"}, defaultCheck1},
			// mixed match cases (wildcard and not)
			{"compliance", map[string]string{"match": "compliance", "anothermatch": "*"}, defaultCheck2},
			{"compliance:profiles",
				map[string]string{"anothermatch": "compliance:*", "match": "compliance:profiles"}, defaultCheck2},
			{"compliance:profiles:foobee",
				map[string]string{"nomatch": "cfgmgmt", "match": "compliance:profiles:*", "nomatch2": "auth"},
				defaultCheck1},
		},
		"negative": {
			// exact matching cases
			{"compliance:scans", map[string]string{}, noMatch},
			{"compliance:scans", map[string]string{"nomatch": "compliance:scans:123"}, noMatch},
			{"compliance:scans:123", map[string]string{"nomatch": "compliance:scans:abc"}, noMatch},
			{"compliance:scans", map[string]string{"nomatch": "compliance:profiles"}, noMatch},
			// wildcard name cases
			{"cfgmgmt", map[string]string{"nomatch": "compliance:*"}, noMatch},
			{"cfgmgmt:nodes", map[string]string{"nomatch": "compliance:*"}, noMatch},
			{"cfgmgmt:nodes:foo", map[string]string{"nomatch": "compliance:*"}, noMatch},
			// wildcard property cases
			{"compliance", map[string]string{"nomatch": "compliance:*"}, noMatch},
			{"compliance:scans", map[string]string{"nomatch": "compliance:profiles:*"}, noMatch},
			{"compliance:scans:bar", map[string]string{"nomatch": "compliance:profiles:*"}, noMatch},
			{"compliance:profiles", map[string]string{"match": "compliance:profiles:*"}, noMatch},
			{"cfgmgmt", map[string]string{"nomatch": "compliance:profiles:*"}, noMatch},
			// matches on wildcard, not prefix
			{"compliance:profiles:foo", map[string]string{"nomatch": "compliance:profiles:f*"}, noMatch},
			// multiple no match cases
			{"compliance:profiles:foobee",
				map[string]string{"nomatch": "cfgmgmt", "nomatch2": "auth"}, noMatch},
		},
	}

	for class, cases := range tests {
		t.Run(class, func(t *testing.T) {
			for _, tc := range cases {
				t.Run(tc.name(), func(t *testing.T) {
					rs := resourceResultSet(t, tc.input, tc.policyData)
					for _, check := range tc.checks {
						check(t, rs)
					}
				})
			}
		})
	}
}

func TestSubjectsMatching(t *testing.T) {
	tests := map[string][]subjectsTestcase{
		// Note: as far as OPA is concerned, these are all just strings. However,
		// keeping the input close to our actual formats makes the test cases
		// more interesting. Also, we _could_ start to care about the format,
		// and then it doesn't all have to be redone.
		"positive": {
			// 1:1 matches
			{[]string{"user:local:someid"}, map[string][]string{"match": {"user:local:someid"}}, defaultCheck1},
			{[]string{"user:local:someid"},
				map[string][]string{"match": {"user:local:someid"}, "anothermatch": {"user:local:someid"}},
				defaultCheck2},
			// one input, many policy subjects
			{[]string{"user:local:someid"}, map[string][]string{"match": {"user:local:someid", "team:local:admins"}},
				defaultCheck1},
			{[]string{"team:local:admins"}, map[string][]string{"match": {"user:local:someid", "team:local:admins"}},
				defaultCheck1},
			{[]string{"team:local:admins"},
				map[string][]string{
					"match":        {"user:local:someid", "team:local:admins"},
					"anothermatch": {"user:local:someotherid", "team:local:admins"}},
				defaultCheck2},
			// many inputs, one policy subject
			{[]string{"user:local:someid", "team:local:admins"}, map[string][]string{"match": {"user:local:someid"}},
				defaultCheck1},
			{[]string{"user:local:someid", "team:local:admins"},
				map[string][]string{"match": {"user:local:someid"}, "nomatch": {"user:local:someother"}},
				defaultCheck1},
			{[]string{"user:local:someid", "team:local:admins"}, map[string][]string{"match": {"team:local:admins"}},
				defaultCheck1},
			{[]string{"user:local:someid", "team:local:admins"},
				map[string][]string{"match": {"team:local:admins"}, "nomatch": {"user:local:someother"}},
				defaultCheck1},
			{[]string{"user:local:someid", "team:local:admins"},
				map[string][]string{"match": {"user:local:someid"}, "nomatch": {"user:local:someotherid"}},
				defaultCheck1},
			{[]string{"user:local:someid", "team:local:admins"},
				map[string][]string{"match": {"user:local:someid"}, "teammatch": {"team:local:admins"}},
				defaultCheckCustom(2, "match", "teammatch")},

			// many inputs, many policy subjects
			// both matches "user"
			{[]string{"user:local:someid", "team:local:admins"},
				map[string][]string{"match": {"user:local:someid", "team:local:animals"},
					"match2": {"team:local:bears", "user:local:someid"}},
				defaultCheckCustom(2, "match", "match2")},
			// one match each
			{[]string{"user:local:someid", "team:local:admins"},
				map[string][]string{"match": {"user:local:someid", "team:local:animals"},
					"teammatch": {"team:local:admins", "user:local:otherid"}},
				defaultCheckCustom(2, "match", "teammatch")},

			// matching pairs
			{[]string{"user:local:someid", "team:local:admins"},
				map[string][]string{"match": {"user:local:someid", "team:local:admins"},
					"match2": {"team:local:admins", "user:local:someid"}},
				defaultCheckCustom(4, "match", "match2")}, // these match multiple times
			// one matching pair
			{[]string{"user:local:someid", "team:local:admins"},
				map[string][]string{"match": {"user:local:someid", "team:local:admins"},
					"match2": {"user:local:someid"}},
				defaultCheckCustom(3, "match", "match2")}, // pair matches twice
			// one matching pair, with teams matching
			{[]string{"user:local:someid", "team:local:admins"},
				map[string][]string{"match": {"user:local:someid", "team:local:admins"},
					"match2": {"team:local:admins"}},
				defaultCheckCustom(3, "match", "match2")}, // pair matches twice

			// wildcards in policies
			// one input
			{[]string{"user:local:someid"}, map[string][]string{"match": {"*"}}, defaultCheck1},
			// multiple inputs
			{[]string{"user:local:someid", "team:local:admin"}, map[string][]string{"match": {"*"}}, defaultCheck1},
			{[]string{"user:local:someid", "team:local:admin", "team:ldap:animals"},
				map[string][]string{"match": {"*"}},
				defaultCheck1},
			// one direct match, one wildcard
			{[]string{"user:local:someid"}, map[string][]string{"match": {"*"}, "anothermatch": {"user:local:someid"}},
				defaultCheck2},
			// two wildcards
			{[]string{"user:local:someid"}, map[string][]string{"match": {"*"}, "anothermatch": {"*"}}, defaultCheck2},
			// two inputs, two wildcards
			{[]string{"user:local:someid", "team:local:admin"},
				map[string][]string{"match": {"*"}, "anothermatch": {"*"}},
				defaultCheck2},

			// "partial wildcards"
			// user:*
			{[]string{"user:local:someid"}, map[string][]string{"match": {"user:*"}}, defaultCheck1},

			// team:{local,ldap,saml}:*
			{[]string{"team:local:somename"}, map[string][]string{"match": {"team:local:*"}}, defaultCheck1},

			// token:*
			{[]string{"token:5d29c419-cd6e-4f23-b57a-20c253dcaab1"}, map[string][]string{"match": {"token:*"}}, defaultCheck1},
		},
		"negative": {
			{[]string{"user:local:someid"}, map[string][]string{}, noMatch},
			{[]string{"user:local:someid"}, map[string][]string{"nomatch": {"user:local:SOMEID"}}, noMatch},
			{[]string{"user:local:someid"}, map[string][]string{"nomatch": {"user:local:someid "}}, noMatch},
			{[]string{"user:local:someid"}, map[string][]string{"nomatch": {" user:local:someid"}}, noMatch},
			{[]string{"user:local:someid"}, map[string][]string{"nomatch": {"USER:SOMEID"}}, noMatch},
			{[]string{"user:local:someid"}, map[string][]string{"nomatch": {"user:local:someotherid"}}, noMatch},
			{[]string{"team:local:admins"}, map[string][]string{"nomatch": {"team:local:foobears"}}, noMatch},
			{[]string{"team:local:admins"},
				map[string][]string{"nomatch": {"team:local:foobears"}, "anothernomatch": {"user:local:someid"}},
				noMatch},
			// We don't support prefixes within sections
			{[]string{"user:local:someid"}, map[string][]string{"nomatch": {"user:local:some*"}}, noMatch},
			{[]string{"user:local:someid"}, map[string][]string{"nomatch": {"user:l*"}}, noMatch},
			{[]string{"user:local:someid"}, map[string][]string{"nomatch": {"use*"}}, noMatch},
			{[]string{"team:local:somename"}, map[string][]string{"nomatch": {"team:local:some*"}}, noMatch},
			{[]string{"team:local:somename"}, map[string][]string{"nomatch": {"team:l*"}}, noMatch},
			{[]string{"team:local:somename"}, map[string][]string{"nomatch": {"tea*"}}, noMatch},
			{[]string{"token:someid"}, map[string][]string{"nomatch": {"token:some*"}}, noMatch},
			{[]string{"token:someid"}, map[string][]string{"nomatch": {"tok*"}}, noMatch},
		},
	}

	for class, cases := range tests {
		t.Run(class, func(t *testing.T) {
			for _, tc := range cases {
				t.Run(tc.name(), func(t *testing.T) {
					rs := subjectsResultSet(t, tc.input, tc.policyData)
					for _, check := range tc.checks {
						check(t, rs)
					}
				})
			}
		})
	}
}

func TestPolicyVariablesResourceMatching(t *testing.T) {
	type testcase struct {
		input      map[string]interface{}
		policyData map[string]string
		checks     []checkFunc
	}
	tests := map[string][]testcase{
		"positive": {
			{map[string]interface{}{
				"subjects": []string{"user:local:alice"},
				"resource": "auth:users:alice",
			}, map[string]string{"match": "auth:users:${a2:username}"}, defaultCheck1},
			{map[string]interface{}{
				"subjects": []string{"user:local:alice"},
				"resource": "auth:madeup:alice:something",
			}, map[string]string{"match": "auth:madeup:${a2:username}:something"}, defaultCheck1},
			// wildcard resource with variable
			{map[string]interface{}{
				"subjects": []string{"user:local:alice"},
				"resource": "auth:madeup:alice:something",
			}, map[string]string{"match": "auth:madeup:${a2:username}:*"}, defaultCheck1},
			// teams are present in input subjects
			{map[string]interface{}{
				"subjects": []string{"user:local:alice", "team:local:any"},
				"resource": "auth:users:alice",
			}, map[string]string{"match": "auth:users:${a2:username}"}, defaultCheck1},
		},
		"negative": {
			{map[string]interface{}{
				"subjects": []string{"user:local:alice"},
				"resource": "auth:users:bob",
			}, map[string]string{"nomatch": "auth:users:${a2:username}"}, noMatch},
			{map[string]interface{}{
				"subjects": []string{"team:local:any"}, // no username in inputs
				"resource": "auth:users:any",
			}, map[string]string{"nomatch": "auth:users:${a2:username}"}, noMatch},
		},
	}

	for class, cases := range tests {
		t.Run(class, func(t *testing.T) {
			for _, tc := range cases {
				t.Run(fmt.Sprintf("%v", tc), func(t *testing.T) {
					rs := resultSet(t, tc.input, tc.policyData, "resource")
					for _, check := range tc.checks {
						check(t, rs)
					}
				})
			}
		})
	}
}

type authorizedPairsTestcase struct {
	inputSubjects []string
	inputPairs    []pair
	policies      []policy
	checks        []checkFunc
}

// The struct annotations below let us use mapstructure.Decode() both ways:
// struct to map[string]string, and vice versa. Unfortunately, due to our
// wrapper types engine.Resource and engine.Action, the same isn't possible for the engine code.
// (There, mapstructure.Decode() is only used for one way, and that one doesn't need the tags.)
type pair struct {
	Resource string `mapstructure:"resource"`
	Action   string `mapstructure:"action"`
}

type policy struct {
	Subjects []string
	Resource string
	Action   string
	Effect   string
}

func TestAuthorizedPairs(t *testing.T) {
	tests := map[string]map[string]authorizedPairsTestcase{
		"positive": {
			"one of two input pairs is allowed (single subjects input)": {
				[]string{"user:local:someid"},
				[]pair{
					{"cfgmgmt:nodes", "read"},
					{"compliance:profiles", "delete"},
				},
				[]policy{
					{[]string{"user:local:someid", "team:local:admins"}, "cfgmgmt:nodes", "read", "allow"},
				},
				check(hasPairs(1), containsPair("cfgmgmt:nodes", "read")),
			},
			"two of two input pairs are allowed (one for team, one for user)": {
				[]string{"user:local:someid", "team:local:admins"},
				[]pair{
					{"cfgmgmt:nodes", "read"},
					{"compliance:profiles", "delete"},
				},
				[]policy{
					{[]string{"user:local:someid"}, "cfgmgmt:nodes", "read", "allow"},
					{[]string{"team:local:admins"}, "compliance:profiles", "delete", "allow"},
				},
				check(hasPairs(2),
					containsPair("cfgmgmt:nodes", "read"),
					containsPair("compliance:profiles", "delete"),
				),
			},
			"both input pairs are allowed, by hierarchical resources (using wildcards)": {
				[]string{"user:local:someid"},
				[]pair{
					{"cfgmgmt:nodes:98ba1a16-efbc-4274-a4a8-ca5c4ae03682", "read"},
					{"compliance:profiles:foobarprofile", "delete"},
				},
				[]policy{
					{[]string{"user:local:someid"}, "cfgmgmt:nodes:*", "read", "allow"},
					{[]string{"user:local:someid"}, "compliance:*", "delete", "allow"},
				},
				check(hasPairs(2),
					containsPair("cfgmgmt:nodes:98ba1a16-efbc-4274-a4a8-ca5c4ae03682", "read"),
					containsPair("compliance:profiles:foobarprofile", "delete"),
				),
			},
			"two of three input pairs are allowed (both from one policy)": {
				[]string{"user:local:someid"},
				[]pair{
					{"cfgmgmt:nodes:101", "read"},
					{"cfgmgmt:nodes:202", "read"},
					{"compliance:profiles", "delete"},
				},
				[]policy{
					{[]string{"user:local:someid", "team:local:admins"}, "cfgmgmt:nodes:*", "read", "allow"},
				},
				check(hasPairs(2),
					containsPair("cfgmgmt:nodes:101", "read"),
					containsPair("cfgmgmt:nodes:202", "read")),
			},
			"one of three input pairs are allowed (second pair allow overridden by deny)": {
				[]string{"user:local:someid"},
				[]pair{
					{"cfgmgmt:nodes:101", "read"},
					{"cfgmgmt:nodes:202", "read"},
					{"compliance:profiles", "delete"},
				},
				[]policy{
					{[]string{"user:local:someid", "team:local:admins"}, "cfgmgmt:nodes:*", "read", "allow"},
					{[]string{"user:local:someid"}, "cfgmgmt:nodes:202", "read", "deny"},
				},
				check(hasPairs(1),
					containsPair("cfgmgmt:nodes:101", "read")),
			},
			"input pairs with any subject allowed with top-level wildcard": {
				[]string{"team:local:admins"},
				[]pair{
					{"cfgmgmt", "read"},
					{"cfgmgmt:nodes", "read"},
					{"cfgmgmt:nodes:101", "read"},
					{"cfgmgmt:nodes:101:runs", "read"},
					{"cfgmgmt:nodes:101:runs:999", "read"},
				},
				[]policy{
					{[]string{"team:local:admins"}, "*", "read", "allow"},
				},
				check(hasPairs(5),
					containsPair("cfgmgmt", "read"),
					containsPair("cfgmgmt:nodes", "read"),
					containsPair("cfgmgmt:nodes:101", "read"),
					containsPair("cfgmgmt:nodes:101:runs", "read"),
					containsPair("cfgmgmt:nodes:101:runs:999", "read")),
			},
		},
		"negative": {
			"no pair returned when no policies match their resources": {
				[]string{"user:local:someid", "team:local:admins"},
				[]pair{
					{"cfgmgmt:nodes", "read"},
					{"compliance:profiles", "delete"},
				},
				[]policy{
					{[]string{"user:local:someid"}, "cfgmgmt:mismatch", "read", "allow"},
					{[]string{"team:local:admins"}, "compliance:mismatch", "delete", "allow"},
				},
				check(hasPairs(0)),
			},
			"no pair returned when no policies match their actions": {
				[]string{"user:local:someid", "team:local:admins"},
				[]pair{
					{"cfgmgmt:nodes", "read"},
					{"compliance:profiles", "delete"},
				},
				[]policy{
					{[]string{"user:local:someid"}, "cfgmgmt:nodes", "mismatch", "allow"},
					{[]string{"team:local:admins"}, "compliance:profiles", "mismatch", "allow"},
				},
				check(hasPairs(0)),
			},
			"no pair returned when matching policy is deny (no wildcard)": {
				[]string{"user:local:someid", "team:local:admins"},
				[]pair{
					{"cfgmgmt:nodes", "read"},
					{"compliance:profiles", "delete"},
				},
				[]policy{
					{[]string{"user:local:someid"}, "cfgmgmt:nodes", "read", "deny"},
				},
				check(hasPairs(0)),
			},
			"no pair returned when matching policy is deny (with same level wildcard)": {
				[]string{"user:local:someid", "team:local:admins"},
				[]pair{
					{"cfgmgmt:nodes:101", "read"},
					{"compliance:profiles", "delete"},
				},
				[]policy{
					{[]string{"user:local:someid"}, "cfgmgmt:nodes:*", "read", "deny"},
				},
				check(hasPairs(0)),
			},
			"no pair returned when matching policy is deny (with hierarchical wildcard)": {
				[]string{"user:local:someid", "team:local:admins"},
				[]pair{
					{"cfgmgmt:nodes:101", "read"},
					{"compliance:profiles", "delete"},
				},
				[]policy{
					{[]string{"user:local:someid"}, "cfgmgmt:*", "read", "deny"},
				},
				check(hasPairs(0)),
			},
			"no pair returned when ultimately policy is deny": {
				[]string{"user:local:someid", "team:local:admins"},
				[]pair{
					{"cfgmgmt:nodes:101", "read"},
					{"compliance:profiles", "delete"},
				},
				[]policy{
					{[]string{"user:local:someid"}, "cfgmgmt:*", "read", "allow"},
					{[]string{"user:local:someid"}, "cfgmgmt:nodes:*", "read", "allow"},
					{[]string{"user:local:someid"}, "cfgmgmt:nodes:101", "read", "deny"}},
				check(hasPairs(0)),
			},
		},
	}

	for class, cases := range tests {
		t.Run(class, func(t *testing.T) {
			for desc, tc := range cases {
				t.Run(desc, func(t *testing.T) {
					rs := authorizedPairResultSet(t, tc.inputSubjects, tc.inputPairs, tc.policies)
					for _, check := range tc.checks {
						check(t, rs)
					}
				})
			}
		})
	}
}

// Helper functions for declaring checks

type checkFunc func(*testing.T, rego.ResultSet)

func check(fs ...checkFunc) []checkFunc {
	return fs
}

func onlyMatch(matches ...string) checkFunc {
	return func(t *testing.T, rs rego.ResultSet) {
		ms := collectMatches(t, rs)
		assert.NotZero(t, len(ms))
		matched := map[string]bool{}
		for _, m := range ms {
			matched[m] = true
		}
		assert.Equalf(t, len(matched), len(matches), "expected only policies %q to be matched", matches)
	}
}

// defaultCheck1 provides a shortcut for defining checks: it expects only the
// policy with name "match" to be matched, any non-zero number of times.
var defaultCheck1 = check(onlyMatch("match"))
var defaultCheck2 = check(onlyMatch("match", "anothermatch"))

func defaultCheckCustom(count int, ms ...string) []checkFunc {
	fs := []checkFunc{hasMatches(count)}
	for _, match := range ms {
		fs = append(fs, matches(match))
	}

	return fs
}

// noMatch is another check shortcut: no matches whatsoever
var noMatch = check(hasMatches(0))

func hasMatches(num int) checkFunc {
	return func(t *testing.T, rs rego.ResultSet) {
		assert.Equalf(t, num, len(rs), "expected %d matches, matched %q", num, collectMatches(t, rs))
	}
}

func matches(targetName string) checkFunc {
	return func(t *testing.T, rs rego.ResultSet) {
		matchedNames := collectMatches(t, rs)
		match := stringutils.SliceContains(matchedNames, targetName)
		assert.Truef(t, match, "matched policies %q instead of %q", matchedNames, targetName)
	}
}

func collectMatches(t *testing.T, rs rego.ResultSet) []string {
	matchedNames := []string{}
	for _, result := range rs {
		name, ok := result.Expressions[0].Value.(string)
		require.True(t, ok, "result value is a string")

		matchedNames = append(matchedNames, name)
	}
	return matchedNames
}

func collectPairs(t *testing.T, rs rego.ResultSet) []pair {
	returnedPairs := []pair{}
	for _, result := range rs {
		require.Equalf(t, 1, len(result.Expressions),
			"expecting one expression per result, got %d", len(result.Expressions))
		exp := result.Expressions[0]
		m, ok := exp.Value.(map[string]interface{})
		require.True(t, ok, "expected result value to be map")

		p := pair{}
		err := mapstructure.Decode(m, &p)
		require.NoError(t, err, "failed to decode map")

		returnedPairs = append(returnedPairs, p)
	}
	return returnedPairs
}

func hasPairs(num int) checkFunc {
	return func(t *testing.T, rs rego.ResultSet) {
		assert.Equalf(t, num, len(rs), "expected %d pairs, got %v", num, collectPairs(t, rs))
	}
}

func containsPair(res, act string) checkFunc {
	return func(t *testing.T, rs rego.ResultSet) {
		found := false
		for _, pair := range collectPairs(t, rs) {
			if pair.Resource == res && pair.Action == act {
				found = true
			}
		}

		assert.Truef(t, found, "expected to find pair (%s, %s) in returned pairs", res, act)
	}
}

// These must remain synchronized with the authz.rego and introspection.rego
// files.
const (
	regoActionRule          = "data.authz.has_action[pol_id]"
	regoResourceRule        = "data.authz.has_resource[pol_id]"
	regoSubjectRule         = "data.authz.has_subject[pol_id]"
	regoAuthorizedPairsRule = "data.authz.introspection.authorized_pair[_]"
)

var propertiesLookup = map[string]string{
	"action":   regoActionRule,
	"resource": regoResourceRule,
	"subjects": regoSubjectRule,
}

func actionResultSet(t *testing.T, inputAction string, existingActions map[string]string) rego.ResultSet {
	key := "action"
	return resultSet(t, map[string]interface{}{key: inputAction}, existingActions, key)
}

func resourceResultSet(t *testing.T, inputResource string, existingResources map[string]string) rego.ResultSet {
	key := "resource"
	return resultSet(t, map[string]interface{}{key: inputResource}, existingResources, key)
}

func subjectsResultSet(t *testing.T, inputSubjects []string, existingSubjects map[string][]string) rego.ResultSet {
	key := "subjects"
	return resultSet(t, map[string]interface{}{key: inputSubjects}, existingSubjects, key)
}

// we have to provide inputPairs, because OPA doesn't know all the actions and resources
func authorizedPairResultSet(t *testing.T,
	inputSubjects []string, inputPairs []pair, policies []policy) rego.ResultSet {

	t.Helper()
	var tracer *topdown.BufferTracer
	// ⓘ DEBUG note: if you want to see what's happening during policy execution
	// in OPA, uncomment the following line
	// tracer = topdown.NewBufferTracer()

	inputPairsData := make([]interface{}, len(inputPairs))
	for i, pair := range inputPairs {
		inputPairsData[i] = map[string]string{}
		err := mapstructure.Decode(pair, &inputPairsData[i])
		require.NoErrorf(t, err, "failed to convert pair %v", pair)
	}

	opaInput := map[string]interface{}{
		"subjects": inputSubjects,
		"pairs":    inputPairsData,
	}

	policiesData := make(map[string]interface{}, len(policies))
	// use counter for policy ID
	for i, pol := range policies {
		policiesData[strconv.Itoa(i)] = map[string]interface{}{
			"subjects": pol.Subjects,
			"resource": pol.Resource,
			"action":   pol.Action,
			"effect":   pol.Effect,
		}
	}

	r := rego.New(
		rego.Query(regoAuthorizedPairsRule),
		rego.Compiler(compiler(t)),
		rego.Store(store(policiesData)),
		rego.Tracer(tracer),
		rego.Input(opaInput),
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

func resultSet(t *testing.T,
	input map[string]interface{},
	data interface{},
	key string) rego.ResultSet {
	t.Helper()
	var tracer *topdown.BufferTracer
	// ⓘ DEBUG note: if you want to see what's happening during policy execution
	// in OPA, uncomment the following line
	// tracer = topdown.NewBufferTracer()
	query := propertiesLookup[key]

	// Note 2018/04/12 (sr): Now that the policies data in OPA is a map, keyed by
	// policy ids, we use those instead of our "tagging" approach.
	policies := map[string]interface{}{}

	// Note 2018/02/28 (sr): This is accounting for the fact that when we work
	// with subjects, it's []string in inputs and existingData, whereas actions
	// and resources have plain string. It's not nice, but it's still better than
	// copying the entire resultSet method.
	switch y := data.(type) {
	case map[string]string:
		for name, datum := range y {
			policies[name] = map[string]interface{}{key: datum}
		}
	case map[string][]string:
		for name, datum := range y {
			policies[name] = map[string]interface{}{key: datum}
		}
	}

	r := rego.New(
		rego.Query(query),
		rego.Compiler(compiler(t)),
		rego.Store(store(policies)),
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

func store(policies map[string]interface{}) storage.Store {
	return inmem.NewFromObject(map[string]interface{}{
		"policies": policies,
	})
}
