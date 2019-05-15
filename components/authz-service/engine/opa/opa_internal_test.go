package opa

import (
	"context"
	"fmt"
	"math/rand"
	"os"
	"testing"
	"time"

	"github.com/mitchellh/mapstructure"
	"github.com/open-policy-agent/opa/ast"
	"github.com/open-policy-agent/opa/storage"
	"github.com/open-policy-agent/opa/storage/inmem"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/authz-service/engine"
	"github.com/chef/automate/lib/logger"
)

// This test file is for micro-benchmarks

// these package variables are required so the compiler does not optimize return values out
var result ast.Value
var authzResponse bool

func genericInput(subjects []string, resource string, action string) (ast.Value, error) {
	subs := make([]interface{}, len(subjects))
	for i, sub := range subjects {
		subs[i] = sub
	}
	input := map[string]interface{}{
		"subjects": subs,
		"resource": resource,
		"action":   action,
	}
	return ast.InterfaceToValue(input)
}

func BenchmarkGenericInput(b *testing.B) {
	var r ast.Value
	var err error
	for n := 0; n < b.N; n++ {
		// always record the result to prevent the compiler eliminating the function
		// call.
		r, err = genericInput([]string{"user:local:alice@example.com", "team:local:admins"}, "some:resource", "read")
		if err != nil {
			b.Fatal(err)
		}
	}
	// always store the result to a package level variable
	// so the compiler cannot eliminate the Benchmark itself.
	result = r
}

func BenchmarkSpecificInput(b *testing.B) {
	var r ast.Value
	subjects, resource, action := []string{"user:local:alice@example.com", "team:local:admins"}, "some:resource", "read"
	for n := 0; n < b.N; n++ {
		subs := make(ast.Array, len(subjects))
		for i, sub := range subjects {
			subs[i] = ast.NewTerm(ast.String(sub))
		}
		r = ast.NewObject(
			[2]*ast.Term{ast.NewTerm(ast.String("subjects")), ast.NewTerm(subs)},
			[2]*ast.Term{ast.NewTerm(ast.String("resource")), ast.NewTerm(ast.String(resource))},
			[2]*ast.Term{ast.NewTerm(ast.String("action")), ast.NewTerm(ast.String(action))},
		)
	}
	result = r
}

var errResult error

func BenchmarkInitPartialResult(b *testing.B) {
	var r error
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	initStore := func(s *State, i int) {
		policies := make([]map[string]interface{}, i)
		for j := 0; j < i; j++ {
			policies[j] = map[string]interface{}{
				"subjects": engine.Subject("token:any"),
				"action":   "read",
				"resource": "cfgmgmt:nodes",
				"effect":   "allow",
			}
		}
		s.store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
		})
	}

	for i := 0; i < 5; i++ {
		b.Run(fmt.Sprintf("store with %d items", i), func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				initStore(s, i)
				r = s.initPartialResult(ctx)
				if r != nil {
					b.Error(r)
				}
			}
			errResult = r
		})
	}
}

// Q: What takes longer: parsing the module or compiling it?

var parsedResult *ast.Module

func BenchmarkCompareParseCompile(b *testing.B) {
	policy := MustAsset("policy/authz.rego")
	compiler := ast.NewCompiler()
	parsed, err := ast.ParseModule("authz.rego", string(policy))
	if err != nil {
		b.Fatal(err)
	}

	b.Run("parsing", func(b *testing.B) {
		for n := 0; n < b.N; n++ {
			parsed, err = ast.ParseModule("authz.rego", string(policy))
			if err != nil {
				b.Error(err)
			}
		}
		parsedResult = parsed
	})

	b.Run("compiling", func(b *testing.B) {
		for n := 0; n < b.N; n++ {
			compiler.Compile(map[string]*ast.Module{"authz.rego": parsed})
			if compiler.Failed() {
				b.Error(compiler.Errors)
			}
		}
	})

	// consistency check
	b.Run("parse and compile", func(b *testing.B) {
		for n := 0; n < b.N; n++ {
			parsed, err = ast.ParseModule("authz.rego", string(policy))
			if err != nil {
				b.Error(err)
			}
			compiler.Compile(map[string]*ast.Module{"authz.rego": parsed})
			if compiler.Failed() {
				b.Error(compiler.Errors)
			}
		}
		parsedResult = parsed
	})
}

// A: parsing is worse:
// BenchmarkCompareParseCompile/parsing-8           100	  14742148 ns/op	 4060407 B/op	  119417 allocs/op
// BenchmarkCompareParseCompile/compiling-8         200	  12774183 ns/op	 2148726 B/op	  112140 allocs/op
// BenchmarkCompareParseCompile/parse_and_compile-8  50	  34038622 ns/op	 6883385 B/op	  272642 allocs/op

func BenchmarkInitPartialResultWithPolicies(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := []int{0, 5, 10, 20, 50, 100, 200}

	for _, count := range policyCount {
		policies := testPolicies(count)
		s.store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
		})

		b.Run(fmt.Sprintf("store with default policies and %d random policies", count), func(b *testing.B) {
			var r error

			for n := 0; n < b.N; n++ {
				r = s.initPartialResult(ctx)
				if r != nil {
					b.Error(r)
				}
			}
			errResult = r
		})

	}
}

func BenchmarkIsAuthorizedWithInitPartialResult(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := []int{0, 5, 10, 20, 50, 100, 200}

	for _, count := range policyCount {
		policies := testPolicies(count)
		s.store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
		})
		err = s.initPartialResult(ctx)
		require.NoError(b, err, "init partial result")

		b.Run(fmt.Sprintf("store with default policies and %d random policies", count), func(b *testing.B) {
			var resp bool
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.IsAuthorized(ctx, []string{"user:local:test@example.com"}, "read", "cfgmgmt:nodes")
				if err != nil {
					b.Error(err)
				}
			}
			authzResponse = resp
		})
	}
}

var filteredPairsResp []engine.Pair

func BenchmarkFilterAuthorizedPairsWithPolicies(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := []int{0, 1, 5, 10, 20, 50, 100}

	for _, count := range policyCount {
		policies := testPolicies(count)
		s.store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
		})

		b.Run(fmt.Sprintf("store with default policies and %d random policies", count), func(b *testing.B) {
			var resp []engine.Pair
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.FilterAuthorizedPairs(ctx, []string{"user:local:test@example.com"},
					[]engine.Pair{{Action: "read", Resource: "cfgmgmt:nodes"}})
				if err != nil {
					b.Error(err)
				}
			}
			filteredPairsResp = resp
		})
	}

	pairCount := []int{0, 1, 5, 10, 20, 50, 100}
	for _, count := range pairCount {
		s.store = inmem.NewFromObject(map[string]interface{}{
			"policies": testPolicies(0),
		})
		pairs := testPairs(count)

		b.Run(fmt.Sprintf("store with default policies and %d random pairs", count), func(b *testing.B) {
			var resp []engine.Pair
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.FilterAuthorizedPairs(ctx, []string{"user:local:test@example.com"}, pairs)
				if err != nil {
					b.Error(err)
				}
			}
			filteredPairsResp = resp
		})
	}

	teamCount := []int{0, 1, 5, 10, 20, 50, 100}
	for _, count := range teamCount {
		policies := testPolicies(0)
		s.store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
		})

		b.Run(fmt.Sprintf("store with default policies and %d random teams", count), func(b *testing.B) {
			var resp []engine.Pair
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.FilterAuthorizedPairs(ctx, append([]string{"user:local:test@example.com"}, randomTeams(count)...),
					[]engine.Pair{{Action: "read", Resource: "cfgmgmt:nodes"}})
				if err != nil {
					b.Error(err)
				}
			}
			filteredPairsResp = resp
		})
	}
}

// BenchmarkInitPartialResultV2 is initializing a partial result object using
// what currently (as of this commit) is a pretty default store content.
// The JSON is taken from the store-pretty.json file in engine/opa/example_v2/.
func BenchmarkInitPartialResultV2(b *testing.B) {
	var r error
	ctx := context.Background()

	f, err := os.Open("example_v2/store-pretty.json")
	require.NoError(b, err, "read example store JSON file")
	defer f.Close()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	store := inmem.NewFromReader(f)

	for d := 0; d < 5; d++ {
		b.Run(fmt.Sprintf("default policies, run %d times", d), func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				s, err := New(ctx, l)
				require.NoError(b, err, "init state")
				s.v2Store = store

				for e := 0; e <= d; e++ {
					r = s.initPartialResultV2(ctx)
					if r != nil {
						b.Error(r)
					}
				}
			}
			errResult = r
		})
	}

	for k := 0; k < 5; k++ {
		m := k * 25
		store = storeWithDummyPolicies(b, m)
		b.Run(fmt.Sprintf("%d dummy policies", m), func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				s, err := New(ctx, l)
				require.NoError(b, err, "init state")
				s.v2Store = store

				r = s.initPartialResultV2(ctx)
				if r != nil {
					b.Error(r)
				}
			}
			errResult = r
		})
	}
}

// helpers

func storeWithDummyPolicies(b *testing.B, k int) storage.Store {
	data := map[string]interface{}{
		"roles": map[string]interface{}{},
		"rules": map[string][]interface{}{},
	}
	policies := map[string]interface{}{}
	for i := 0; i <= k; i++ {
		policies[fmt.Sprintf("pol_id%d", i)] = map[string]interface{}{
			"members": []string{"user:local:alice"},
			"statements": map[string]interface{}{
				fmt.Sprintf("sid%d", i): map[string]interface{}{
					"actions":   []string{"iam:project:delete"},
					"resources": []string{"*"},
					"effect":    "allow",
					"projects":  []string{"~~ALL-PROJECTS~~"},
				},
			},
			"type": "custom",
		}
	}
	data["policies"] = policies
	// b.Log(data)
	return inmem.NewFromObject(data)
}

func randomTeams(c int) []string {
	ret := make([]string, c)
	for i := 0; i < c; i++ {
		ret[i] = fmt.Sprintf("team:local:team%d", i)
	}
	return ret
}

func testPairs(c int) []engine.Pair {
	ret := make([]engine.Pair, c)
	for i := 0; i < c; i++ {
		var pol map[string]interface{}
		// repeat until our random policy has no wildcard action
		// the resource count contain a wildcard, but it really doesn't matter;
		// it'll be treated as a string
		for pol = randomPolicies(1)[0]; pol["action"] == "*"; pol = randomPolicies(1)[0] {
		}

		// append the count to ensure we don't end up with duplicates
		ret[i] = engine.Pair{
			Action:   engine.Action(fmt.Sprintf("%s%d", pol["action"], i)),
			Resource: engine.Resource(fmt.Sprintf("%s%d", pol["resource"], i)),
		}
	}
	return ret
}

func defaultPolicies() []map[string]interface{} {
	allUsers := []string{"user:*"}
	allTokens := []string{"token:*"}
	admins := []string{"team:local:admins"}
	anyAction := "*"
	defaultEffect := "allow"

	defPolicyResources := []string{
		"compliance:*",
		"cfgmgmt:*",
		"cfgmgmt",
		"auth_introspection:*",
		"events",
		"events:*",
		"nodemanagers",
		"nodemanagers:*",
		"nodes",
		"nodes:*",
		"secrets",
		"secrets:*",
		"service_info:*",
	}
	defPolicyCount := len(defPolicyResources) + 5

	defPolicies := make([]map[string]interface{}, defPolicyCount)
	defPolicies[0] = map[string]interface{}{
		"subjects": admins,
		"resource": "*",
		"action":   anyAction,
		"effect":   defaultEffect,
	}
	defPolicies[1] = map[string]interface{}{
		"subjects": allTokens,
		"resource": "ingest:*",
		"action":   anyAction,
		"effect":   defaultEffect,
	}
	defPolicies[2] = map[string]interface{}{
		"subjects": allTokens,
		"resource": "compliance:profiles",
		"action":   "search",
		"effect":   defaultEffect,
	}
	defPolicies[3] = map[string]interface{}{
		"subjects": allTokens,
		"resource": "compliance:profiles:storage:*",
		"action":   "read",
		"effect":   defaultEffect,
	}
	defPolicies[4] = map[string]interface{}{
		"subjects": allTokens,
		"resource": "compliance:profiles:storage:*",
		"action":   "upload",
		"effect":   defaultEffect,
	}

	for j := 5; j < defPolicyCount; j++ {
		defPolicies[j] = map[string]interface{}{
			"subjects": allUsers,
			"action":   anyAction,
			"resource": defPolicyResources[j-5],
			"effect":   "allow",
		}
	}

	return defPolicies
}

// r is the number of random policies to include, appended to the default ones
func testPolicies(r int) []map[string]interface{} {
	return append(defaultPolicies(), randomPolicies(r)...)
}

func randomPolicies(i int) []map[string]interface{} {
	subjects := []string{"user:local:admin", "team:*", "team:local:sec", "team:local:admin", "user:ldap:*", "token:*"}
	resources := []string{
		"nodes",
		"nodes:123",
		"cfgmgmt:nodes:*",
		"notifications:rules",
		"cfgmgmt:stats",
		"compliance:profiles:alice",
		"compliance:reporting:reports:123",
		"events:*",
		"events:tasks",
		"secrets:*",
		"secrets:12345",
		"service_info:telemetry",
	}
	actions := []string{"read", "update", "delete", "create", "*", "search", "rerun", "count", "stop", "start"}

	newPolicies := make([]map[string]interface{}, i)
	for j := 0; j < i; j++ {
		rand.Seed(time.Now().Unix())
		subIndex := rand.Intn(len(subjects))
		resIndex := rand.Intn(len(resources))
		actIndex := rand.Intn(len(actions))

		newPolicies[j] = map[string]interface{}{
			"subjects": subjects[subIndex],
			"action":   actions[actIndex],
			"resource": resources[resIndex],
			"effect":   "allow",
		}
	}

	return newPolicies
}

var rule engine.Rule

func BenchmarkUnmarshallingWithMapstructure(b *testing.B) {
	var r engine.Rule
	m := map[string]interface{}{
		"type":   "ChefServers",
		"values": []string{"foo", "bar"},
	}
	for n := 0; n < b.N; n++ {
		r = engine.Rule{}
		if err := mapstructure.Decode(m, &r); err != nil {
			b.Error(err)
		}
	}
	rule = r
}

func BenchmarkUnmarshallingWithoutMapstructure(b *testing.B) {
	var r engine.Rule
	m := map[string]interface{}{
		"type":   "ChefServers",
		"values": []string{"foo", "bar"},
	}
	for n := 0; n < b.N; n++ {
		r = engine.Rule{}
		if t, ok := m["type"]; ok {
			if t, ok := t.(string); ok {
				r.Type = t
			}
		}
		if vs, ok := m["values"]; ok {
			if vs, ok := vs.([]string); ok {
				r.Values = vs
			}
		}
	}
	rule = r
}
