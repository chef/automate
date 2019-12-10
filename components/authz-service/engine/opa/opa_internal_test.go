package opa

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"math/rand"
	"os"
	"testing"
	"time"

	"github.com/open-policy-agent/opa/ast"
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

// At time of benchmarking, this is around half a second for 1000 teams
func BenchmarkV2FilterAuthorizedPairsRealWorldExample(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	pairs := []engine.Pair{
		engine.Pair{Resource: "compliance:reporting:nodes", Action: "compliance:reportNodes:list"},
		engine.Pair{Resource: "iam:policies", Action: "iam:policies:list"},
		engine.Pair{Resource: "iam:teams", Action: "iam:teams:create"},
		engine.Pair{Resource: "system:config", Action: "system:telemetryConfig:get"},
		engine.Pair{Resource: "compliance:profiles:market", Action: "compliance:marketProfiles:get"},
		engine.Pair{Resource: "retention:nodes", Action: "retention:nodes:update"},
		engine.Pair{Resource: "compliance:reporting:reports", Action: "compliance:reports:list"},
		engine.Pair{Resource: "infra:actions", Action: "infra:ingest:create"},
		engine.Pair{Resource: "system:service:logLevel", Action: "system:serviceLogLevel:set"},
		engine.Pair{Resource: "iam:rules", Action: "iam:rules:apply"},
		engine.Pair{Resource: "secrets:secrets", Action: "secrets:secrets:create"},
		engine.Pair{Resource: "system:iam:upgradeToV2", Action: "system:iam:upgrade"},
		engine.Pair{Resource: "iam:teams", Action: "iam:teams:update"},
		engine.Pair{Resource: "system:service:version", Action: "system:serviceVersion:get"},
		engine.Pair{Resource: "iam:policies", Action: "iam:policies:create"},
		engine.Pair{Resource: "compliance:reporting:stats:trend", Action: "compliance:reportTrend:get"},
		engine.Pair{Resource: "compliance:profiles", Action: "compliance:profiles:list"},
		engine.Pair{Resource: "infra:nodes", Action: "infra:nodes:delete"},
		engine.Pair{Resource: "compliance:reporting:stats:failures", Action: "compliance:reportFailures:get"},
		engine.Pair{Resource: "system:health", Action: "system:health:get"},
		engine.Pair{Resource: "compliance:scanner:jobs", Action: "compliance:scannerJobs:create"},
		engine.Pair{Resource: "compliance:profiles", Action: "compliance:profiles:create"},
		engine.Pair{Resource: "secrets:secrets", Action: "secrets:secrets:list"},
		engine.Pair{Resource: "applications:serviceGroups", Action: "applications:serviceGroups:list"},
		engine.Pair{Resource: "event:events", Action: "event:events:list"},
		engine.Pair{Resource: "iam:projects", Action: "iam:projects:create"},
		engine.Pair{Resource: "system:iam:resetToV1", Action: "system:iam:reset"},
		engine.Pair{Resource: "compliance:reporting:stats:profiles", Action: "compliance:reportProfiles:get"},
		engine.Pair{Resource: "infra:status", Action: "infra:ingest:get"},
		engine.Pair{Resource: "compliance:reporting:licenseusage", Action: "compliance:reportingLicenseUsage:list"},
		engine.Pair{Resource: "iam:policyVersion", Action: "iam:policies:get"},
		engine.Pair{Resource: "iam:projects", Action: "iam:projects:list"},
		engine.Pair{Resource: "iam:introspect", Action: "iam:introspect:get"},
		engine.Pair{Resource: "system:status", Action: "system:license:get"},
		engine.Pair{Resource: "iam:teams", Action: "iam:teams:list"},
		engine.Pair{Resource: "infra:nodes", Action: "infra:nodes:list"},
		engine.Pair{Resource: "iam:introspect", Action: "iam:introspect:getAllProjects"},
		engine.Pair{Resource: "retention:nodes", Action: "retention:nodes:get"},
		engine.Pair{Resource: "retention:serviceGroups", Action: "retention:serviceGroups:update"},
		engine.Pair{Resource: "compliance:scanner:jobs", Action: "compliance:scannerJobs:list"},
		engine.Pair{Resource: "infra:nodes", Action: "infra:nodes:create"},
		engine.Pair{Resource: "iam:tokens", Action: "iam:tokens:create"},
		engine.Pair{Resource: "iam:rules", Action: "iam:rules:cancel"},
		engine.Pair{Resource: "iam:tokens", Action: "iam:tokens:list"},
		engine.Pair{Resource: "infra:nodeManagers", Action: "infra:nodeManagers:create"},
		engine.Pair{Resource: "compliance:reporting:profiles", Action: "compliance:reportProfiles:list"},
		engine.Pair{Resource: "system:license", Action: "system:license:apply"},
		engine.Pair{Resource: "iam:introspect", Action: "iam:introspect:getAll"},
		engine.Pair{Resource: "infra:nodes", Action: "infra:ingestNodes:delete"},
		engine.Pair{Resource: "iam:roles", Action: "iam:roles:list"},
		engine.Pair{Resource: "retention:serviceGroups", Action: "retention:serviceGroups:get"},
		engine.Pair{Resource: "notifications:rules", Action: "notifications:notifyRules:validate"},
		engine.Pair{Resource: "compliance:reporting:suggestions", Action: "compliance:reportSuggestions:list"},
		engine.Pair{Resource: "system:service:version", Action: "system:serviceVersion:list"},
		engine.Pair{Resource: "iam:users", Action: "iam:users:list"},
		engine.Pair{Resource: "infra:nodes", Action: "infra:ingest:delete"},
		engine.Pair{Resource: "notifications:rules", Action: "notifications:notifyRules:create"},
		engine.Pair{Resource: "iam:rules", Action: "iam:rules:status"},
		engine.Pair{Resource: "iam:introspect", Action: "iam:introspect:getSome"},
		engine.Pair{Resource: "infra:nodeManagers", Action: "infra:nodeManagers:list"},
		engine.Pair{Resource: "applications:serviceGroups", Action: "applications:serviceGroups:delete"},
		engine.Pair{Resource: "compliance:reporting:control", Action: "compliance:controlItems:list"},
		engine.Pair{Resource: "compliance:reporting:report-ids", Action: "compliance:reportids:list"},
		engine.Pair{Resource: "compliance:reporting:stats:summary", Action: "compliance:reportSummary:get"},
		engine.Pair{Resource: "iam:users", Action: "iam:users:create"},
		engine.Pair{Resource: "system:license", Action: "system:license:request"},
		engine.Pair{Resource: "notifications:rules", Action: "notifications:notifyRules:list"},
		engine.Pair{Resource: "iam:roles", Action: "iam:roles:create"},
	}

	jsonFile, err := os.Open("example_v2/real_world_2p1_store.json")
	if err != nil {
		fmt.Println(err)
	}
	defer jsonFile.Close()
	byteValue, _ := ioutil.ReadAll(jsonFile)
	var pr struct {
		Policies map[string]interface{} `json:policies`
		Roles    map[string]interface{} `json:roles`
	}
	json.Unmarshal(byteValue, &pr)

	s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
		"policies": pr.Policies,
		"roles":    pr.Roles,
	})

	teamCount := []int{0, 1, 10, 30, 50, 100, 150, 300, 500, 1000, 10000}
	for _, count := range teamCount {
		b.Run(fmt.Sprintf("V2FilterAuthorizedPairs with real life input and %d teams", count), func(b *testing.B) {
			var resp []engine.Pair
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.V2FilterAuthorizedPairs(ctx, append([]string{"user:local:test@example.com"}, randomTeams(count)...), pairs)
				if err != nil {
					b.Error(err)
				}
			}
			filteredPairsResp = resp
		})
	}
}

// helpers

// randomTeams is shared
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

/// v2p1 section
// TODO make project count configurable
// there should be three policies per created project (viewer, editor, owner)
var (
	allProjects = []string{
		"(unassigned)",
		"project1",
		"project2",
		"project3",
		"project4",
		"project5",
		"project6",
	}
	projectsResponse []string
)

func BenchmarkV2P1GenericInput(b *testing.B) {
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

func BenchmarkV2P1SpecificInput(b *testing.B) {
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

func BenchmarkV2P1InitPartialResult(b *testing.B) {
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

func BenchmarkV2P1CompareParseCompile(b *testing.B) {
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

func BenchmarkV2P1InitPartialResultWithPolicies(b *testing.B) {
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

func BenchmarkProjectsAuthorizedWithInitPartialResult(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := []int{0, 5, 10, 20, 50, 100, 200}
	roleCount := 10 // keep this static while policies grow

	for _, count := range policyCount {
		policies, roles := v2p1RandomPoliciesAndRoles(count, roleCount)
		s.store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})
		err = s.initPartialResult(ctx)
		require.NoError(b, err, "init partial result")

		b.Run(fmt.Sprintf("store with %d random policies and %d random roles", count, roleCount), func(b *testing.B) {
			var resp []string
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.V2ProjectsAuthorized(ctx, []string{"user:local:test@example.com"}, "compliance:profiles:list", "compliance:profiles", allProjects)
				if err != nil {
					b.Error(err)
				}
			}
			projectsResponse = resp
		})
	}
}

var filteredPairsResp []engine.Pair

func BenchmarkV2P1FilterAuthorizedPairsWithPolicies(b *testing.B) {
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

// At time of benchmarking, this is around half a second for 1000 teams
func BenchmarkV2P1FilterAuthorizedPairsRealWorldExample(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	pairs := []engine.Pair{
		engine.Pair{Resource: "compliance:reporting:nodes", Action: "compliance:reportNodes:list"},
		engine.Pair{Resource: "iam:policies", Action: "iam:policies:list"},
		engine.Pair{Resource: "iam:teams", Action: "iam:teams:create"},
		engine.Pair{Resource: "system:config", Action: "system:telemetryConfig:get"},
		engine.Pair{Resource: "compliance:profiles:market", Action: "compliance:marketProfiles:get"},
		engine.Pair{Resource: "retention:nodes", Action: "retention:nodes:update"},
		engine.Pair{Resource: "compliance:reporting:reports", Action: "compliance:reports:list"},
		engine.Pair{Resource: "infra:actions", Action: "infra:ingest:create"},
		engine.Pair{Resource: "system:service:logLevel", Action: "system:serviceLogLevel:set"},
		engine.Pair{Resource: "iam:rules", Action: "iam:rules:apply"},
		engine.Pair{Resource: "secrets:secrets", Action: "secrets:secrets:create"},
		engine.Pair{Resource: "system:iam:upgradeToV2", Action: "system:iam:upgrade"},
		engine.Pair{Resource: "iam:teams", Action: "iam:teams:update"},
		engine.Pair{Resource: "system:service:version", Action: "system:serviceVersion:get"},
		engine.Pair{Resource: "iam:policies", Action: "iam:policies:create"},
		engine.Pair{Resource: "compliance:reporting:stats:trend", Action: "compliance:reportTrend:get"},
		engine.Pair{Resource: "compliance:profiles", Action: "compliance:profiles:list"},
		engine.Pair{Resource: "infra:nodes", Action: "infra:nodes:delete"},
		engine.Pair{Resource: "compliance:reporting:stats:failures", Action: "compliance:reportFailures:get"},
		engine.Pair{Resource: "system:health", Action: "system:health:get"},
		engine.Pair{Resource: "compliance:scanner:jobs", Action: "compliance:scannerJobs:create"},
		engine.Pair{Resource: "compliance:profiles", Action: "compliance:profiles:create"},
		engine.Pair{Resource: "secrets:secrets", Action: "secrets:secrets:list"},
		engine.Pair{Resource: "applications:serviceGroups", Action: "applications:serviceGroups:list"},
		engine.Pair{Resource: "event:events", Action: "event:events:list"},
		engine.Pair{Resource: "iam:projects", Action: "iam:projects:create"},
		engine.Pair{Resource: "system:iam:resetToV1", Action: "system:iam:reset"},
		engine.Pair{Resource: "compliance:reporting:stats:profiles", Action: "compliance:reportProfiles:get"},
		engine.Pair{Resource: "infra:status", Action: "infra:ingest:get"},
		engine.Pair{Resource: "compliance:reporting:licenseusage", Action: "compliance:reportingLicenseUsage:list"},
		engine.Pair{Resource: "iam:policyVersion", Action: "iam:policies:get"},
		engine.Pair{Resource: "iam:projects", Action: "iam:projects:list"},
		engine.Pair{Resource: "iam:introspect", Action: "iam:introspect:get"},
		engine.Pair{Resource: "system:status", Action: "system:license:get"},
		engine.Pair{Resource: "iam:teams", Action: "iam:teams:list"},
		engine.Pair{Resource: "infra:nodes", Action: "infra:nodes:list"},
		engine.Pair{Resource: "iam:introspect", Action: "iam:introspect:getAllProjects"},
		engine.Pair{Resource: "retention:nodes", Action: "retention:nodes:get"},
		engine.Pair{Resource: "retention:serviceGroups", Action: "retention:serviceGroups:update"},
		engine.Pair{Resource: "compliance:scanner:jobs", Action: "compliance:scannerJobs:list"},
		engine.Pair{Resource: "infra:nodes", Action: "infra:nodes:create"},
		engine.Pair{Resource: "iam:tokens", Action: "iam:tokens:create"},
		engine.Pair{Resource: "iam:rules", Action: "iam:rules:cancel"},
		engine.Pair{Resource: "iam:tokens", Action: "iam:tokens:list"},
		engine.Pair{Resource: "infra:nodeManagers", Action: "infra:nodeManagers:create"},
		engine.Pair{Resource: "compliance:reporting:profiles", Action: "compliance:reportProfiles:list"},
		engine.Pair{Resource: "system:license", Action: "system:license:apply"},
		engine.Pair{Resource: "iam:introspect", Action: "iam:introspect:getAll"},
		engine.Pair{Resource: "infra:nodes", Action: "infra:ingestNodes:delete"},
		engine.Pair{Resource: "iam:roles", Action: "iam:roles:list"},
		engine.Pair{Resource: "retention:serviceGroups", Action: "retention:serviceGroups:get"},
		engine.Pair{Resource: "notifications:rules", Action: "notifications:notifyRules:validate"},
		engine.Pair{Resource: "compliance:reporting:suggestions", Action: "compliance:reportSuggestions:list"},
		engine.Pair{Resource: "system:service:version", Action: "system:serviceVersion:list"},
		engine.Pair{Resource: "iam:users", Action: "iam:users:list"},
		engine.Pair{Resource: "infra:nodes", Action: "infra:ingest:delete"},
		engine.Pair{Resource: "notifications:rules", Action: "notifications:notifyRules:create"},
		engine.Pair{Resource: "iam:rules", Action: "iam:rules:status"},
		engine.Pair{Resource: "iam:introspect", Action: "iam:introspect:getSome"},
		engine.Pair{Resource: "infra:nodeManagers", Action: "infra:nodeManagers:list"},
		engine.Pair{Resource: "applications:serviceGroups", Action: "applications:serviceGroups:delete"},
		engine.Pair{Resource: "compliance:reporting:control", Action: "compliance:controlItems:list"},
		engine.Pair{Resource: "compliance:reporting:report-ids", Action: "compliance:reportids:list"},
		engine.Pair{Resource: "compliance:reporting:stats:summary", Action: "compliance:reportSummary:get"},
		engine.Pair{Resource: "iam:users", Action: "iam:users:create"},
		engine.Pair{Resource: "system:license", Action: "system:license:request"},
		engine.Pair{Resource: "notifications:rules", Action: "notifications:notifyRules:list"},
		engine.Pair{Resource: "iam:roles", Action: "iam:roles:create"},
	}

	jsonFile, err := os.Open("example_v2/real_world_2p1_store.json")
	if err != nil {
		fmt.Println(err)
	}
	defer jsonFile.Close()
	byteValue, _ := ioutil.ReadAll(jsonFile)
	var pr struct {
		Policies map[string]interface{} `json:policies`
		Roles    map[string]interface{} `json:roles`
	}
	json.Unmarshal(byteValue, &pr)

	s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
		"policies": pr.Policies,
		"roles":    pr.Roles,
	})

	teamCount := []int{0, 1, 10, 30, 50, 100, 150, 300, 500, 1000, 10000}
	for _, count := range teamCount {
		b.Run(fmt.Sprintf("V2FilterAuthorizedPairs with real life input and %d teams", count), func(b *testing.B) {
			var resp []engine.Pair
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.V2FilterAuthorizedPairs(ctx, append([]string{"user:local:test@example.com"}, randomTeams(count)...), pairs)
				if err != nil {
					b.Error(err)
				}
			}
			filteredPairsResp = resp
		})
	}
}

// 2p1helpers

// TODO useful for 2p1?
// func testPairs(c int) []engine.Pair {
// 	ret := make([]engine.Pair, c)
// 	for i := 0; i < c; i++ {
// 		var pol map[string]interface{}
// 		// repeat until our random policy has no wildcard action
// 		// the resource count contain a wildcard, but it really doesn't matter;
// 		// it'll be treated as a string
// 		for pol = randomPolicies(1)[0]; pol["action"] == "*"; pol = randomPolicies(1)[0] {
// 		}

// 		// append the count to ensure we don't end up with duplicates
// 		ret[i] = engine.Pair{
// 			Action:   engine.Action(fmt.Sprintf("%s%d", pol["action"], i)),
// 			Resource: engine.Resource(fmt.Sprintf("%s%d", pol["resource"], i)),
// 		}
// 	}
// 	return ret
// }

// TODO maybe don't need?
func migratedV1defaultPolicies() []map[string]interface{} {
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

func chefManagedRoles() map[string]interface{} {
	chefRoles := make(map[string]interface{}, 5)
	chefRoles["editor"] = map[string]interface{}{
		"actions": []string{
			"infra:*",
			"compliance:*",
			"system:*",
			"event:*",
			"ingest:*",
			"secrets:*",
			"telemetry:*",
			"iam:projects:list",
			"iam:projects:get",
			"iam:projects:assign",
			"applications:*",
		},
	}
	chefRoles["ingest"] = map[string]interface{}{
		"actions": []string{"infra:ingest:*", "compliance:profiles:get", "compliance:profiles:list"},
	}
	chefRoles["owner"] = map[string]interface{}{
		"actions": []string{"*"},
	}
	chefRoles["viewer"] = map[string]interface{}{
		"actions": []string{
			"secrets:*:get",
			"secrets:*:list",
			"infra:*:get",
			"infra:*:list",
			"compliance:*:get",
			"compliance:*:list",
			"system:*:get",
			"system:*:list",
			"event:*:get",
			"event:*:list",
			"ingest:*:get",
			"ingest:*:list",
			"iam:projects:list",
			"iam:projects:get",
			"applications:*:list",
			"applications:*:get",
		},
	}
	chefRoles["project-owner"] = map[string]interface{}{
		"actions": []string{
			"infra:*",
			"compliance:*",
			"system:*",
			"event:*",
			"ingest:*",
			"secrets:*",
			"telemetry:*",
			"iam:projects:list",
			"iam:projects:get",
			"iam:projects:assign",
			"iam:policies:list",
			"iam:policies:get",
			"iam:policyMembers:*",
			"iam:teams:list",
			"iam:teams:get",
			"iam:teamUsers:*",
			"iam:users:get",
			"iam:users:list",
		},
	}

	return chefRoles
}

func chefManagedPolicies() map[string]interface{} {
	// load chefManaged policies (editor, viewer, owner, ingest, project-owner)
	chefPolicies := make(map[string]interface{}, 4)
	chefPolicies["editor"] = map[string]interface{}{
		"members": []string{"team:local:editors"},
		"statements": map[string]interface{}{
			"edit-s0": map[string]interface{}{
				"effect":    "allow",
				"role":      "editor",
				"resources": []string{"*"},
				"projects":  []string{"*"},
			},
		},
	}
	chefPolicies["ingest"] = map[string]interface{}{
		"members": []string{"token:data_collector"},
		"statements": map[string]interface{}{
			"ing-s0": map[string]interface{}{
				"effect":    "allow",
				"role":      "ingest",
				"resources": []string{"*"},
				"projects":  []string{"*"},
			},
		},
	}
	chefPolicies["owner"] = map[string]interface{}{
		"members": []string{"user:local:admin"},
		"statements": map[string]interface{}{
			"adm-s0": map[string]interface{}{
				"effect":    "allow",
				"role":      "owner",
				"resources": []string{"*"},
				"projects":  []string{"*"},
			},
		},
	}
	chefPolicies["viewer"] = map[string]interface{}{
		"members": []string{"team:local:viewers"},
		"statements": map[string]interface{}{
			"adm-s0": map[string]interface{}{
				"effect":    "allow",
				"role":      "viewer",
				"resources": []string{"*"},
				"projects":  []string{"*"},
			},
		},
	}
	return chefPolicies
}

// TODO factory for projectPolicies (project-owner, editor, viewer)

func v2p1RandomPoliciesAndRoles(policyCount int, roleCount int) (policyMap map[string]interface{}, roleMap map[string]interface{}) {
	rand.Seed(time.Now().UnixNano())

	members := []string{"user:local:admin", "team:*", "team:local:sec", "team:local:admin", "user:ldap:*", "token:*"}
	actions := []string{
		"iam:teams:get",
		"iam:tokens:list",
		"iam:projects:delete",
		"iam:users:edit",
		"*",
		"compliance:profiles:list",
		"compliance:scannerJobs:rerun",
		"event:types:get",
		"infra:nodes:list",
		"notifications:notifyRules:create",
		"secrets:secrets:create",
		"system:license:apply",
		"compliance:reportSuggestions:list",
		"applications:serviceGroups:list",
		"applications:serviceGroups:delete"}

	// extract Chef-managed role IDs from map
	chefRoles := chefManagedRoles()
	chefRoleIDs := make([]string, len(chefRoles))
	for _, role := range chefRoles {
		chefRoleIDs[role["id"].(string)] = role
	}
	allRoleCount := len(chefRoleIDs) + roleCount

	// generate custom role IDs
	customRoleIDs := make([]string, roleCount)
	for b := 0; b < roleCount; b++ {
		customRoleIDs[b] = fmt.Sprintf("role-%v", b)
	}

	allRoles := make([]string, allRoleCount)
	allRoles = append(allRoles, chefRoleIDs...)
	allRoles = append(allRoles, customRoleIDs...)

	newRoles := make(map[string]interface{}, allRoleCount)
	// add chefRoles
	for _, role := range chefRoles {
		newRoles[role["id"].(string)] = role
	}

	// add customRoles with randomized actions
	for _, id := range customRoleIDs {
		// generate btwn range 1..total number of actions
		roleActionCount := rand.Intn((len(actions) - 1) + 1)
		// TODO actions not being shuffled enough on each loop
		rand.Shuffle(len(actions), func(x, y int) { actions[x], actions[y] = actions[y], actions[x] })

		newRoles[id] = map[string]interface{}{
			"actions": actions[:roleActionCount],
		}
	}

	// generate custom policy IDs
	customPolicyIDs := make([]string, policyCount)
	for e := 0; e < policyCount; e++ {
		customPolicyIDs[e] = fmt.Sprintf("pol-%v", e)
	}

	newPolicies := make(map[string]interface{}, policyCount)
	for _, pid := range customPolicyIDs {
		// generate btwn range 1..total number of members
		memberCount := rand.Intn((len(members) - 1) + 1)
		// shuffle the array of possible members,
		// then we take a slice from 0 to the random memberCount to get a randomized member list
		rand.Shuffle(len(members), func(m, n int) { members[m], members[n] = members[n], members[m] })

		statementCount := rand.Intn(10) // assume no more than 10 statements in each policy

		statements := make(map[string]interface{}, statementCount)
		for k := 0; k < statementCount; k++ {
			stID := fmt.Sprintf("statement-%v", k)

			var statementActions []string
			var statementRole string

			// determine if the statement has actions or a role with a coin toss
			coinToss := rand.Intn(1)
			if coinToss == 1 { // no actions, just a role
				statementActions = nil

				roleIndex := rand.Intn(allRoleCount)
				statementRole = allRoles[roleIndex]
			} else { // no role, just a list of actions
				statementRole = ""

				// generate btwn range 1..total number of actions
				policyActionCount := rand.Intn((len(actions) - 1) + 1)
				rand.Shuffle(len(actions), func(x, y int) { actions[x], actions[y] = actions[y], actions[x] })
				statementActions = actions[:policyActionCount]
			}

			var statementProjects []string
			projectCount := rand.Intn(len(allProjects))
			// there can never be 0 projects in a statement
			if projectCount == 0 {
				statementProjects = []string{"*"}
			} else {
				rand.Shuffle(len(allProjects), func(x, y int) { allProjects[x], allProjects[y] = allProjects[y], allProjects[x] })
			}

			statements[stID] = map[string]interface{}{
				"actions":   statementActions,
				"role":      statementRole,
				"resources": "*",     // v2.1 policies can only have "*" resources
				"effect":    "allow", // TODO write a separate test with deny cases
				"projects":  statementProjects,
			}
		}

		newPolicies[pid] = map[string]interface{}{
			"members":    members[:memberCount],
			"statements": statements,
		}
		fmt.Printf("policy: %#v\n\n", newPolicies[pid])
	}

	return newPolicies, newRoles
}
