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

// v2-only benchmarks

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

// At time of benchmarking, this is around half a second for 1000 teams
func BenchmarkFilterAuthorizedPairsRealWorldExample(b *testing.B) {
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

	policies, roles := v2BaselinePoliciesAndRoles()

	s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
		"policies": policies,
		"roles":    roles,
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

// Q: Which type of input is computed faster, generic Go interface or specific OPA Term?
func BenchmarkV2GenericInput(b *testing.B) {
	var r ast.Value
	var err error
	for n := 0; n < b.N; n++ {
		// always record the result to prevent the compiler eliminating the function
		// call.
		r, err = genericV2Input(
			[]string{"user:local:alice@example.com", "team:local:admins"},
			"some:resource",
			"some:resource:action",
			[]string{"project-1", "project-2"})
		if err != nil {
			b.Fatal(err)
		}
	}
	// always store the result to a package level variable
	// so the compiler cannot eliminate the Benchmark itself.
	result = r
}

func BenchmarkV2SpecificInput(b *testing.B) {
	var r ast.Value
	subjects, projects := []string{"user:local:alice@example.com", "team:local:admins"}, []string{"project-1", "project-2"}
	resource, action := "some:resource", "some:resource:action"
	for n := 0; n < b.N; n++ {
		subs := make(ast.Array, len(subjects))
		for i, sub := range subjects {
			subs[i] = ast.NewTerm(ast.String(sub))
		}
		projs := make(ast.Array, len(projects))
		for j, proj := range projects {
			projs[j] = ast.NewTerm(ast.String(proj))
		}
		r = ast.NewObject(
			[2]*ast.Term{ast.NewTerm(ast.String("subjects")), ast.NewTerm(subs)},
			[2]*ast.Term{ast.NewTerm(ast.String("resource")), ast.NewTerm(ast.String(resource))},
			[2]*ast.Term{ast.NewTerm(ast.String("action")), ast.NewTerm(ast.String(action))},
			[2]*ast.Term{ast.NewTerm(ast.String("projects")), ast.NewTerm(projs)},
		)
	}
	result = r
}

// A: Specific input is faster!
// BenchmarkGenericInput-8   	    328531	      4034 ns/op	     1664 B/op	    50 allocs/op
// BenchmarkSpecificInput-8   	  572641	      2073 ns/op	     976 B/op	      30 allocs/op

func BenchmarkAuthorizedProjectPreparedQueryWithIncreasingPolicies(b *testing.B) {
	var r error
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := []int{0, 5, 10, 20, 50, 100, 200, 1000}
	roleCount := 10 // keep this constant while increasing policyCount

	chefPolicies, _ := v2BaselinePoliciesAndRoles()

	for _, count := range policyCount {
		policies, roles := v2BaselineAndRandomPoliciesAndRoles(count, roleCount)
		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})

		b.Run(fmt.Sprintf("store with %d chef-managed policies and %d custom policies", len(chefPolicies), count),
			func(b *testing.B) {
				for n := 0; n < b.N; n++ {
					r = s.makeAuthorizedProjectPreparedQuery(ctx)
					if r != nil {
						b.Error(r)
					}
				}
				errResult = r
			})
	}
}

func BenchmarkProjectsAuthorizedWithIncreasingPolicies(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := []int{0, 5, 10, 20, 50, 100, 200, 1000}
	roleCount := 10 // keep this constant while increasing policyCount

	for _, count := range policyCount {
		policies, roles := v2BaselineAndRandomPoliciesAndRoles(count, roleCount)

		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})
		err = s.makeAuthorizedProjectPreparedQuery(ctx)
		require.NoError(b, err, "update OPA store and prepare projects query")

		b.Run(fmt.Sprintf("store with %d custom policies and %d custom roles", count, roleCount), func(b *testing.B) {
			var resp []string
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.V2ProjectsAuthorized(ctx, []string{"user:local:test"}, "compliance:profiles:list", "compliance:profiles", allProjects)
				if err != nil {
					b.Error(err)
				}
			}
			projectsResponse = resp
		})
	}
}

func BenchmarkFilterAuthorizedProjectsWithIncreasingPolicies(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := []int{0, 5, 10, 20, 50, 100, 200, 1000}
	roleCount := 10 // keep this constant while increasing policyCount

	for _, count := range policyCount {
		policies, roles := v2BaselineAndRandomPoliciesAndRoles(count, roleCount)

		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})
		err = s.makeAuthorizedProjectPreparedQuery(ctx)
		require.NoError(b, err, "update OPA store and prepare projects query")

		b.Run(fmt.Sprintf("store with %d custom policies and %d custom roles", count, roleCount), func(b *testing.B) {
			var resp []string
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.V2FilterAuthorizedProjects(ctx, []string{"user:local:test"})
				if err != nil {
					b.Error(err)
				}
			}
			projectsResponse = resp
		})
	}
}

func BenchmarkAuthorizedProjectPreparedQueryWithIncreasingRoles(b *testing.B) {
	var r error
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := 20 // keep this constant while increasing roleCount
	roleCount := []int{0, 5, 10, 20, 50, 100}

	chefPolicies, _ := v2BaselinePoliciesAndRoles()

	for _, count := range roleCount {
		policies, roles := v2BaselineAndRandomPoliciesAndRoles(policyCount, count)
		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})

		b.Run(fmt.Sprintf("store with %d chef-managed policies and %d custom policies", len(chefPolicies), count),
			func(b *testing.B) {
				for n := 0; n < b.N; n++ {
					r = s.makeAuthorizedProjectPreparedQuery(ctx)
					if r != nil {
						b.Error(r)
					}
				}
				errResult = r
			})
	}
}

func BenchmarkProjectsAuthorizedWithIncreasingRoles(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := 20 // keep this constant while increasing roleCount
	roleCount := []int{0, 5, 10, 20, 50, 100}

	for _, count := range roleCount {
		policies, roles := v2BaselineAndRandomPoliciesAndRoles(policyCount, count)
		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})

		err = s.makeAuthorizedProjectPreparedQuery(ctx)
		require.NoError(b, err, "prepared authorized project query")

		b.Run(fmt.Sprintf("store with %d custom policies and %d custom roles", policyCount, count), func(b *testing.B) {
			var resp []string
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.V2ProjectsAuthorized(ctx, []string{"user:local:test"}, "compliance:profiles:list", "compliance:profiles", allProjects)
				if err != nil {
					b.Error(err)
				}
			}
			projectsResponse = resp
		})
	}
}

func BenchmarkFilterAuthorizedProjectsWithIncreasingRoles(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := 20 // keep this constant while increasing roleCount
	roleCount := []int{0, 5, 10, 20, 50, 100}

	for _, count := range roleCount {
		policies, roles := v2BaselineAndRandomPoliciesAndRoles(policyCount, count)
		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})

		err = s.makeAuthorizedProjectPreparedQuery(ctx)
		require.NoError(b, err, "prepared authorized project query")

		b.Run(fmt.Sprintf("store with %d custom policies and %d custom roles", policyCount, count), func(b *testing.B) {
			var resp []string
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.V2FilterAuthorizedProjects(ctx, []string{"user:local:test"})
				if err != nil {
					b.Error(err)
				}
			}
			projectsResponse = resp
		})
	}
}

func BenchmarkProjectsAuthorizedWithIncreasingProjects(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	projectCounts := []int{5, 20, 100, 200, 300}
	member := "user:local:test"

	for _, projCount := range projectCounts {
		policyMap, projectIDs := v2BaselineAndProjectPolicies(projCount)

		_, roleMap := v2BaselinePoliciesAndRoles()

		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policyMap,
			"roles":    roleMap,
		})

		err = s.makeAuthorizedProjectPreparedQuery(ctx)
		require.NoError(b, err, "prepared authorized project query")

		b.Run(fmt.Sprintf("store with %d projects %d policies, and %d roles", projCount, len(policyMap), len(roleMap)), func(b *testing.B) {
			var resp []string
			var err error
			for n := 0; n < b.N; n++ {
				// include all projects in the filter to test the most amount of work the function might have to undertake
				resp, err = s.V2ProjectsAuthorized(ctx, []string{member}, "secrets:secrets:create", "secrets:secrets", projectIDs)
				if err != nil {
					b.Error(err)
				}
			}
			projectsResponse = resp
		})
	}
}

func BenchmarkFilterAuthorizedProjectsIncreasingProjects(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	roleCount := 5
	projectCounts := []int{5, 20, 100, 200, 300}
	member := "user:local:test"

	for _, projCount := range projectCounts {
		policyMap, _ := v2BaselineAndProjectPolicies(projCount)

		_, roleMap := v2BaselinePoliciesAndRoles()

		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policyMap,
			"roles":    roleMap,
		})

		err = s.makeAuthorizedProjectPreparedQuery(ctx)
		require.NoError(b, err, "prepared authorized project query")

		b.Run(fmt.Sprintf("store with %d projects %d policies, and %d roles", projCount, len(policyMap), roleCount), func(b *testing.B) {
			var resp []string
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.V2FilterAuthorizedProjects(ctx, []string{member})
				if err != nil {
					b.Error(err)
				}
			}
			projectsResponse = resp
		})
	}
}

func BenchmarkProjectsAuthorizedWithIncreasingSubjects(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	// keep these values constant as we increase the number of subjects
	policyCount := 20
	roleCount := 10

	policies, roles := v2BaselineAndRandomPoliciesAndRoles(policyCount, roleCount)
	s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
		"policies": policies,
		"roles":    roles,
	})

	err = s.makeAuthorizedProjectPreparedQuery(ctx)
	require.NoError(b, err, "prepared authorized project query")

	subjectCount := []int{0, 1, 10, 30, 50, 100, 150, 300, 500, 1000, 10000}
	for _, count := range subjectCount {
		b.Run(fmt.Sprintf("input with %d subjects", count), func(b *testing.B) {
			var resp []string
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.V2ProjectsAuthorized(ctx, []string{"user:local:test"}, "iam:projects:delete", "iam:projects", allProjects)
				if err != nil {
					b.Error(err)
				}
			}
			projectsResponse = resp
		})
	}
}

func BenchmarkFilterAuthorizedProjectsWithIncreasingSubjects(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	// keep these values constant as we increase the number of subjects
	policyCount := 20
	roleCount := 10

	policies, roles := v2BaselineAndRandomPoliciesAndRoles(policyCount, roleCount)
	s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
		"policies": policies,
		"roles":    roles,
	})

	err = s.makeAuthorizedProjectPreparedQuery(ctx)
	require.NoError(b, err, "prepared authorized project query")

	subjectCount := []int{0, 1, 10, 30, 50, 100, 150, 300, 500, 1000, 10000}
	for _, count := range subjectCount {
		b.Run(fmt.Sprintf("input with %d subjects", count), func(b *testing.B) {
			var resp []string
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.V2FilterAuthorizedProjects(ctx, append([]string{"user:local:test"}, randomTeams(count)...))
				if err != nil {
					b.Error(err)
				}
			}
			projectsResponse = resp
		})
	}
}

// Q: What happens if the subject appears more often as a member of different policies?
func BenchmarkAuthorizedProjectsIncreasingMembershipFrequency(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := 10
	roleCount := 5
	member := "user:local:test"

	// generate some simple, slightly differentiated policies
	policies := make(map[string]interface{}, policyCount)
	for i := 0; i < policyCount; i++ {
		pid := fmt.Sprintf("pol-%v", i)

		statement := map[string]interface{}{
			"resources": []string{"*"},
			"role":      []string{fmt.Sprintf("role-%v", i)},
			"effect":    "allow",
			"projects":  []string{fmt.Sprintf("proj-%v", i)},
		}
		statements := make(map[string]interface{}, 1)
		statements["s-1"] = statement

		policies[pid] = map[string]interface{}{
			"members":    []string{},
			"statements": statements,
		}
	}

	roles := make(map[string]interface{}, roleCount)
	for j := 0; j < roleCount; j++ {
		id := fmt.Sprintf("role-%v", j)
		roles[id] = map[string]interface{}{
			"actions": []string{fmt.Sprintf("some:automate:action%v", j)},
		}
	}

	s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
		"policies": policies,
		"roles":    roles,
	})

	b.Run("store with 0 policies that include the subject as a member", func(b *testing.B) {
		var resp []string
		var err error
		for n := 0; n < b.N; n++ {
			resp, err = s.V2ProjectsAuthorized(ctx, []string{"user:local:test"}, "iam:projects:delete", "iam:projects", allProjects)
			if err != nil {
				b.Error(err)
			}
		}
		projectsResponse = resp
	})

	for k := 0; k < policyCount; k++ {
		// add member to each policy as we iterate
		pol := policies[fmt.Sprintf("pol-%v", k)].(map[string]interface{})
		pol["members"] = []string{member}

		// refresh store to reflect policies with the subject as a member
		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})

		b.Run(fmt.Sprintf("store with %d out of 10 policies that include the subject as a member", k+1), func(b *testing.B) {
			var resp []string
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.V2ProjectsAuthorized(ctx, []string{"user:local:test"}, "iam:projects:delete", "iam:projects", allProjects)
				if err != nil {
					b.Error(err)
				}
			}
			projectsResponse = resp
		})
	}
}

// shared (v1/v2) helpers
func randomTeams(c int) []string {
	ret := make([]string, c)
	for i := 0; i < c; i++ {
		ret[i] = fmt.Sprintf("team:local:team%d", i)
	}
	return ret
}

// v2 helpers

func genericV2Input(subjects []string, resource string, action string, projects []string) (ast.Value, error) {
	subs := make([]interface{}, len(subjects))
	for i, sub := range subjects {
		subs[i] = sub
	}

	projs := make([]interface{}, len(projects))
	for i, proj := range projects {
		projs[i] = proj
	}
	input := map[string]interface{}{
		"subjects": subs,
		"resource": resource,
		"action":   action,
		"projects": projs,
	}
	return ast.InterfaceToValue(input)
}

func v2BaselineAndRandomPoliciesAndRoles(customPolicyCount int, customRoleCount int) (policies map[string]interface{}, roles map[string]interface{}) {
	rand.Seed(time.Now().UnixNano())

	// set lists of potential members and actions to be used to randomly generate custom policy contents
	members := []string{"user:local:admin", "team:*", "team:local:sec", "team:local:admin", "user:ldap:*", "token:*", "user:local:test"}
	actions := []string{
		"iam:teams:get",
		"iam:tokens:list",
		"iam:projects:delete",
		"iam:users:edit",
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

	chefPolicies, chefRoles := v2BaselinePoliciesAndRoles()

	roleMap := allRoles(customRoleCount, actions, chefRoles)

	policyMap := make(map[string]interface{}, customPolicyCount+len(chefPolicies))
	// first we add chef-managed policies to the map
	for id, pol := range chefPolicies {
		policyMap[id] = pol
	}

	// next we add custom policies with randomized members and statements
	if customPolicyCount > 0 {
		// generate custom policy IDs
		customPolicyIDs := make([]string, customPolicyCount)
		for e := 0; e < customPolicyCount; e++ {
			customPolicyIDs[e] = fmt.Sprintf("pol-%v", e)
		}

		for _, id := range customPolicyIDs {
			// generate btwn range 1..10 statements
			// 10 is an arbitrary max
			statementCount := rand.Intn(10-1) + 1

			statements := make(map[string]interface{}, statementCount)
			for k := 0; k < statementCount; k++ {
				stID := fmt.Sprintf("statement-%v", k)

				var statementActions []string
				var statementRole string

				// determine if the statement has actions or a role based on a coin toss
				coinToss := rand.Intn(2)
				if coinToss == 1 {
					// no actions, just a role
					statementActions = nil

					statementRole = getRandomRole(roleMap)
				} else {
					// no role, just a list of actions
					statementRole = ""

					statementActionCount := rand.Intn(len(actions))
					// the statement must have at least one action
					if statementActionCount == 0 {
						// so we use the 0 case as All Actions
						statementActions = []string{"*"}
					} else {
						rand.Shuffle(len(actions), func(x, y int) { actions[x], actions[y] = actions[y], actions[x] })
						statementActions = actions[:statementActionCount]
					}
				}

				var statementProjects []string
				projectCount := rand.Intn(len(allProjects))
				// there can never be 0 projects in a statement
				if projectCount == 0 {
					// so we use 0 as the All Projects case
					statementProjects = []string{"~~ALL-PROJECTS~~"}
				} else {
					rand.Shuffle(len(allProjects), func(x, y int) { allProjects[x], allProjects[y] = allProjects[y], allProjects[x] })
					statementProjects = allProjects[:projectCount]
				}

				statements[stID] = map[string]interface{}{
					"actions":   statementActions,
					"role":      statementRole,
					"resources": "*",     // v2.1 custom policies can only have "*" resources
					"effect":    "allow", // TODO write separate test setup with deny cases
					"projects":  statementProjects,
				}
			}

			// generate btwn range 1..total number of members
			memberCount := rand.Intn((len(members) - 1) + 1)

			// shuffle the array of possible members,
			// then we take a slice from 0 to the random memberCount to get a randomized member list for the policy
			rand.Shuffle(len(members), func(m, n int) { members[m], members[n] = members[n], members[m] })

			policyMap[id] = map[string]interface{}{
				"members":    members[:memberCount],
				"statements": statements,
			}
		}
	}

	return policyMap, roleMap
}

func v2BaselinePoliciesAndRoles() (policies map[string]interface{}, roles map[string]interface{}) {
	// this file includes system, migrated legacy, and chef-managed policies
	// and chef-managed roles
	jsonFile, err := os.Open("example_v2/real_world_2p1_store.json")
	if err != nil {
		fmt.Println(err)
	}
	defer jsonFile.Close()
	byteValue, _ := ioutil.ReadAll(jsonFile)
	var pr struct {
		Policies map[string]interface{} `json:"policies"`
		Roles    map[string]interface{} `json:"roles"`
	}
	json.Unmarshal(byteValue, &pr)

	return pr.Policies, pr.Roles
}

func v2BaselineAndProjectPolicies(count int) (policies map[string]interface{}, projects []string) {
	policyCount := count * 3

	chefPolicies, _ := v2BaselinePoliciesAndRoles()

	policyMap := make(map[string]interface{}, policyCount+len(chefPolicies))
	// first we add chef-managed policies to the map
	for id, pol := range chefPolicies {
		policyMap[id] = pol
	}

	// create a list of project ids, each of which will have 3 corresponding policies
	projectIDs := make([]string, count)
	for x := 0; x < count; x++ {
		projectIDs[x] = fmt.Sprintf("proj-%v", x)
	}

	for _, proj := range projectIDs {
		projectRoles := []string{"editor", "project-owner", "viewer"}
		for _, role := range projectRoles {
			pid := fmt.Sprintf("%s-%s", proj, role)

			statement := map[string]interface{}{
				"resources": []string{"*"},
				"role":      role,
				"effect":    "allow",
				"projects":  []string{proj},
			}
			statements := make(map[string]interface{}, 1)
			statements["s-1"] = statement

			policy := map[string]interface{}{
				"id":         pid,
				"members":    []string{"user:local:test", "token:fake"},
				"statements": statements,
			}

			policyMap[pid] = policy
		}
	}
	return policyMap, projectIDs
}

func allRoles(count int, actions []string, chefRoles map[string]interface{}) map[string]interface{} {
	rand.Seed(time.Now().UnixNano())

	// first we add chef-managed roles to the map
	allRoleCount := len(chefRoles) + count
	allRoles := make(map[string]interface{}, allRoleCount)
	for id, role := range chefRoles {
		allRoles[id] = role
	}

	// next we add custom roles with randomized actions to the role map
	if count > 0 {
		// generate custom role IDs
		customRoleIDs := make([]string, count)
		for b := 0; b < count; b++ {
			customRoleIDs[b] = fmt.Sprintf("role-%v", b)
		}

		for _, id := range customRoleIDs {
			// we'll add 1-roleActionCount actions to this role
			roleActionCount := rand.Intn(len(actions))

			// there can never be 0 actions in a role
			if roleActionCount == 0 {
				// so we use the 0 case as All Actions
				allRoles[id] = map[string]interface{}{
					"actions": []string{"*"},
				}
			} else {
				rand.Shuffle(len(actions), func(x, y int) { actions[x], actions[y] = actions[y], actions[x] })

				allRoles[id] = map[string]interface{}{
					"actions": actions[:roleActionCount],
				}
			}
		}
	}
	return allRoles
}

func getRandomRole(roleMap map[string]interface{}) string {
	var roleIDs []string
	for id := range roleMap {
		roleIDs = append(roleIDs, id)
	}

	randomIndex := rand.Intn(len(roleIDs))
	roleID := roleIDs[randomIndex]
	return roleID
}
