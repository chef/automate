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
		engine.Pair{Resource: "infra:nodes", Action: "infra:ingest:delete"},
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
		b.Run(fmt.Sprintf("with %d teams in input", count), func(b *testing.B) {
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

// BenchmarkFilterAuthorizedPairsRealWorldExample/with_0_teams_in_input-8       18	   71469413 ns/op	   27715819 B/op	  416006 allocs/op
// BenchmarkFilterAuthorizedPairsRealWorldExample/with_1_teams_in_input-8       15	   70341373 ns/op	   28104209 B/op	  423505 allocs/op
// BenchmarkFilterAuthorizedPairsRealWorldExample/with_10_teams_in_input-8      14	   79268379 ns/op	   31604062 B/op	  490993 allocs/op
// BenchmarkFilterAuthorizedPairsRealWorldExample/with_30_teams_in_input-8      13	  115535168 ns/op	   39381388 B/op	  640964 allocs/op
// BenchmarkFilterAuthorizedPairsRealWorldExample/with_50_teams_in_input-8       9	  126016309 ns/op	   47169624 B/op	  790930 allocs/op
// BenchmarkFilterAuthorizedPairsRealWorldExample/with_100_teams_in_input-8      7	  178359130 ns/op	   66610725 B/op	 1166059 allocs/op
// BenchmarkFilterAuthorizedPairsRealWorldExample/with_150_teams_in_input-8      4	  290961881 ns/op	   86099896 B/op	 1552118 allocs/op
// BenchmarkFilterAuthorizedPairsRealWorldExample/with_300_teams_in_input-8      3	  410043023 ns/op	  144511557 B/op	 2710281 allocs/op
// BenchmarkFilterAuthorizedPairsRealWorldExample/with_500_teams_in_input-8      2	  623028756 ns/op	  222406888 B/op	 4254489 allocs/op
// BenchmarkFilterAuthorizedPairsRealWorldExample/with_1000_teams_in_input-8     1	 1253675227 ns/op	  417184776 B/op	 8115021 allocs/op
// BenchmarkFilterAuthorizedPairsRealWorldExample/with_10000_teams_in_input-8    1	19126847294 ns/op	 3931882536 B/op	77604823 allocs/op
// 12/16/19 summary: up to 1-2 seconds with 1000 teams

// Q: Which type of input is computed faster, generic Go interface or specific OPA Term?
func BenchmarkV2GenericInput(b *testing.B) {
	subjects, projects := []string{"user:local:alice@example.com", "team:local:admins"}, []string{"project-1", "project-2"}
	resource, action := "some:resource", "some:resource:action"
	var r ast.Value
	var err error
	for n := 0; n < b.N; n++ {
		// always record the result to prevent the compiler eliminating the function
		// call.
		r, err = genericV2Input(
			subjects,
			resource,
			action,
			projects)
		if err != nil {
			b.Fatal(err)
		}
	}
	// always store the result to a package level variable
	// so the compiler cannot eliminate the Benchmark itself.
	result = r
}

func BenchmarkV2SpecificInput(b *testing.B) {
	subjects, projects := []string{"user:local:alice@example.com", "team:local:admins"}, []string{"project-1", "project-2"}
	resource, action := "some:resource", "some:resource:action"
	var r ast.Value
	for n := 0; n < b.N; n++ {
		r = specificV2Input(
			subjects,
			resource,
			action,
			projects)
	}
	result = r
}

// A: Specific input is faster!
// BenchmarkV2GenericInput-8   	    338047	      3727 ns/op	    1664 B/op	      50 allocs/op
// BenchmarkV2SpecificInput-8   	  713064	      1931 ns/op	     976 B/op	      30 allocs/op

func BenchmarkAuthorizedProjectPreparedQueryWithIncreasingPolicies(b *testing.B) {
	var r error
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCounts := []int{0, 5, 10, 20, 50, 100, 200, 1000}
	roleCount := 10 // keep this constant while increasing policyCount

	chefPolicies, _ := v2BaselinePoliciesAndRoles()

	for _, policyCount := range policyCounts {
		policies, roles := v2BaselineAndRandomPoliciesAndRoles(policyCount, roleCount)
		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})

		b.Run(fmt.Sprintf("store with %d chef-managed policies and %d custom policies", len(chefPolicies), policyCount),
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

// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingPolicies/store_with_18_chef-managed_policies_and_0_custom_policies-8       12	  84996295 ns/op	 14671988 B/op	  373316 allocs/op
// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingPolicies/store_with_18_chef-managed_policies_and_5_custom_policies-8       12	 101366184 ns/op	 15695630 B/op	  394906 allocs/op
// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingPolicies/store_with_18_chef-managed_policies_and_10_custom_policies-8      13	 119584001 ns/op	 17528115 B/op	  433279 allocs/op
// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingPolicies/store_with_18_chef-managed_policies_and_20_custom_policies-8      10	 134156734 ns/op	 20404048 B/op	  493707 allocs/op
// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingPolicies/store_with_18_chef-managed_policies_and_50_custom_policies-8       8	 169178163 ns/op	 30520222 B/op	  704484 allocs/op
// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingPolicies/store_with_18_chef-managed_policies_and_100_custom_policies-8      4	 372269868 ns/op	 43306724 B/op	  975824 allocs/op
// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingPolicies/store_with_18_chef-managed_policies_and_200_custom_policies-8      2	 511771048 ns/op	 69466028 B/op	 1524912 allocs/op
// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingPolicies/store_with_18_chef-managed_policies_and_1000_custom_policies-8     1	1359604423 ns/op	257229008 B/op	 5492555 allocs/op
// 12/16/19 summary: up to 1-2 seconds with over 1000 policies

func BenchmarkProjectsAuthorizedWithIncreasingPolicies(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCounts := []int{0, 5, 10, 20, 50, 100, 200, 1000}
	roleCount := 10 // keep this constant while increasing policyCount

	for _, policyCount := range policyCounts {
		policies, roles := v2BaselineAndRandomPoliciesAndRoles(policyCount, roleCount)

		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})
		err = s.makeAuthorizedProjectPreparedQuery(ctx)
		require.NoError(b, err, "update OPA store and prepare projects query")

		b.Run(fmt.Sprintf("store with %d custom policies and %d custom roles", policyCount, roleCount), func(b *testing.B) {
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

// BenchmarkProjectsAuthorizedWithIncreasingPolicies/store_with_0_custom_policies_and_10_custom_roles-8         	    8162	    148936 ns/op	   49535 B/op	     541 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingPolicies/store_with_5_custom_policies_and_10_custom_roles-8         	   10000	    149835 ns/op	   49566 B/op	     543 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingPolicies/store_with_10_custom_policies_and_10_custom_roles-8        	    5775	    176088 ns/op	   49566 B/op	     543 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingPolicies/store_with_20_custom_policies_and_10_custom_roles-8        	    7954	    247887 ns/op	   49522 B/op	     541 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingPolicies/store_with_50_custom_policies_and_10_custom_roles-8        	    8079	    352043 ns/op	   49563 B/op	     543 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingPolicies/store_with_100_custom_policies_and_10_custom_roles-8       	    5569	    293344 ns/op	   49563 B/op	     543 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingPolicies/store_with_200_custom_policies_and_10_custom_roles-8       	    4040	    342521 ns/op	   49559 B/op	     543 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingPolicies/store_with_1000_custom_policies_and_10_custom_roles-8      	    6307	    377988 ns/op	   49558 B/op	     543 allocs/op
// 12/16/19 summary: less than half a millisecond with over 1000 policies

func BenchmarkFilterAuthorizedProjectsWithIncreasingPolicies(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCounts := []int{0, 5, 10, 20, 50, 100, 200, 1000}
	roleCount := 10 // keep this constant while increasing policyCount

	for _, policyCount := range policyCounts {
		policies, roles := v2BaselineAndRandomPoliciesAndRoles(policyCount, roleCount)

		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})
		err = s.makeAuthorizedProjectPreparedQuery(ctx)
		require.NoError(b, err, "update OPA store and prepare projects query")

		b.Run(fmt.Sprintf("store with %d custom policies and %d custom roles", policyCount, roleCount), func(b *testing.B) {
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

// BenchmarkFilterAuthorizedProjectsWithIncreasingPolicies/store_with_0_custom_policies_and_10_custom_roles-8       1108	   1107602 ns/op	   275539 B/op	    5656 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingPolicies/store_with_5_custom_policies_and_10_custom_roles-8         69	  18851156 ns/op	  6038974 B/op	   92694 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingPolicies/store_with_10_custom_policies_and_10_custom_roles-8        72	  18495157 ns/op	  5203787 B/op	   81788 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingPolicies/store_with_20_custom_policies_and_10_custom_roles-8        37	  34467119 ns/op	  9826932 B/op	  154555 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingPolicies/store_with_50_custom_policies_and_10_custom_roles-8        12	 112702166 ns/op	 30529685 B/op	  477700 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingPolicies/store_with_100_custom_policies_and_10_custom_roles-8        5	 229425400 ns/op	 61646865 B/op	  949819 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingPolicies/store_with_200_custom_policies_and_10_custom_roles-8        2	 566905075 ns/op	102334816 B/op	 1599472 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingPolicies/store_with_1000_custom_policies_and_10_custom_roles-8       1	8002475951 ns/op	550344056 B/op	 8512827 allocs/op
// 12/16/19 summary: up to 8 seconds with over 1000 policies

func BenchmarkAuthorizedProjectPreparedQueryWithIncreasingRoles(b *testing.B) {
	var r error
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := 20 // keep this constant while increasing roleCount
	roleCounts := []int{0, 5, 10, 20, 50, 100}

	for _, roleCount := range roleCounts {
		policies, roles := v2BaselineAndRandomPoliciesAndRoles(policyCount, roleCount)
		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})

		b.Run(fmt.Sprintf("store with %d custom roles and %d custom policies", roleCount, policyCount),
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

// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingRoles/store_with_0_custom_roles_and_20_custom_policies-8      10	 177838629 ns/op	19926572 B/op	  483156 allocs/op
// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingRoles/store_with_5_custom_roles_and_20_custom_policies-8       6	 174438303 ns/op	19061697 B/op	  466096 allocs/op
// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingRoles/store_with_10_custom_roles_and_20_custom_policies-8      6	 191945097 ns/op	19702312 B/op	  478546 allocs/op
// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingRoles/store_with_20_custom_roles_and_20_custom_policies-8      5	 290461488 ns/op	20393857 B/op	  493008 allocs/op
// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingRoles/store_with_50_custom_roles_and_20_custom_policies-8      3	 384545383 ns/op	18926930 B/op	  462799 allocs/op
// BenchmarkAuthorizedProjectPreparedQueryWithIncreasingRoles/store_with_100_custom_roles_and_20_custom_policies-8     2	 761480943 ns/op	18748932 B/op	  459208 allocs/op
// 12/16/19 summary: up to 1 second with over 100 roles

func BenchmarkProjectsAuthorizedWithIncreasingRoles(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := 20 // keep this constant while increasing roleCount
	roleCounts := []int{0, 5, 10, 20, 50, 100}

	for _, roleCount := range roleCounts {
		policies, roles := v2BaselineAndRandomPoliciesAndRoles(policyCount, roleCount)
		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})

		err = s.makeAuthorizedProjectPreparedQuery(ctx)
		require.NoError(b, err, "prepared authorized project query")

		b.Run(fmt.Sprintf("store with %d custom roles and %d custom policies", roleCount, policyCount), func(b *testing.B) {
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

// BenchmarkProjectsAuthorizedWithIncreasingRoles/store_with_0_custom_roles_and_20_custom_policies-8      8073	    141641 ns/op	   49583 B/op	     543 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingRoles/store_with_5_custom_roles_and_20_custom_policies-8     10000	    129528 ns/op	   49523 B/op	     541 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingRoles/store_with_10_custom_roles_and_20_custom_policies-8     8367	    152109 ns/op	   49567 B/op	     543 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingRoles/store_with_20_custom_roles_and_20_custom_policies-8    10000	    140887 ns/op	   49516 B/op	     541 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingRoles/store_with_50_custom_roles_and_20_custom_policies-8     8964	    152607 ns/op	   49522 B/op	     541 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingRoles/store_with_100_custom_roles_and_20_custom_policies-8    8270	    149753 ns/op	   49564 B/op	     543 allocs/op
// 12/16/19 summary: less than half a millisecond with over 100 roles

func BenchmarkFilterAuthorizedProjectsWithIncreasingRoles(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	policyCount := 20 // keep this constant while increasing roleCount
	roleCounts := []int{0, 5, 10, 20, 50, 100}

	for _, roleCount := range roleCounts {
		policies, roles := v2BaselineAndRandomPoliciesAndRoles(policyCount, roleCount)
		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policies,
			"roles":    roles,
		})

		err = s.makeAuthorizedProjectPreparedQuery(ctx)
		require.NoError(b, err, "prepared authorized project query")

		b.Run(fmt.Sprintf("store with %d custom roles and %d custom policies", roleCount, policyCount), func(b *testing.B) {
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

// BenchmarkFilterAuthorizedProjectsWithIncreasingRoles/store_with_0_custom_roles_and_20_custom_policies-8       24	  44586494 ns/op	12848894 B/op	  199904 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingRoles/store_with_5_custom_roles_and_20_custom_policies-8       28	  62857583 ns/op	10947845 B/op	  171561 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingRoles/store_with_10_custom_roles_and_20_custom_policies-8      39	  50451429 ns/op	 8080393 B/op	  129856 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingRoles/store_with_20_custom_roles_and_20_custom_policies-8      18	  57813417 ns/op	12546945 B/op	  197058 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingRoles/store_with_50_custom_roles_and_20_custom_policies-8      33	  39547176 ns/op	 8922209 B/op	  143759 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingRoles/store_with_100_custom_roles_and_20_custom_policies-8     19	  67250666 ns/op	11250568 B/op	  174188 allocs/op
// 12/16/19 summary: less than 7 hundredths of a second with over 100 roles

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

		b.Run(fmt.Sprintf("store with %d projects, %d policies, and %d roles", projCount, len(policyMap), len(roleMap)), func(b *testing.B) {
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

// BenchmarkProjectsAuthorizedWithIncreasingProjects/store_with_5_projects,_33_policies,_and_5_roles-8         	     519	   2623504 ns/op	  676939 B/op	    9317 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingProjects/store_with_20_projects,_78_policies,_and_5_roles-8        	      99	  12418626 ns/op	 2676439 B/op	   37550 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingProjects/store_with_100_projects,_318_policies,_and_5_roles-8      	      12	  99153320 ns/op	15689900 B/op	  233776 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingProjects/store_with_200_projects,_618_policies,_and_5_roles-8      	       6	 188506243 ns/op	37933061 B/op	  627093 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingProjects/store_with_300_projects,_918_policies,_and_5_roles-8      	       4	 324684845 ns/op	67148484 B/op	 1180311 allocs/op
// 12/16/19 summary: less than half a second with 300 projects and 900 corresponding policies

func BenchmarkFilterAuthorizedProjectsIncreasingProjects(b *testing.B) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(b, err, "init logger")
	s, err := New(ctx, l)
	require.NoError(b, err, "init state")

	projectCounts := []int{5, 20, 100, 200, 300}
	member := "user:local:test"

	for _, projectCount := range projectCounts {
		policyMap, _ := v2BaselineAndProjectPolicies(projectCount)

		_, roleMap := v2BaselinePoliciesAndRoles()

		s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
			"policies": policyMap,
			"roles":    roleMap,
		})

		err = s.makeAuthorizedProjectPreparedQuery(ctx)
		require.NoError(b, err, "prepared authorized project query")

		b.Run(fmt.Sprintf("store with %d projects, %d policies, and %d roles", projectCount, len(policyMap), len(roleMap)), func(b *testing.B) {
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

// BenchmarkFilterAuthorizedProjectsIncreasingProjects/store_with_5_projects,_33_policies,_and_5_roles-8         	   481	   2673394 ns/op	  661999 B/op	   12346 allocs/op
// BenchmarkFilterAuthorizedProjectsIncreasingProjects/store_with_20_projects,_78_policies,_and_5_roles-8        	   188	   7190143 ns/op	 1821465 B/op	   32369 allocs/op
// BenchmarkFilterAuthorizedProjectsIncreasingProjects/store_with_100_projects,_318_policies,_and_5_roles-8      	    46	  30991366 ns/op	 8001491 B/op	  139110 allocs/op
// BenchmarkFilterAuthorizedProjectsIncreasingProjects/store_with_200_projects,_618_policies,_and_5_roles-8      	    18	  74279446 ns/op	15727943 B/op	  272518 allocs/op
// BenchmarkFilterAuthorizedProjectsIncreasingProjects/store_with_300_projects,_918_policies,_and_5_roles-8      	    18	  91202140 ns/op	23486797 B/op	  405924 allocs/op
// 12/16/19 summary: less than a tenth of a second with 300 projects and 900 corresponding policies

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

	subjectCounts := []int{0, 1, 10, 30, 50, 100, 150, 300, 500, 1000, 10000}
	for _, subjectCount := range subjectCounts {
		b.Run(fmt.Sprintf("input with %d subjects", subjectCount), func(b *testing.B) {
			var resp []string
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.V2ProjectsAuthorized(ctx, append([]string{"user:local:test"}, randomTeams(subjectCount)...),
					"iam:projects:delete", "iam:projects", allProjects)
				if err != nil {
					b.Error(err)
				}
			}
			projectsResponse = resp
		})
	}
}

// BenchmarkProjectsAuthorizedWithIncreasingSubjects/input_with_0_subjects-8         	   10000	    134722 ns/op	   46185 B/op	     499 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingSubjects/input_with_1_subjects-8         	    6447	    164869 ns/op	   51547 B/op	     606 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingSubjects/input_with_10_subjects-8        	    3540	    351820 ns/op	   99702 B/op	    1560 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingSubjects/input_with_30_subjects-8        	    1128	    928421 ns/op	  207047 B/op	    3680 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingSubjects/input_with_50_subjects-8        	     741	   1383707 ns/op	  314539 B/op	    5800 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingSubjects/input_with_100_subjects-8       	     480	   2387500 ns/op	  583147 B/op	   11127 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingSubjects/input_with_150_subjects-8       	     322	   4052937 ns/op	  855935 B/op	   17778 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingSubjects/input_with_300_subjects-8       	     145	   7813388 ns/op	 1673432 B/op	   37729 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingSubjects/input_with_500_subjects-8       	     100	  13112543 ns/op	 2763625 B/op	   64331 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingSubjects/input_with_1000_subjects-8      	      50	  25798461 ns/op	 5488807 B/op	  130833 allocs/op
// BenchmarkProjectsAuthorizedWithIncreasingSubjects/input_with_10000_subjects-8     	       4	 279753601 ns/op	54817776 B/op	 1327834 allocs/op
// 12/16/19 summary: less than a second with 10,000 subjects

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

	subjectCounts := []int{0, 1, 10, 30, 50, 100, 150, 300, 500, 1000, 10000}
	for _, subjectCount := range subjectCounts {
		b.Run(fmt.Sprintf("input with %d subjects", subjectCount), func(b *testing.B) {
			var resp []string
			var err error
			for n := 0; n < b.N; n++ {
				resp, err = s.V2FilterAuthorizedProjects(ctx, append([]string{"user:local:test"}, randomTeams(subjectCount)...))
				if err != nil {
					b.Error(err)
				}
			}
			projectsResponse = resp
		})
	}
}

// BenchmarkFilterAuthorizedProjectsWithIncreasingSubjects/input_with_0_subjects-8         	      40	    39874538 ns/op	    13800522 B/op	    214424 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingSubjects/input_with_1_subjects-8         	      19	    74988428 ns/op	    25656845 B/op	    380876 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingSubjects/input_with_10_subjects-8        	       3	   470849861 ns/op	   132350672 B/op	   1878625 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingSubjects/input_with_30_subjects-8        	       2	  1003187020 ns/op	   369447356 B/op	   5206905 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingSubjects/input_with_50_subjects-8        	       1	  1663301441 ns/op	   606530488 B/op	   8535180 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingSubjects/input_with_100_subjects-8       	       1	  4055170687 ns/op	  1199097344 B/op	  16856685 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingSubjects/input_with_150_subjects-8       	       1	  5521300056 ns/op	  1791588352 B/op	  25234372 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingSubjects/input_with_300_subjects-8       	       1	 12194054286 ns/op	  3568958664 B/op	  50366775 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingSubjects/input_with_500_subjects-8       	       1	 19000828806 ns/op	  5938771280 B/op	  83876611 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingSubjects/input_with_1000_subjects-8      	       1	 38236004231 ns/op	 11863293192 B/op	 167651037 allocs/op
// BenchmarkFilterAuthorizedProjectsWithIncreasingSubjects/input_with_10000_subjects-8     	       1	374165499542 ns/op	118586373552 B/op	1675582505 allocs/op
// 12/16/19 summary: up to 30 seconds with 1,000 subjects,
// 6 minutes with 10,000 subjects

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
			"role":      fmt.Sprintf("role-%v", i),
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
	err = s.makeAuthorizedProjectPreparedQuery(ctx)
	require.NoError(b, err, "update OPA store and prepare projects query")

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
		err = s.makeAuthorizedProjectPreparedQuery(ctx)
		require.NoError(b, err, "update OPA store and prepare projects query")

		b.Run(fmt.Sprintf("store with %d out of %d policies that include the subject as a member", k+1, policyCount), func(b *testing.B) {
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

// A: more frequent membership means slower authorization times (but not by much)
// BenchmarkAuthorizedProjectsIncreasingMembershipFrequency/store_with_0_policies_that_include_the_subject_as_a_member-8         	   179049	      8298 ns/op	    3193 B/op	      83 allocs/op
// BenchmarkAuthorizedProjectsIncreasingMembershipFrequency/store_with_1_out_of_10_policies_that_include_the_subject_as_a_member-8   140113	     10122 ns/op	    3304 B/op	      86 allocs/op
// BenchmarkAuthorizedProjectsIncreasingMembershipFrequency/store_with_2_out_of_10_policies_that_include_the_subject_as_a_member-8   135043	     11135 ns/op	    3304 B/op	      86 allocs/op
// BenchmarkAuthorizedProjectsIncreasingMembershipFrequency/store_with_3_out_of_10_policies_that_include_the_subject_as_a_member-8   116342	     11588 ns/op	    3304 B/op	      86 allocs/op
// BenchmarkAuthorizedProjectsIncreasingMembershipFrequency/store_with_4_out_of_10_policies_that_include_the_subject_as_a_member-8   	85137	     13811 ns/op	    3304 B/op	      86 allocs/op
// BenchmarkAuthorizedProjectsIncreasingMembershipFrequency/store_with_5_out_of_10_policies_that_include_the_subject_as_a_member-8    73464	     16099 ns/op	    3352 B/op	      88 allocs/op
// BenchmarkAuthorizedProjectsIncreasingMembershipFrequency/store_with_6_out_of_10_policies_that_include_the_subject_as_a_member-8    74788	     14378 ns/op	    3304 B/op	      86 allocs/op
// BenchmarkAuthorizedProjectsIncreasingMembershipFrequency/store_with_7_out_of_10_policies_that_include_the_subject_as_a_member-8    92748	     15486 ns/op	    3304 B/op	      86 allocs/op
// BenchmarkAuthorizedProjectsIncreasingMembershipFrequency/store_with_8_out_of_10_policies_that_include_the_subject_as_a_member-8    55911	     17941 ns/op	    3304 B/op	      86 allocs/op
// BenchmarkAuthorizedProjectsIncreasingMembershipFrequency/store_with_9_out_of_10_policies_that_include_the_subject_as_a_member-8    95604	     13995 ns/op	    3304 B/op	      86 allocs/op
// BenchmarkAuthorizedProjectsIncreasingMembershipFrequency/store_with_10_out_of_10_policies_that_include_the_subject_as_a_member-8  109905	     12629 ns/op	    3304 B/op	      86 allocs/op

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

func specificV2Input(subjects []string, resource string, action string, projects []string) ast.Value {
	subs := make(ast.Array, len(subjects))
	for i, sub := range subjects {
		subs[i] = ast.NewTerm(ast.String(sub))
	}
	projs := make(ast.Array, len(projects))
	for j, proj := range projects {
		projs[j] = ast.NewTerm(ast.String(proj))
	}
	return ast.NewObject(
		[2]*ast.Term{ast.NewTerm(ast.String("subjects")), ast.NewTerm(subs)},
		[2]*ast.Term{ast.NewTerm(ast.String("resource")), ast.NewTerm(ast.String(resource))},
		[2]*ast.Term{ast.NewTerm(ast.String("action")), ast.NewTerm(ast.String(action))},
		[2]*ast.Term{ast.NewTerm(ast.String("projects")), ast.NewTerm(projs)},
	)
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
			// generate between range 1..10 statements
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

			// generate between range 1..total number of members
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
