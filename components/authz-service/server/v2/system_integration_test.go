package v2_test

import (
	"context"
	"fmt"
	"math/rand"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	api_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/authz-service/config"
	"github.com/chef/automate/components/authz-service/testhelpers"
	project_update_tags "github.com/chef/automate/lib/authz"
	event_ids "github.com/chef/automate/lib/event"
)

// In these tests, we assert that our default policies are in place, and do
// their job as expected. For this, we setup a v2 server using a proper OPA
// engine instance, and run a few queries.

func TestIntegrationSystemPolicies(t *testing.T) {
	// TODO (tc): Do we need to set up a new TestFramework instance per test?
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	ts := setupWithOPAV2p1(t)
	defer ts.Shutdown(t, ctx)
	cl := ts.Authz

	isAuthorized := func(subject, action, resource string) func(*testing.T) {
		return func(t *testing.T) {
			resp, err := cl.IsAuthorized(ctx, &api_v2.IsAuthorizedReq{Subjects: []string{subject}, Resource: resource, Action: action})
			require.NoError(t, err)
			assert.True(t, resp.Authorized)
		}
	}

	cases := map[string]func(t *testing.T){
		"service version":                       isAuthorized("user:ldap:alice", "system:serviceVersion:get", "system:service:version"),
		"introspect all":                        isAuthorized("user:ldap:alice", "iam:introspect:getAll", "iam:introspect"),
		"introspect some":                       isAuthorized("user:ldap:alice", "iam:introspect:getSome", "iam:introspect"),
		"introspect get":                        isAuthorized("user:ldap:alice", "iam:introspect:get", "iam:introspect"),
		"get user record":                       isAuthorized("user:local:alice", "iam:users:get", "iam:users:alice"),
		"list user record":                      isAuthorized("user:local:alice", "iam:users:list", "iam:users:alice"),
		"d-s can do allthethings":               isAuthorized("tls:service:deployment-service:cert-id", "iam:users:delete", "iam:users:alice"),
		"ingest run as provider oc-erchef":      isAuthorized("tls:service:automate-cs-oc-erchef:cert", "infra:ingest:create", "infra:nodes:nodeUUID:runs"),
		"ingest action as provider oc-erchef":   isAuthorized("tls:service:automate-cs-oc-erchef:cert", "infra:ingest:create", "infra:actions"),
		"ingest delete as provider oc-erchef":   isAuthorized("tls:service:automate-cs-oc-erchef:cert", "infra:ingest:delete", "infra:nodes"),
		"ingest liveness as provider oc-erchef": isAuthorized("tls:service:automate-cs-oc-erchef:cert", "infra:ingest:create", "infra:nodes:nodeUUID:liveness"),
		"ingest run as provider cs-nginx":       isAuthorized("tls:service:automate-cs-nginx:cert", "infra:ingest:create", "infra:nodes:nodeUUID:runs"),
		"ingest action as provider nginx":       isAuthorized("tls:service:automate-cs-nginx:cert", "infra:ingest:create", "infra:actions"),
		"ingest delete as provider nginx":       isAuthorized("tls:service:automate-cs-nginx:cert", "infra:ingest:delete", "infra:nodes"),
		"ingest liveness as provider nginx":     isAuthorized("tls:service:automate-cs-nginx:cert", "infra:ingest:create", "infra:nodes:nodeUUID:liveness"),
	}

	for desc, test := range cases {
		t.Run(desc, test)
		ts.TestDB.Flush(t)
	}
}

func TestIntegrationFilterAuthorizedProjectsWithSystemPolicies(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	ts := setupWithOPAV2p1(t)
	defer ts.Shutdown(t, ctx)

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"user should only get projects they have non-system level access to", func(t *testing.T) {
			_, err := ts.Projects.CreateProject(ctx, &api_v2.CreateProjectReq{
				Id:   "project-1",
				Name: "name1",
			})
			require.NoError(t, err)

			statement := api_v2.Statement{
				Effect:    api_v2.Statement_ALLOW,
				Resources: []string{"infra:nodes:*"},
				Actions:   []string{"infra:nodes:get", "infra:nodes:list"},
				Projects:  []string{"project-1"},
			}
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"user:local:alice"},
				Statements: []*api_v2.Statement{&statement},
			}
			_, err = ts.Policy.CreatePolicy(ctx, &req)
			require.NoError(t, err)

			// force sync refresh
			err = ts.PolicyRefresher.Refresh(ctx)
			require.NoError(t, err)

			resp, err := ts.Authz.FilterAuthorizedProjects(ctx,
				&api_v2.FilterAuthorizedProjectsReq{
					Subjects: []string{"user:local:alice"},
				})
			require.NoError(t, err)

			assert.ElementsMatch(t, []string{"project-1"}, resp.Projects)
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
	}
}

func TestIntegrationRuleApplyAndList(t *testing.T) {
	values1, values2, values3 := []string{"opscode", "chef"}, []string{"chef"}, []string{"other", "org"}
	type1 := api_v2.ProjectRuleTypes_NODE
	conditions1 := []*api_v2.Condition{
		{
			Attribute: api_v2.ProjectRuleConditionAttributes_CHEF_ORGS,
			Values:    values1,
			Operator:  api_v2.ProjectRuleConditionOperators_MEMBER_OF,
		},
	}
	type2 := api_v2.ProjectRuleTypes_EVENT
	conditions2 := []*api_v2.Condition{
		{
			Attribute: api_v2.ProjectRuleConditionAttributes_CHEF_ORGS,
			Values:    values2,
			Operator:  api_v2.ProjectRuleConditionOperators_EQUALS,
		},
	}
	conditions3 := []*api_v2.Condition{
		{
			Attribute: api_v2.ProjectRuleConditionAttributes_CHEF_ORGS,
			Values:    values3,
			Operator:  api_v2.ProjectRuleConditionOperators_MEMBER_OF,
		},
	}

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"when no rules exist, returns an empty list", func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			ts := setupWithOPAV2p1(t)
			defer ts.Shutdown(t, ctx)
			resp, err := ts.Projects.ApplyRulesStart(ctx, &api_v2.ApplyRulesStartReq{})
			assert.Equal(t, &api_v2.ApplyRulesStartResp{}, resp)
			assert.NoError(t, err)
			rules, err := ts.Engine.ListProjectMappings(ctx)
			assert.NoError(t, err)
			assert.Equal(t, 0, len(rules))
		}},
		{"when applied rules exist, returns a rule map with all applied rules", func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			ts := setupWithOPAV2p1(t)
			defer ts.Shutdown(t, ctx)
			id1, id2, id3 := "rule-number-1", "rule-number-2", "rule-number-3"
			pid1, pid2 := "foo-project", "bar-project"
			name := "you don't talk about fight club"

			_, err := ts.Projects.CreateProject(ctx, &api_v2.CreateProjectReq{
				Id:   pid1,
				Name: name,
			})
			require.NoError(t, err)

			_, err = ts.Projects.CreateProject(ctx, &api_v2.CreateProjectReq{
				Id:   pid2,
				Name: name,
			})
			require.NoError(t, err)

			createResp1, err := ts.Projects.CreateRule(ctx, &api_v2.CreateRuleReq{
				Id:         id1,
				Name:       name,
				Type:       type1,
				ProjectId:  pid1,
				Conditions: conditions1,
			})
			require.NoError(t, err)
			createResp2, err := ts.Projects.CreateRule(ctx, &api_v2.CreateRuleReq{
				Id:         id2,
				Name:       name,
				Type:       type2,
				ProjectId:  pid1,
				Conditions: conditions2,
			})
			require.NoError(t, err)
			createResp3, err := ts.Projects.CreateRule(ctx, &api_v2.CreateRuleReq{
				Id:         id3,
				Name:       name,
				Type:       type2,
				ProjectId:  pid2,
				Conditions: conditions3,
			})
			require.NoError(t, err)

			createResp1.Rule.Status = "applied"
			createResp2.Rule.Status = "applied"
			createResp3.Rule.Status = "applied"
			expectedRules1 := []*api_v2.ProjectRule{createResp1.Rule, createResp2.Rule}
			expectedRules2 := []*api_v2.ProjectRule{createResp3.Rule}

			// Before rule apply
			list, err := ts.Projects.ListRules(ctx, &api_v2.ListRulesReq{})
			require.Equal(t, 0, len(list.Rules))
			require.NoError(t, err)
			beforeResp, err := ts.Projects.ListRulesForAllProjects(ctx, &api_v2.ListRulesForAllProjectsReq{})
			require.NoError(t, err)
			require.Equal(t, 0, len(beforeResp.ProjectRules))

			// After rule apply
			_, err = ts.Projects.ApplyRulesStart(ctx, &api_v2.ApplyRulesStartReq{})
			assert.NoError(t, err)

			resp, err := ts.Projects.ListRulesForAllProjects(ctx, &api_v2.ListRulesForAllProjectsReq{})
			require.NoError(t, err)
			actualPid1Rules, ok := resp.ProjectRules[pid1]
			require.True(t, ok)
			actualPid2Rules, ok := resp.ProjectRules[pid2]
			require.True(t, ok)
			rulesEqual(t, expectedRules1, actualPid1Rules.Rules)
			rulesEqual(t, expectedRules2, actualPid2Rules.Rules)
		}},
		{"when some rule updates are applied, returns updated rule map", func(t *testing.T) {

			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			ts := setupWithOPAV2p1(t)
			defer ts.Shutdown(t, ctx)
			id1, id2, id3 := "rule-number-1", "rule-number-2", "rule-number-3"
			pid1, pid2 := "foo-project", "bar-project"
			name := "you don't talk about fight club"

			_, err := ts.Projects.CreateProject(ctx, &api_v2.CreateProjectReq{
				Id:   pid1,
				Name: name,
			})
			require.NoError(t, err)

			_, err = ts.Projects.CreateProject(ctx, &api_v2.CreateProjectReq{
				Id:   pid2,
				Name: name,
			})
			require.NoError(t, err)

			createResp1, err := ts.Projects.CreateRule(ctx, &api_v2.CreateRuleReq{
				Id:         id1,
				Name:       name,
				Type:       type1,
				ProjectId:  pid1,
				Conditions: conditions1,
			})
			require.NoError(t, err)
			createResp2, err := ts.Projects.CreateRule(ctx, &api_v2.CreateRuleReq{
				Id:         id2,
				Name:       name,
				Type:       type1,
				ProjectId:  pid1,
				Conditions: conditions2,
			})
			require.NoError(t, err)
			createResp3, err := ts.Projects.CreateRule(ctx, &api_v2.CreateRuleReq{
				Id:         id3,
				Name:       name,
				Type:       type2,
				ProjectId:  pid2,
				Conditions: conditions3,
			})
			require.NoError(t, err)

			// Apply rules
			_, err = ts.Projects.ApplyRulesStart(ctx, &api_v2.ApplyRulesStartReq{})
			assert.NoError(t, err)

			testhelpers.WaitForWithTimeout(t, func() bool {
				return config.RunningState == ts.ProjectUpdateManager.State()
			}, time.Second*3, "State did not switch to Running")

			createResp1.Rule.Status = "applied"
			createResp2.Rule.Status = "applied"
			createResp3.Rule.Status = "applied"
			expectedRules1 := []*api_v2.ProjectRule{createResp1.Rule, createResp2.Rule}
			expectedRules2 := []*api_v2.ProjectRule{createResp3.Rule}

			resp, err := ts.Projects.ListRulesForAllProjects(ctx, &api_v2.ListRulesForAllProjectsReq{})
			require.NoError(t, err)
			actualPid1Rules, ok := resp.ProjectRules[pid1]
			require.True(t, ok)
			actualPid2Rules, ok := resp.ProjectRules[pid2]
			require.True(t, ok)
			rulesEqual(t, expectedRules1, actualPid1Rules.Rules)
			rulesEqual(t, expectedRules2, actualPid2Rules.Rules)

			// Manually trigger completed state and wait for it to be true
			eventData := ts.LatestEvent.Data
			projectUpdateIDTag := eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

			infraStatusEvent := testhelpers.CreateStatusEventMsg(projectUpdateIDTag,
				0.0,  // EstimatedTimeCompleteInSec
				1.0,  // percentageComplete
				true, // completed
				event_ids.InfraClientRunsProducerID)
			err = ts.ProjectUpdateManager.ProcessStatusEvent(infraStatusEvent)
			assert.NoError(t, err)

			eventData = ts.LatestEvent.Data
			projectUpdateIDTag = eventData.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue()

			complianceStatusEvent := testhelpers.CreateStatusEventMsg(projectUpdateIDTag,
				0.0,  // EstimatedTimeCompleteInSec
				1.0,  // percentageComplete
				true, // completed
				event_ids.ComplianceInspecReportProducerID)
			err = ts.ProjectUpdateManager.ProcessStatusEvent(complianceStatusEvent)
			assert.NoError(t, err)

			testhelpers.WaitForWithTimeout(t, func() bool {
				return config.NotRunningState == ts.ProjectUpdateManager.State()
			}, time.Second*3, "State did not switch to Running")

			// Update and delete rules
			_, err = ts.Projects.DeleteRule(ctx, &api_v2.DeleteRuleReq{Id: id1, ProjectId: pid1})
			require.NoError(t, err)

			newConditions := []*api_v2.Condition{
				{
					Attribute: api_v2.ProjectRuleConditionAttributes_CHEF_TAGS,
					Values:    []string{"test_tag1", "test_tag2"},
					Operator:  api_v2.ProjectRuleConditionOperators_MEMBER_OF,
				},
				{
					Attribute: api_v2.ProjectRuleConditionAttributes_CHEF_ORGS,
					Values:    []string{"test_org"},
					Operator:  api_v2.ProjectRuleConditionOperators_EQUALS,
				},
			}

			_, err = ts.Projects.UpdateRule(ctx, &api_v2.UpdateRuleReq{
				Id:         createResp2.Rule.Id,
				ProjectId:  createResp2.Rule.ProjectId,
				Name:       "this has been updated",
				Type:       createResp2.Rule.Type,
				Conditions: newConditions,
			})
			require.NoError(t, err)

			// Re-apply and check cache
			_, err = ts.Projects.ApplyRulesStart(ctx, &api_v2.ApplyRulesStartReq{})
			assert.NoError(t, err)

			expectedAfterApplyRules1 := []*api_v2.ProjectRule{
				&api_v2.ProjectRule{
					Id:         createResp2.Rule.Id,
					ProjectId:  createResp2.Rule.ProjectId,
					Name:       "this has been updated",
					Type:       createResp2.Rule.Type,
					Conditions: newConditions,
					Status:     "applied",
				},
			}
			expectedAfterApplyRules2 := []*api_v2.ProjectRule{createResp3.Rule}

			resp2, err := ts.Projects.ListRulesForAllProjects(ctx, &api_v2.ListRulesForAllProjectsReq{})
			require.NoError(t, err)
			actualAfterApplyPid1Rules, ok := resp2.ProjectRules[pid1]
			require.True(t, ok)
			actualAfterApplyPid2Rules, ok := resp2.ProjectRules[pid2]
			require.True(t, ok)
			rulesEqual(t, expectedAfterApplyRules1, actualAfterApplyPid1Rules.Rules)
			rulesEqual(t, expectedAfterApplyRules2, actualAfterApplyPid2Rules.Rules)
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
	}
}

func rulesEqual(t *testing.T, expected, actual []*api_v2.ProjectRule) {
	t.Helper()
	if len(expected) != len(actual) {
		t.FailNow()
		fmt.Printf("expected len %d != actual len %d", len(expected), len(actual))
	}

	expectedMap := make(map[string]*api_v2.ProjectRule, len(expected))
	for _, expectedRule := range expected {
		expectedMap[expectedRule.Id] = expectedRule
	}
	for _, actualRule := range actual {
		expectedRule := expectedMap[actualRule.Id]
		assert.Equal(t, expectedRule.Id, actualRule.Id)
		assert.Equal(t, expectedRule.ProjectId, actualRule.ProjectId)
		assert.Equal(t, expectedRule.Type, actualRule.Type)
		assert.Equal(t, expectedRule.Deleted, actualRule.Deleted)
		assert.Equal(t, expectedRule.Status, actualRule.Status)

		expectedConditions := expectedRule.Conditions
		actualConditions := actualRule.Conditions

		if len(expectedConditions) != len(actualConditions) {
			t.FailNow()
			fmt.Printf("expected len %d != actual len %d", len(expectedConditions), len(actualConditions))
		}

		for i, expectedCondition := range expectedConditions {
			actualCondition := actualConditions[i]
			assert.Equal(t, expectedCondition.Attribute, actualCondition.Attribute)
			assert.Equal(t, expectedCondition.Values, actualCondition.Values)
			assert.Equal(t, expectedCondition.Operator, actualCondition.Operator)
		}
	}
}

func setupWithOPAV2(t *testing.T) *testhelpers.TestFramework {
	return setupWithOPAV2pX(t, false)
}

func setupWithOPAV2p1(t *testing.T) *testhelpers.TestFramework {
	return setupWithOPAV2pX(t, true)
}

func setupWithOPAV2pX(t *testing.T, twoPointOne bool) *testhelpers.TestFramework {
	t.Helper()
	ctx := context.Background()

	tf := testhelpers.NewTestFramework(t, ctx)
	var flag api_v2.Flag
	if twoPointOne {
		flag = api_v2.Flag_VERSION_2_1
	} else {
		flag = api_v2.Flag_VERSION_2_0
	}
	_, err := tf.Policy.MigrateToV2(ctx, &api_v2.MigrateToV2Req{
		Flag:           flag,
		SkipV1Policies: true,
	})
	require.NoError(t, err)
	return tf
}
