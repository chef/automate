package v2_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/logger"

	api_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/authz-service/engine/opa"
)

// In these tests, we assert that our default policies are in place, and do
// their job as expected. For this, we setup a v2 server using a proper OPA
// engine instance, and run a few queries.

func TestSystemPolicies(t *testing.T) {
	ctx := context.Background()
	ts := setupWithOPAV2(t)
	cl := ts.authz

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
	}
}

func TestFilterAuthorizedProjectsWithSystemPolicies(t *testing.T) {
	ctx := context.Background()
	ts := setupWithOPAV2p1(t)

	t.Run("user should only get projects they have non-system level access to", func(t *testing.T) {
		_, err := ts.projects.CreateProject(ctx, &api_v2.CreateProjectReq{
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
		_, err = ts.policy.CreatePolicy(ctx, &req)
		require.NoError(t, err)

		resp, err := ts.authz.FilterAuthorizedProjects(ctx,
			&api_v2.FilterAuthorizedProjectsReq{
				Subjects: []string{"user:local:alice"},
			})
		require.NoError(t, err)

		assert.ElementsMatch(t, []string{"project-1"}, resp.Projects)
	})
}

func TestListRulesForAllProjects(t *testing.T) {
	ctx := context.Background()
	ts := setupWithOPAV2p1(t)

	values1, values2, values3 := []string{"opscode", "chef"}, []string{"chef"}, []string{"other", "org"}
	type1 := api_v2.ProjectRuleTypes_NODE
	conditions1 := []*api_v2.Condition{
		{
			Type:     api_v2.ProjectRuleConditionTypes_CHEF_ORGS,
			Values:   values1,
			Operator: api_v2.ProjectRuleConditionOperators_MEMBER_OF,
		},
	}
	type2 := api_v2.ProjectRuleTypes_EVENT
	conditions2 := []*api_v2.Condition{
		{
			Type:     api_v2.ProjectRuleConditionTypes_CHEF_ORGS,
			Values:   values2,
			Operator: api_v2.ProjectRuleConditionOperators_EQUALS,
		},
	}
	conditions3 := []*api_v2.Condition{
		{
			Type:     api_v2.ProjectRuleConditionTypes_CHEF_ORGS,
			Values:   values3,
			Operator: api_v2.ProjectRuleConditionOperators_MEMBER_OF,
		},
	}

	t.Run("if no rules exist, returns empty list", func(t *testing.T) {
		list, err := ts.projects.ListRules(ctx, &api_v2.ListRulesReq{})
		require.Empty(t, list.Rules)
		require.NoError(t, err)

		resp, err := ts.projects.ListRulesForAllProjects(ctx, &api_v2.ListRulesForAllProjectsReq{})
		require.NoError(t, err)
		assert.Equal(t, &api_v2.ListRulesForAllProjectsResp{}, resp)
	})

	t.Run("if multiple rules exist, returns complete rule map", func(t *testing.T) {
		id1, id2, id3 := "rule-number-1", "rule-number-2", "rule-number-3"
		pid1, pid2 := "foo-project", "bar-project"
		name := "you don't talk about fight club"
		createResp1, err := ts.projects.CreateRule(ctx, &api_v2.CreateRuleReq{
			Id:         id1,
			Name:       name,
			Type:       type1,
			ProjectId:  pid1,
			Conditions: conditions1,
		})
		require.NoError(t, err)
		createResp2, err := ts.projects.CreateRule(ctx, &api_v2.CreateRuleReq{
			Id:         id2,
			Name:       name,
			Type:       type2,
			ProjectId:  pid1,
			Conditions: conditions2,
		})
		require.NoError(t, err)
		createResp3, err := ts.projects.CreateRule(ctx, &api_v2.CreateRuleReq{
			Id:         id3,
			Name:       name,
			Type:       type2,
			ProjectId:  pid2,
			Conditions: conditions3,
		})
		require.NoError(t, err)

		expectedRules1 := []*api_v2.ProjectRule{createResp1.Rule, createResp2.Rule}
		expectedRules2 := []*api_v2.ProjectRule{createResp3.Rule}
		expectedMap := map[string]*api_v2.ProjectRules{
			pid1: &api_v2.ProjectRules{
				Rules: expectedRules1,
			},
			pid2: &api_v2.ProjectRules{
				Rules: expectedRules2,
			},
		}

		list, err := ts.projects.ListRules(ctx, &api_v2.ListRulesReq{})
		require.Equal(t, 3, len(list.Rules))
		require.NoError(t, err)

		resp, err := ts.projects.ListRulesForAllProjects(ctx, &api_v2.ListRulesForAllProjectsReq{})
		require.NoError(t, err)
		assert.EqualValues(t, expectedMap, resp.ProjectRules)
	})
}

func setupWithOPAV2(t *testing.T) testSetup {
	return setupWithOPAV2pX(t, false)
}

func setupWithOPAV2p1(t *testing.T) testSetup {
	return setupWithOPAV2pX(t, true)
}

func setupWithOPAV2pX(t *testing.T, twoPointOne bool) testSetup {
	t.Helper()
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "init logger for storage+engine")

	o, err := opa.New(ctx, l)
	require.NoError(t, err, "init OPA")

	vChan := make(chan api_v2.Version, 1)
	emptyV1List := v1Lister{}
	ts := setupV2WithMigrationState(t, o, o, &emptyV1List, o, vChan, nil)
	var flag api_v2.Flag
	if twoPointOne {
		flag = api_v2.Flag_VERSION_2_1
	} else {
		flag = api_v2.Flag_VERSION_2_0
	}
	_, err = ts.policy.MigrateToV2(ctx, &api_v2.MigrateToV2Req{Flag: flag})
	require.NoError(t, err)
	return ts
}
