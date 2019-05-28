package v2_test

import (
	"context"
	"math/rand"
	"testing"

	cache "github.com/patrickmn/go-cache"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"

	api "github.com/chef/automate/api/interservice/authz/v2"
	v2 "github.com/chef/automate/components/authz-service/server/v2"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/lib/grpc/grpctest"
)

func TestCreateRule(t *testing.T) {
	ctx := context.Background()
	cl, store, _, _ := setupRules(t)

	// it's cumbersome to set this up, so we re-use it in a few of the following
	// cases
	apiConditions := []*api.Condition{
		{
			Type:     api.ProjectRuleConditionTypes_CHEF_ORGS,
			Values:   []string{"opscode"},
			Operator: api.ProjectRuleConditionOperators_EQUALS,
		},
	}
	storageConditions := []storage.Condition{
		{
			Type:      storage.Node,
			Attribute: storage.Organization,
			Operator:  storage.MemberOf,
			Value:     []string{"opscode"},
		},
	}

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"if the rule name is empty, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.CreateRule(ctx, &api.CreateRuleReq{
				Id:         "empty-name",
				Name:       "",
				ProjectId:  "foo",
				Conditions: apiConditions,
			})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the rule id is empty, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.CreateRule(ctx, &api.CreateRuleReq{
				Id:         "",
				Name:       "empty id",
				ProjectId:  "foo",
				Conditions: apiConditions,
			})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the rule id is invalid, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.CreateRule(ctx, &api.CreateRuleReq{
				Id:         "no_underscores",
				Name:       "any name",
				ProjectId:  "foo",
				Conditions: apiConditions,
			})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the passed rule project id is invalid, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.CreateRule(ctx, &api.CreateRuleReq{
				Id:         "any-name",
				Name:       "any name",
				ProjectId:  "foo bar",
				Conditions: apiConditions,
			})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if there are no conditions, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.CreateRule(ctx, &api.CreateRuleReq{
				Id:        "foo",
				Name:      "foo rule",
				ProjectId: "bar",
			})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if a rule with that id already exists, returns 'already exists'", func(t *testing.T) {
			id := "foo-rule"
			addRuleToStore(t, store, id, "my foo rule", storage.Node, "foo-project", storageConditions)

			resp, err := cl.CreateRule(ctx, &api.CreateRuleReq{
				Id:         id,
				Name:       "my other foo",
				ProjectId:  "bar",
				Conditions: apiConditions,
			})
			grpctest.AssertCode(t, codes.AlreadyExists, err)
			assert.Nil(t, resp)
		}},
		// happy path
		{"with valid rule data, returns no error and creates the rule in storage", func(t *testing.T) {
			resp, err := cl.CreateRule(ctx, &api.CreateRuleReq{
				Id:        "any-name",
				Name:      "any name",
				ProjectId: "foo",
				Type:      api.ProjectRuleTypes_NODE,
				Conditions: []*api.Condition{
					{
						Type:     api.ProjectRuleConditionTypes_CHEF_ORGS,
						Values:   []string{"chef"},
						Operator: api.ProjectRuleConditionOperators_EQUALS,
					},
					{
						Type:     api.ProjectRuleConditionTypes_CHEF_TAGS,
						Values:   []string{"tag1", "tag2"},
						Operator: api.ProjectRuleConditionOperators_MEMBER_OF,
					},
				},
			})
			assert.NoError(t, err)
			assert.Equal(t, &api.CreateRuleResp{
				Rule: &api.ProjectRule{
					Id:        "any-name",
					Name:      "any name",
					ProjectId: "foo",
					Type:      api.ProjectRuleTypes_NODE,
					Conditions: []*api.Condition{
						{
							Type:     api.ProjectRuleConditionTypes_CHEF_ORGS,
							Values:   []string{"chef"},
							Operator: api.ProjectRuleConditionOperators_EQUALS,
						},
						{
							Type:     api.ProjectRuleConditionTypes_CHEF_TAGS,
							Values:   []string{"tag1", "tag2"},
							Operator: api.ProjectRuleConditionOperators_MEMBER_OF,
						},
					},
				},
			}, resp)
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		store.Flush()
	}
}

func TestGetRule(t *testing.T) {
	ctx := context.Background()
	cl, store, _, _ := setupRules(t)

	storageConditions := []storage.Condition{
		{
			Type:      storage.Node,
			Attribute: storage.Organization,
			Operator:  storage.MemberOf,
			Value:     []string{"opscode"},
		},
	}

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"if the rule id is empty, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.GetRule(ctx, &api.GetRuleReq{Id: ""})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the rule id is invalid, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.GetRule(ctx, &api.GetRuleReq{Id: "no_underscore_allowed"})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the rule does not exist, returns code 'not found'", func(t *testing.T) {
			resp, err := cl.GetRule(ctx, &api.GetRuleReq{Id: "foo"})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, resp)
		}},
		{"if a rule with the requested id exists, returns the rule", func(t *testing.T) {
			id := "foo-rule"
			apiRule, err := addRuleToStore(t, store, id, "my coo foo rule", storage.Node, "foo-project", storageConditions)
			require.NoError(t, err)

			resp, err := cl.GetRule(ctx, &api.GetRuleReq{Id: id})

			require.NoError(t, err)
			assert.Equal(t, &apiRule, resp.Rule)
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		store.Flush()
	}
}

func addRuleToStore(t *testing.T, store *cache.Cache, id, name string, ruleType storage.RuleType, projectID string,
	conditions []storage.Condition) (api.ProjectRule, error) {
	t.Helper()

	rule := &storage.Rule{
		ID:         id,
		Name:       name,
		Type:       ruleType,
		ProjectID:  projectID,
		Conditions: conditions,
	}
	store.Add(id, rule, 0)

	returnType := api.ProjectRuleTypes_NODE
	if ruleType == storage.Event {
		returnType = api.ProjectRuleTypes_EVENT
	}

	returnConditions, err := v2.FromStorageConditions(conditions)
	if err != nil {
		return api.ProjectRule{}, errors.Errorf("failed to convert conditions: %s", err)
	}

	return api.ProjectRule{
		Id:         id,
		Name:       name,
		Type:       returnType,
		ProjectId:  projectID,
		Conditions: returnConditions,
	}, nil
}

func setupRules(t *testing.T) (api.ProjectsClient, *cache.Cache, *mockEventServiceClient, int64) {
	cl, _, ca, mc, s := setupProjectsAndRules(t)
	return cl, ca, mc, s
}
