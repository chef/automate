package v2_test

import (
	"context"
	"math/rand"
	"testing"

	cache "github.com/patrickmn/go-cache"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"

	api "github.com/chef/automate/api/interservice/authz/v2"
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

func TestUpdateRule(t *testing.T) {
	ctx := context.Background()
	cl, store, _, _ := setupRules(t)

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
			resp, err := cl.UpdateRule(ctx, &api.UpdateRuleReq{
				Id:         "empty-name",
				Name:       "",
				ProjectId:  "foo",
				Conditions: apiConditions,
				Type:       api.ProjectRuleTypes_NODE,
			})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the rule id is empty, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.UpdateRule(ctx, &api.UpdateRuleReq{
				Id:         "",
				Name:       "empty id",
				ProjectId:  "foo",
				Conditions: apiConditions,
				Type:       api.ProjectRuleTypes_NODE,
			})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the rule id is invalid, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.UpdateRule(ctx, &api.UpdateRuleReq{
				Id:         "no_underscores",
				Name:       "any name",
				ProjectId:  "foo",
				Conditions: apiConditions,
				Type:       api.ProjectRuleTypes_NODE,
			})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if there are no conditions, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.UpdateRule(ctx, &api.UpdateRuleReq{
				Id:        "foo",
				Name:      "foo rule",
				ProjectId: "bar",
				Type:      api.ProjectRuleTypes_NODE,
			})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if a rule with that id does not exist, returns 'not found'", func(t *testing.T) {
			resp, err := cl.UpdateRule(ctx, &api.UpdateRuleReq{
				Id:         "not-found",
				Name:       "my other foo",
				ProjectId:  "bar",
				Conditions: apiConditions,
				Type:       api.ProjectRuleTypes_NODE,
			})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, resp)
		}},
		{"if the passed rule changes the project, returns 'failed precondition'", func(t *testing.T) {
			id := "foo-rule"
			addRuleToStore(t, store, id, "my foo rule", storage.Node, "foo-project", storageConditions)

			resp, err := cl.UpdateRule(ctx, &api.UpdateRuleReq{
				Id:         id,
				Name:       "my other foo",
				ProjectId:  "cannot-change",
				Conditions: apiConditions,
				Type:       api.ProjectRuleTypes_NODE,
			})
			grpctest.AssertCode(t, codes.FailedPrecondition, err)
			assert.Nil(t, resp)
		}},
		{"if the updated rule passes conditions not compatible with the type, throw an error", func(t *testing.T) {
			id := "foo-rule"
			addRuleToStore(t, store, id, "my foo rule", storage.Event, "foo-project", storageConditions)

			changedAPIConditions := []*api.Condition{
				{
					Type:     api.ProjectRuleConditionTypes_CHEF_TAGS,
					Values:   []string{"should-break"},
					Operator: api.ProjectRuleConditionOperators_EQUALS,
				},
			}

			resp, err := cl.UpdateRule(ctx, &api.UpdateRuleReq{
				Id:         id,
				Name:       "my other foo",
				ProjectId:  "foo-project",
				Conditions: changedAPIConditions,
				Type:       api.ProjectRuleTypes_EVENT,
			})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"with valid rule data, returns no error and updates the rule in storage", func(t *testing.T) {
			id := "foo-rule"
			projectID := "foo-project"
			addRuleToStore(t, store, id, "my foo rule", storage.Node, projectID, storageConditions)

			resp, err := cl.UpdateRule(ctx, &api.UpdateRuleReq{
				Id:        id,
				Name:      "updated name",
				ProjectId: projectID,
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
			assert.Equal(t, &api.UpdateRuleResp{
				Rule: &api.ProjectRule{
					Id:        id,
					Name:      "updated name",
					ProjectId: projectID,
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
			Operator:  storage.Equals,
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
		{"if the rule does not exist, returns 'not found'", func(t *testing.T) {
			resp, err := cl.GetRule(ctx, &api.GetRuleReq{Id: "foo"})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, resp)
		}},
		{"if there are multiple rules and one matches the requested ID, returns the matching rule", func(t *testing.T) {
			id := "foo-rule"
			projectID := "foo-project"
			name := "my coo foo rule"
			addRuleToStore(t, store, id, name, storage.Node, projectID, storageConditions)
			addRuleToStore(t, store, "bar-rule", "bar rule", storage.Event, projectID, storageConditions)
			expectedRule := api.ProjectRule{
				Id:         id,
				Name:       name,
				Type:       api.ProjectRuleTypes_NODE,
				ProjectId:  projectID,
				Conditions: apiConditions,
			}

			resp, err := cl.GetRule(ctx, &api.GetRuleReq{Id: id})
			require.NoError(t, err)
			assert.Equal(t, &expectedRule, resp.Rule)
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

func TestListRules(t *testing.T) {
	ctx := context.Background()
	cl, store, _, _ := setupRules(t)

	apiConditions1 := []*api.Condition{
		{
			Type:     api.ProjectRuleConditionTypes_CHEF_ORGS,
			Values:   []string{"opscode", "chef"},
			Operator: api.ProjectRuleConditionOperators_MEMBER_OF,
		},
	}
	storageConditions1 := []storage.Condition{
		{
			Type:      storage.Node,
			Attribute: storage.Organization,
			Operator:  storage.MemberOf,
			Value:     []string{"opscode", "chef"},
		},
	}
	apiConditions2 := []*api.Condition{
		{
			Type:     api.ProjectRuleConditionTypes_CHEF_ORGS,
			Values:   []string{"chef"},
			Operator: api.ProjectRuleConditionOperators_EQUALS,
		},
	}
	storageConditions2 := []storage.Condition{
		{
			Type:      storage.Event,
			Attribute: storage.Organization,
			Operator:  storage.Equals,
			Value:     []string{"chef"},
		},
	}

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"if no rules exist, returns empty list", func(t *testing.T) {
			resp, err := cl.ListRules(ctx, &api.ListRulesReq{})
			require.NoError(t, err)
			assert.Equal(t, &api.ListRulesResp{}, resp)
		}},
		{"if multiple rules exist, returns all rules", func(t *testing.T) {
			id1, id2 := "rule-number-1", "rule-number-2"
			projectID := "foo-project"
			name := "you don't talk about fight club"
			addRuleToStore(t, store, id1, name, storage.Node, projectID, storageConditions1)
			addRuleToStore(t, store, id2, name, storage.Event, projectID, storageConditions2)
			expected1 := api.ProjectRule{
				Id:         id1,
				Name:       name,
				Type:       api.ProjectRuleTypes_NODE,
				ProjectId:  projectID,
				Conditions: apiConditions1,
			}
			expected2 := api.ProjectRule{
				Id:         id2,
				Name:       name,
				Type:       api.ProjectRuleTypes_EVENT,
				ProjectId:  projectID,
				Conditions: apiConditions2,
			}
			expected := []*api.ProjectRule{&expected1, &expected2}

			resp, err := cl.ListRules(ctx, &api.ListRulesReq{})
			require.NoError(t, err)
			assert.ElementsMatch(t, expected, resp.Rules)
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

func TestDeleteRule(t *testing.T) {
	ctx := context.Background()
	cl, store, _, _ := setupRules(t)

	storageConditions1 := []storage.Condition{
		{
			Type:      storage.Node,
			Attribute: storage.Organization,
			Operator:  storage.MemberOf,
			Value:     []string{"opscode", "chef"},
		},
	}
	storageConditions2 := []storage.Condition{
		{
			Type:      storage.Event,
			Attribute: storage.Organization,
			Operator:  storage.Equals,
			Value:     []string{"chef"},
		},
	}

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"if the rule id is empty, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.DeleteRule(ctx, &api.DeleteRuleReq{Id: ""})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the rule id is invalid, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.DeleteRule(ctx, &api.DeleteRuleReq{Id: "no_underscore_allowed"})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the rule does not exist, returns 'not found'", func(t *testing.T) {
			resp, err := cl.DeleteRule(ctx, &api.DeleteRuleReq{Id: "foo"})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, resp)
		}},
		{"if there are multiple rules and one matches the requested ID, delete the matching rule", func(t *testing.T) {
			id1, id2 := "rule-number-1", "rule-number-2"
			projectID := "foo-project"
			name := "you don't talk about fight club"
			addRuleToStore(t, store, id1, name, storage.Node, projectID, storageConditions1)
			addRuleToStore(t, store, id2, name, storage.Event, projectID, storageConditions2)

			resp, err := cl.DeleteRule(ctx, &api.DeleteRuleReq{Id: id1})
			require.NoError(t, err)
			assert.Equal(t, &api.DeleteRuleResp{}, resp)

			rule, exists := store.Get(id1)
			assert.Nil(t, rule)
			assert.False(t, exists)
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
	conditions []storage.Condition) {
	t.Helper()

	rule := &storage.Rule{
		ID:         id,
		Name:       name,
		Type:       ruleType,
		ProjectID:  projectID,
		Conditions: conditions,
	}
	store.Add(id, rule, 0)
}

func setupRules(t *testing.T) (api.ProjectsClient, *cache.Cache, *mockEventServiceClient, int64) {
	cl, _, ca, mc, s := setupProjectsAndRules(t)
	return cl, ca, mc, s
}
