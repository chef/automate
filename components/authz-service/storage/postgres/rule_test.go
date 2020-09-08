package postgres_test

import (
	"context"
	"math/rand"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/testhelpers"
)

func TestCreateRule(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := context.Background()

	cases := map[string]func(*testing.T){
		"when the project doesn't exist, return ForeignKeyError": func(t *testing.T) {
			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("new-id-1", "project-not-found", "name", storage.Node, []storage.Condition{condition1})
			require.NoError(t, err)

			resp, err := store.CreateRule(ctx, &rule)
			require.Error(t, err)
			assert.Nil(t, resp)
			_, ok := err.(*storage.ForeignKeyError)
			require.True(t, ok, "mismatches expected error type")
			assert.Equal(t, "project not found: project-not-found", err.Error())
		},
		"when rule exists in the applied rules table, return error": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule := insertAppliedRuleWithMultipleConditions(t, db, "copy", projID, storage.Node)

			resp, err := store.CreateRule(ctx, rule)
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrConflict, err)
		},
		"when rule exists in the staging rules table, return error": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule := insertStagedRuleWithMultipleConditions(t, db, "copy", projID, storage.Node, false)

			resp, err := store.CreateRule(ctx, rule)
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrConflict, err)
		},
		"cannot use improper condition attributes for events": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			condition, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefTag, storage.MemberOf)
			require.NoError(t, err)
			_, err = storage.NewRule("new-rule", projID, "name", storage.Event, []storage.Condition{condition})
			assert.Error(t, err)
		},
		"creating a condition with zero entries for the 'equals' operator returns an error": func(t *testing.T) {
			condition1, err := storage.NewCondition([]string{}, storage.ChefServer, storage.Equals)
			assert.Equal(t, storage.Condition{}, condition1)
			assert.Error(t, err)
		},
		"creating a condition with zero entries for the 'member-of' operator returns an error": func(t *testing.T) {
			condition1, err := storage.NewCondition([]string{}, storage.ChefServer, storage.MemberOf)
			assert.Equal(t, storage.Condition{}, condition1)
			assert.Error(t, err)
		},
		"creating an equals condition with multiple entries returns an error": func(t *testing.T) {
			condition1, err := storage.NewCondition([]string{"chef-server-1", "chef-server-2"}, storage.ChefServer, storage.Equals)
			assert.Equal(t, storage.Condition{}, condition1)
			assert.Error(t, err)
		},
		"creating a condition with multiple entries for 'member-of' operator is allowed": func(t *testing.T) {
			condition1, err := storage.NewCondition([]string{"1", "2", "3"}, storage.ChefServer, storage.MemberOf)
			assert.NotNil(t, condition1)
			require.NoError(t, err)
		},
		"creating a condition with a single entry for 'member-of' operator is allowed": func(t *testing.T) {
			condition1, err := storage.NewCondition([]string{"1"}, storage.ChefServer, storage.MemberOf)
			assert.NotNil(t, condition1)
			require.NoError(t, err)
		},
		"create node rule with multiple conditions": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ruleType := storage.Node
			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			condition2, err := storage.NewCondition([]string{"org1", "org2", "org3"}, storage.Organization, storage.MemberOf)
			require.NoError(t, err)
			condition3, err := storage.NewCondition([]string{"role1"}, storage.ChefRole, storage.MemberOf)
			require.NoError(t, err)
			ruleID := "new-id-1"
			rule, err := storage.NewRule(ruleID, "project-1", "name", ruleType,
				[]storage.Condition{condition1, condition2, condition3})
			require.NoError(t, err)

			resp, err := store.CreateRule(ctx, &rule)
			require.NoError(t, err)
			require.Equal(t, &rule, resp)
			assertCount(t, 3, db.QueryRow(
				`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`, ruleID))
		},
		"create event rule with multiple conditions": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ruleType := storage.Event
			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			condition2, err := storage.NewCondition([]string{"org1", "org2", "org3"}, storage.Organization, storage.MemberOf)
			require.NoError(t, err)
			condition3, err := storage.NewCondition([]string{"chef-server-2", "chef-server-3"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			ruleID := "new-id-1"
			rule, err := storage.NewRule(ruleID, "project-1", "name", ruleType,
				[]storage.Condition{condition1, condition2, condition3})
			require.NoError(t, err)

			resp, err := store.CreateRule(ctx, &rule)
			require.NoError(t, err)
			require.Equal(t, &rule, resp)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND type=$2
				AND project_id=project_db_id($3) AND name=$4 AND deleted=$5`,
				rule.ID, rule.Type.String(), rule.ProjectID, rule.Name, false))
			assertCount(t, 3, db.QueryRow(
				`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`, ruleID))
		},
		"when the project filter does not allow the given project, return ErrNotFound": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"not-a-match", "some-other-project"})

			condition, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			ruleID := "new-id-1"
			rule, err := storage.NewRule(ruleID, "project-1", "name", storage.Event, []storage.Condition{condition})
			require.NoError(t, err)

			resp, err := store.CreateRule(ctx, &rule)

			assert.Nil(t, resp)
			assert.Regexp(t, "project with ID.*not found", err.Error())
		},
		"when the project filter allows all projects, rule gets created": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"project-1", "project-2", "project-3"})

			condition, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			ruleID := "new-id-1"
			rule, err := storage.NewRule(ruleID, "project-1", "name", storage.Event, []storage.Condition{condition})
			require.NoError(t, err)

			resp, err := store.CreateRule(ctx, &rule)
			require.NoError(t, err)
			require.Equal(t, &rule, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestListRules(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"when no rules exist, returns an empty list": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.ListRules(ctx)
			require.NoError(t, err)
			assert.Nil(t, resp)
		},
		"when only staged rules exist, returns an empty list": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			insertStagedRuleWithMultipleConditions(t, db, "staged-rule", projID, storage.Node, false)
			resp, err := store.ListRules(ctx)
			require.NoError(t, err)
			assert.Nil(t, resp)
		},
		"when multiple rules exist with no project filter, returns the full list": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Event)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID, storage.Node)

			resp, err := store.ListRules(ctx)
			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Rule{rule1, rule2}, resp)
		},
		"when staged and applied rules exist with no project filter, returns applied rules": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule1", projID, storage.Node)
			insertStagedRuleWithMultipleConditions(t, db, "rule2", projID, storage.Event, false)

			resp, err := store.ListRules(ctx)
			require.NoError(t, err)
			require.NotZero(t, len(resp))
			assert.ElementsMatch(t, []*storage.Rule{rule1}, resp)
		},
		"when multiple rules exist with a project filter, returns filtered list": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"project-3", projID2})

			ruleType := storage.Node
			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, ruleType)

			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID2, ruleType)

			resp, err := store.ListRules(ctx)
			require.NoError(t, err)
			require.NotZero(t, len(resp))
			assert.ElementsMatch(t, []*storage.Rule{rule2}, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestListStagedAndAppliedRules(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"when no rules exist, returns an empty list": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.ListRules(ctx)
			require.NoError(t, err)
			assert.Nil(t, resp)
			assert.Zero(t, len(resp))
		},
		"when multiple staged and applied rules exist with no project filter, returns the full list": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID, storage.Event)

			rule3 := insertStagedRuleWithMultipleConditions(t, db, "rule-3", projID, storage.Node, false)
			rule4 := insertStagedRuleWithMultipleConditions(t, db, "rule-4", projID, storage.Node, false)

			resp, err := store.ListStagedAndAppliedRules(ctx)
			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Rule{rule1, rule2, rule3, rule4}, resp)
		},
		"when multiple staged and applied rules exist with a project filter, returns filtered list": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"project-3", projID2})

			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule", projID, storage.Node)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "applied-rule2", projID2, storage.Event)

			insertStagedRuleWithMultipleConditions(t, db, "staged-rule", projID, storage.Event, false)
			rule4 := insertStagedRuleWithMultipleConditions(t, db, "staged-rule4", projID2, storage.Node, false)

			resp, err := store.ListStagedAndAppliedRules(ctx)
			require.NoError(t, err)
			require.NotZero(t, len(resp))
			assert.ElementsMatch(t, []*storage.Rule{rule2, rule4}, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestListRulesForProject(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"when no rules exist and requested project does not exist, returns ForeignKeyError", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "some-project", "some other project", storage.Custom)

			resp, status, err := store.ListRulesForProject(ctx, "project-not-found")
			require.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage.RulesStatusError, status)
			_, ok := err.(*storage.ForeignKeyError)
			require.True(t, ok, "mismatches expected error type")
			assert.Equal(t, "project not found: project-not-found", err.Error())
		}},
		{"when requested project does not exist, returns ForeignKeyError", func(t *testing.T) {
			ctx := context.Background()
			projID := "some-project"
			insertTestProject(t, db, projID, "some other project", storage.Custom)
			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)

			resp, status, err := store.ListRulesForProject(ctx, "project-not-found")
			require.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage.RulesStatusError, status)
			_, ok := err.(*storage.ForeignKeyError)
			require.True(t, ok, "mismatches expected error type")
			assert.Equal(t, "project not found: project-not-found", err.Error())
		}},
		{"when project exists but no rules exist, returns an empty list", func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			resp, status, err := store.ListRulesForProject(ctx, projID)
			require.NoError(t, err)
			assert.Nil(t, resp)
			assert.Zero(t, len(resp))
			assert.Equal(t, storage.NoRules, status)
		}},
		{"when rules exist but not for the project queried, returns an empty list", func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)

			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID2, storage.Node)

			resp, status, err := store.ListRulesForProject(ctx, projID)
			require.NoError(t, err)
			assert.Nil(t, resp)
			assert.Zero(t, len(resp))
			assert.Equal(t, storage.NoRules, status)
		}},
		{"when multiple applied rules exist with no project filter, returns rules for the project", func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)

			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Event)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID2, storage.Node)
			rule3 := insertAppliedRuleWithMultipleConditions(t, db, "rule-3", projID2, storage.Event)

			resp, status, err := store.ListRulesForProject(ctx, projID2)
			require.NoError(t, err)
			assert.Equal(t, 2, len(resp))
			assert.ElementsMatch(t, []*storage.Rule{rule2, rule3}, resp)
			assert.Equal(t, storage.Applied, status)
		}},
		{"when the requested project is in the filter, returns the rules for the project", func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"project-3", projID2})

			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID2, storage.Event)
			rule3 := insertAppliedRuleWithMultipleConditions(t, db, "rule-3", projID2, storage.Event)

			resp, status, err := store.ListRulesForProject(ctx, projID2)
			require.NoError(t, err)
			assert.Equal(t, 2, len(resp))
			assert.ElementsMatch(t, []*storage.Rule{rule2, rule3}, resp)
			assert.Equal(t, storage.Applied, status)
		}},
		{"when the requested project is not in the filter, returns ErrNotFound", func(t *testing.T) {
			ctx := context.Background()
			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"project-3", "project-4"})
			ruleType := storage.Event
			insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID2, ruleType)
			insertAppliedRuleWithMultipleConditions(t, db, "rule-3", projID2, ruleType)

			resp, status, err := store.ListRulesForProject(ctx, projID2)
			require.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage.RulesStatusError, status)
			assert.Equal(t, storage.ErrNotFound, err)
		}},
		{"when there are only staged changes for the project's rules, returns the staged versions of the rules", func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "first project", storage.Custom)

			condition, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("first-rule", projID, "the very first rule", storage.Node,
				[]storage.Condition{condition})
			require.NoError(t, err)
			insertAppliedRule(t, db, &rule)

			updatedCondition, err := storage.NewCondition([]string{"new-chef-role"}, storage.ChefRole, storage.Equals)
			require.NoError(t, err)
			updatedRule, err := storage.NewRule(rule.ID, projID, "updated rule name", rule.Type,
				[]storage.Condition{updatedCondition})
			insertStagedRule(t, db, &updatedRule, false)

			resp, status, err := store.ListRulesForProject(ctx, projID)
			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Rule{&updatedRule}, resp)
			assert.Equal(t, storage.EditsPending, status)

		}},
		{"when there are staged changes for some of the project's rules, returns those staged versions and rules that have no staged changes", func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "first project", storage.Custom)

			condition, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("first-rule", projID, "the very first rule", storage.Event,
				[]storage.Condition{condition})
			require.NoError(t, err)
			insertAppliedRule(t, db, &rule)

			updatedCondition, err := storage.NewCondition([]string{"new-chef-server"}, storage.ChefServer, storage.Equals)
			require.NoError(t, err)
			updatedRule, err := storage.NewRule(rule.ID, projID, "updated rule name", rule.Type,
				[]storage.Condition{updatedCondition})
			require.NoError(t, err)
			insertStagedRule(t, db, &updatedRule, false)

			appliedRule := insertAppliedRuleWithMultipleConditions(t, db, "applied", projID, storage.Node)

			resp, status, err := store.ListRulesForProject(ctx, projID)
			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Rule{&updatedRule, appliedRule}, resp)
			assert.Equal(t, storage.EditsPending, status)
		}},
		{"when a project has two applied rules and one is staged for deletion, returns only the non-deleted one", func(t *testing.T) {
			ctx := context.Background()
			projID := "foo-project"
			insertTestProject(t, db, projID, "first project", storage.Custom)

			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID, storage.Event)
			insertDeletedStagedRule(t, db, rule2)

			resp, status, err := store.ListRulesForProject(ctx, projID)
			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Rule{rule1}, resp)
			assert.Equal(t, storage.Applied, status)
		}},
		{"when multiple projects exist, returns only the requested project's rules", func(t *testing.T) {
			ctx := context.Background()
			projID1 := "foo-project"
			insertTestProject(t, db, projID1, "first project", storage.Custom)
			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID1, storage.Event)

			projID2 := "bar-project"
			insertTestProject(t, db, projID2, "second project", storage.Custom)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID2, storage.Node)

			resp, status, err := store.ListRulesForProject(ctx, projID1)
			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Rule{rule1}, resp)
			assert.NotContains(t, resp, rule2)
			assert.Equal(t, storage.Applied, status)
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		db.Flush(t)
	}
}

func TestUpdateRule(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"when the project does not exist, return ForeignKeyError": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("rule1", projID, "name", storage.Node, []storage.Condition{condition1})
			insertAppliedRule(t, db, &rule)
			require.NoError(t, err)

			rule.ProjectID = "project-not-found"
			resp, err := store.UpdateRule(ctx, &rule)
			require.Error(t, err)
			assert.Nil(t, resp)
			_, ok := err.(*storage.ForeignKeyError)
			require.True(t, ok, "mismatches expected error type")
			assert.Equal(t, "project not found: project-not-found", err.Error())
		},
		"when the rule doesn't exist in either applied or staged, return ErrNotFound": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("not-found", projID, "name", storage.Node, []storage.Condition{condition1})
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &rule)
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"when the update attempts to change the project, throw an error": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			ruleType := storage.Node
			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			ruleOriginal, err := storage.NewRule("new-id-1", "project-1", "name", ruleType,
				[]storage.Condition{condition1})
			require.NoError(t, err)
			insertAppliedRule(t, db, &ruleOriginal)

			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)

			ruleUpdated, err := storage.NewRule(ruleOriginal.ID, projID2, ruleOriginal.Name, ruleType,
				[]storage.Condition{condition1})
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &ruleUpdated)
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrChangeProjectForRule, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleOriginal.ID, ruleOriginal.Name, ruleOriginal.Type.String(), ruleOriginal.ProjectID))
			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleUpdated.ID, ruleUpdated.Name, ruleUpdated.Type.String(), ruleUpdated.ProjectID))
		},
		"when the update attempts to change the type, throw an error": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "project name", storage.Custom)

			ruleType := storage.Node
			condition, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			ruleOriginal, err := storage.NewRule("new-id-1", "project-1", "rule name", ruleType,
				[]storage.Condition{condition})
			require.NoError(t, err)
			insertAppliedRule(t, db, &ruleOriginal)

			ruleUpdated, err := storage.NewRule(ruleOriginal.ID, projID, ruleOriginal.Name, storage.Event,
				[]storage.Condition{condition})
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &ruleUpdated)
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrChangeTypeForRule, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleOriginal.ID, ruleOriginal.Name, ruleOriginal.Type.String(), ruleOriginal.ProjectID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleUpdated.ID, ruleUpdated.Name, ruleUpdated.Type.String(), ruleUpdated.ProjectID))
		},
		"when there is no project filter, update node rule with multiple conditions to have more conditions": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			ruleType := storage.Node
			rule := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, ruleType)

			condition4, err := storage.NewCondition(
				[]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			conditions := []storage.Condition{condition4}
			ruleUpdated, err := storage.NewRule(rule.ID, projID, "name", ruleType, append(conditions, rule.Conditions...))
			require.NoError(t, err)
			ruleUpdated.Status = Applied
			resp, err := store.UpdateRule(ctx, &ruleUpdated)
			require.NoError(t, err)
			assert.Equal(t, &ruleUpdated, resp)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, rule.ID))
			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`, rule.ID))
		},
		"when the project filter matches, update node rule with multiple conditions to have fewer conditions, different name": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID, "some-other-project"})

			ruleType := storage.Node
			condition1, err := storage.NewCondition(
				[]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			condition2, err := storage.NewCondition(
				[]string{"org1", "org2", "org3"}, storage.Organization, storage.MemberOf)
			require.NoError(t, err)
			condition3, err := storage.NewCondition(
				[]string{"role1"}, storage.ChefRole, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("new-id-1", projID, "name", ruleType,
				[]storage.Condition{condition1, condition2, condition3})
			require.NoError(t, err)
			insertAppliedRule(t, db, &rule)

			condition4, err := storage.NewCondition([]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			ruleUpdated, err := storage.NewRule("new-id-1", projID, "updated", rule.Type,
				[]storage.Condition{condition4})
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &ruleUpdated)
			require.NoError(t, err)
			assert.Equal(t, &ruleUpdated, resp)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3`,
				ruleUpdated.ID, ruleUpdated.Name, ruleUpdated.Type.String()))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`, ruleUpdated.ID))
		},
		"when the project filter does not match, return ErrNotFound": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"not-a-match", "some-other-project"})

			ruleType := storage.Node
			ruleOriginal := insertAppliedRuleWithMultipleConditions(t, db, "rule-original", projID, ruleType)

			condition4, err := storage.NewCondition(
				[]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			conditions := []storage.Condition{condition4}
			ruleUpdated, err := storage.NewRule(ruleOriginal.ID, projID, "name", ruleType, append(conditions, ruleOriginal.Conditions...))
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &ruleUpdated)
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleOriginal.ID, ruleOriginal.Name, ruleOriginal.Type.String(), ruleOriginal.ProjectID))
		},
		"when the rule exists in applied but not staged, adds a new rule to staged": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			ruleType := storage.Node
			ruleOriginal := insertAppliedRuleWithMultipleConditions(t, db, "rule-original", projID, ruleType)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleOriginal.ID, ruleOriginal.Name, ruleOriginal.Type.String(), ruleOriginal.ProjectID))

			condition4, err := storage.NewCondition(
				[]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			conditions := []storage.Condition{condition4}
			updatedRule, err := storage.NewRule(ruleOriginal.ID, projID, "new name", ruleType, append(conditions, ruleOriginal.Conditions...))
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &updatedRule)
			require.NoError(t, err)
			assert.Equal(t, &updatedRule, resp)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleOriginal.ID, ruleOriginal.Name, ruleOriginal.Type.String(), ruleOriginal.ProjectID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				updatedRule.ID, updatedRule.Name, updatedRule.Type.String(), updatedRule.ProjectID))
			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`,
				updatedRule.ID))
		},
		"when the rule exists in staged but not applied, updates the staged rule": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			condition, err := storage.NewCondition([]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			conditions := []storage.Condition{condition}
			originalRule, err := storage.NewRule("foo-rule", projID, "foo", storage.Event, conditions)
			require.NoError(t, err)
			insertStagedRule(t, db, &originalRule, false)

			newCondition, err := storage.NewCondition([]string{"new-chef-server-2"}, storage.ChefServer, storage.Equals)
			updatedRule, err := storage.NewRule(originalRule.ID, originalRule.ProjectID, "foo bar", originalRule.Type, append(conditions, newCondition))

			resp, err := store.UpdateRule(ctx, &updatedRule)
			require.NoError(t, err)
			assert.Equal(t, &updatedRule, resp)

			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				updatedRule.ID, updatedRule.Name, updatedRule.Type.String(), updatedRule.ProjectID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`,
				updatedRule.ID))
		},
		"when the rule exists in both staged and applied, updates the staged rule": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			condition, err := storage.NewCondition([]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			conditions := []storage.Condition{condition}
			originalRule, err := storage.NewRule("foo-rule", projID, "foo", storage.Event, conditions)
			require.NoError(t, err)
			insertAppliedRule(t, db, &originalRule)
			insertStagedRule(t, db, &originalRule, false)

			newCondition, err := storage.NewCondition([]string{"new-chef-server-2"}, storage.ChefServer, storage.Equals)
			require.NoError(t, err)
			updatedRule, err := storage.NewRule(originalRule.ID, originalRule.ProjectID, "foo bar", originalRule.Type, append(conditions, newCondition))
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &updatedRule)
			require.NoError(t, err)
			assert.Equal(t, &updatedRule, resp)

			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				updatedRule.ID, updatedRule.Name, updatedRule.Type.String(), updatedRule.ProjectID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`,
				updatedRule.ID))
		},
		"when the rule exists in applied but is marked for deletion in staged, returns marked for deletion": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			condition, err := storage.NewCondition([]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			conditions := []storage.Condition{condition}
			originalRule, err := storage.NewRule("foo-rule", projID, "foo", storage.Event, conditions)
			require.NoError(t, err)
			insertAppliedRule(t, db, &originalRule)
			deletedUpdatedRule, err := storage.NewRule(originalRule.ID, originalRule.ProjectID, "foo bar", originalRule.Type, conditions)
			insertDeletedStagedRule(t, db, &deletedUpdatedRule)

			newCondition, err := storage.NewCondition([]string{"new-chef-server-2"}, storage.ChefServer, storage.Equals)
			updatedRule, err := storage.NewRule(originalRule.ID, originalRule.ProjectID, "this better not work", originalRule.Type, append(conditions, newCondition))

			resp, err := store.UpdateRule(ctx, &updatedRule)
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrMarkedForDeletion, err)

			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				updatedRule.ID, updatedRule.Name, updatedRule.Type.String(), updatedRule.ProjectID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`,
				updatedRule.ID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestGetStagedOrAppliedRule(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"when no rules exist and requested project does not exist, returns ForeignKeyError": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.GetStagedOrAppliedRule(ctx, "project-not-found", "some-rule")
			require.Error(t, err)
			assert.Nil(t, resp)
			_, ok := err.(*storage.ForeignKeyError)
			require.True(t, ok, "mismatches expected error type")
			assert.Equal(t, "project not found: project-not-found", err.Error())
		},
		"when no rules exist in either staged or applied, returns NotFoundErr": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "testing", storage.Custom)
			resp, err := store.GetStagedOrAppliedRule(ctx, projID, "not-found")
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"when the rule doesn't exist in applied or staged, returns NotFoundErr": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)

			resp, err := store.GetStagedOrAppliedRule(ctx, projID, "not-found")
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"when multiple rules exists with no project filter, return correct rule": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			ruleToGet := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "other-rule", projID, storage.Event)

			resp, err := store.GetStagedOrAppliedRule(ctx, projID, ruleToGet.ID)
			require.NoError(t, err)
			assert.Equal(t, ruleToGet, resp)
		},
		"when multiple rules exists with a matching project filter, return correct rule": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID, projID2, "some-other-project"})

			ruleToGet := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "other-project-rule", projID2, storage.Event)

			resp, err := store.GetStagedOrAppliedRule(ctx, projID, ruleToGet.ID)
			require.NoError(t, err)
			assert.Equal(t, ruleToGet, resp)
		},
		"when multiple rules exists with a non-matching project filter, return NotFoundErr": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID2, "some-other-project"})

			ruleToGet := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Event)
			insertAppliedRuleWithMultipleConditions(t, db, "other-rule", projID2, storage.Node)

			resp, err := store.GetStagedOrAppliedRule(ctx, projID, ruleToGet.ID)
			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"when the rule exists only in the staged table, returns the staged rule": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("new-id-1", projID, "name", storage.Node, []storage.Condition{condition1})
			require.NoError(t, err)
			insertStagedRule(t, db, &rule, false)

			resp, err := store.GetStagedOrAppliedRule(ctx, projID, rule.ID)
			require.NoError(t, err)
			assert.NotNil(t, resp)
			expectedRule := storage.Rule{
				ID:         rule.ID,
				ProjectID:  rule.ProjectID,
				Name:       rule.Name,
				Type:       rule.Type,
				Conditions: rule.Conditions,
				Deleted:    false,
				Status:     "staged",
			}
			assert.Equal(t, &expectedRule, resp)
		},
		"when the rule exists only in the applied table, returns the applied rule": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "my new project", storage.Custom)

			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("new-id-1", projID, "name", storage.Event, []storage.Condition{condition1})
			insertAppliedRule(t, db, &rule)

			resp, err := store.GetStagedOrAppliedRule(ctx, projID, rule.ID)
			require.NoError(t, err)
			assert.NotNil(t, resp)
			expectedRule := storage.Rule{
				ID:         rule.ID,
				ProjectID:  rule.ProjectID,
				Name:       rule.Name,
				Type:       rule.Type,
				Conditions: rule.Conditions,
				Deleted:    false,
				Status:     Applied,
			}
			assert.Equal(t, &expectedRule, resp)
		},
		"when the rule exists in the staged and applied tables, returns the staged rule": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "my new project", storage.Custom)

			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("new-id-1", projID, "applied name", storage.Node, []storage.Condition{condition1})
			require.NoError(t, err)
			insertAppliedRule(t, db, &rule)

			stagedRule, err := storage.NewRule(rule.ID, rule.ProjectID, "update: staged name", rule.Type, rule.Conditions)
			require.NoError(t, err)
			insertStagedRule(t, db, &stagedRule, false)
			resp, err := store.GetStagedOrAppliedRule(ctx, projID, rule.ID)
			require.NoError(t, err)
			assert.NotNil(t, resp)
			expectedRule := storage.Rule{
				ID:         stagedRule.ID,
				ProjectID:  stagedRule.ProjectID,
				Name:       stagedRule.Name,
				Type:       stagedRule.Type,
				Conditions: stagedRule.Conditions,
				Deleted:    false,
				Status:     "staged",
			}
			assert.Equal(t, &expectedRule, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestDeleteRule(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"when no rules exist and requested project does not exist, returns ForeignKeyError": func(t *testing.T) {
			ctx := context.Background()
			err := store.DeleteRule(ctx, "project-not-found", "rule1")
			require.Error(t, err)
			_, ok := err.(*storage.ForeignKeyError)
			require.True(t, ok, "mismatches expected error type")
			assert.Equal(t, "project not found: project-not-found", err.Error())
		},
		"when no rules exist but requested project exists, returns NotFoundErr": func(t *testing.T) {
			ctx := context.Background()
			projID := "foo-project"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			err := store.DeleteRule(ctx, projID, "not-found")
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"when an applied rule exists but the wrong id requested, returns NotFoundErr": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule := insertAppliedRuleWithMultipleConditions(t, db, "some-rule", projID, storage.Event)

			err := store.DeleteRule(ctx, projID, "not-found")
			assert.Equal(t, storage.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1`, rule.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions`))
		},
		"when an applied and staged rule exists but the wrong id requested, returns NotFoundErr": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Event)
			insertStagedRuleWithMultipleConditions(t, db, rule.ID, rule.ProjectID, rule.Type, false)

			err := store.DeleteRule(ctx, projID, "not-found")
			assert.Equal(t, storage.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1`, rule.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, rule.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions`))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when only staged rule exists but the wrong id requested, returns NotFoundErr": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule := insertStagedRuleWithMultipleConditions(t, db, "staged", projID, storage.Event, false)

			err := store.DeleteRule(ctx, projID, "not-found")
			assert.Equal(t, storage.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, rule.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when multiple staged rules exist with no project filter, delete rule and associated conditions": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			ruleType := storage.Node
			ruleToDelete := insertStagedRuleWithMultipleConditions(t, db, "delete-me", projID, ruleType, false)
			ruleToSave := insertStagedRuleWithMultipleConditions(t, db, "save-me", projID, ruleType, false)

			err := store.DeleteRule(ctx, projID, ruleToDelete.ID)
			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToDelete.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToSave.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules`))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when multiple staged rules exist with a matching project filter and no applied rules, delete rule and associated conditions": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID, "project-2"})

			ruleType := storage.Node
			ruleToDelete := insertStagedRuleWithMultipleConditions(t, db, "delete-me", projID, ruleType, false)
			insertStagedRuleWithMultipleConditions(t, db, "save-me", projID, ruleType, false)

			err := store.DeleteRule(ctx, projID, ruleToDelete.ID)
			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToDelete.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules`))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when multiple staged rules exists with a non-matching project filter, do not delete anything": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"project-3", "project-2"})

			ruleType := storage.Node
			ruleToDelete := insertStagedRuleWithMultipleConditions(t, db, "delete-me", projID, ruleType, false)
			ruleToSave := insertStagedRuleWithMultipleConditions(t, db, "save-me", projID, ruleType, false)

			err := store.DeleteRule(ctx, projID, ruleToDelete.ID)
			assert.Equal(t, storage.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToDelete.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToSave.ID))
			assertCount(t, 6, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when multiple staged and applied rules exist with a non-matching project filter, do not delete anything": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"project-3", "project-2"})

			ruleType := storage.Node
			ruleToDelete := insertStagedRuleWithMultipleConditions(t, db, "delete-me", projID, ruleType, false)
			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, ruleType)

			condition4, err := storage.NewCondition(
				[]string{"chef-server-2"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			ruleToSave, err := storage.NewRule("new-id-2", projID, "name2", ruleType,
				[]storage.Condition{condition4})
			require.NoError(t, err)
			insertStagedRule(t, db, &ruleToSave, false)
			insertAppliedRule(t, db, &ruleToSave)

			err = store.DeleteRule(ctx, projID, ruleToDelete.ID)
			assert.Equal(t, storage.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND deleted=false`, ruleToDelete.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND deleted=false`, ruleToSave.ID))
			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when multiple staged and applied rules exist with a matching project filter, mark for delete": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID, "project-2"})

			ruleToDelete := insertAppliedRuleWithMultipleConditions(t, db, "delete-me", projID, storage.Node)
			ruleToSave := insertStagedRuleWithMultipleConditions(t, db, "save-me", projID, storage.Event, false)

			err := store.DeleteRule(ctx, projID, ruleToDelete.ID)
			require.NoError(t, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND deleted=true`, ruleToDelete.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND deleted=false`, ruleToSave.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`, ruleToDelete.ID))
		},
		"when multiple applied rules exist with a matching project filter, mark for delete": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID, "project-2"})

			ruleType := storage.Node
			ruleToDelete := insertAppliedRuleWithMultipleConditions(t, db, "delete-me", projID, ruleType)
			ruleToSave := insertAppliedRuleWithMultipleConditions(t, db, "save-me", projID, ruleType)

			err := store.DeleteRule(ctx, projID, ruleToDelete.ID)
			require.NoError(t, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND deleted=true`, ruleToDelete.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND deleted=false`, ruleToSave.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when multiple applied rules exist with a non-matching project filter, do nothing and return NotFoundErr": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"wrong-project", "project-2"})

			ruleToDelete := insertAppliedRuleWithMultipleConditions(t, db, "delete-me", projID, storage.Event)
			ruleToSave := insertAppliedRuleWithMultipleConditions(t, db, "save-me", projID, storage.Node)

			err := store.DeleteRule(ctx, projID, ruleToDelete.ID)
			assert.Equal(t, storage.ErrNotFound, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToDelete.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToSave.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestApplyStagedRules(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	// No project filter concerns in these tests so safe to re-use context.
	ctx := context.Background()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"when there are no staged rules, applied rules are unchanged", func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			rule := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)

			err := store.ApplyStagedRules(ctx)
			require.NoError(t, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules
				WHERE id=$1 AND project_id=project_db_id($2) AND name=$3 AND type=$4`, rule.ID, rule.ProjectID, rule.Name, rule.Type.String()))
		}},
		{"when there are n staged rules marked for update but no applied rules, it creates n applied rules", func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ruleType := storage.Node
			rule1 := insertStagedRuleWithMultipleConditions(t, db, "rule-1", projID, ruleType, false)
			condition, err := storage.NewCondition(
				[]string{"chef-server-2"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule2, err := storage.NewRule("new-id-2", projID, "name2", ruleType,
				[]storage.Condition{condition})
			require.NoError(t, err)
			insertStagedRule(t, db, &rule2, false)

			err = store.ApplyStagedRules(ctx)
			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1`, rule1.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1`, rule2.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_project_rules r WHERE r.id=$1)`, rule1.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_project_rules r WHERE r.id=$1)`, rule2.ID))
		}},
		{"when all staged rules are marked for delete, there are no applied rules or conditions remaining", func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ruleType := storage.Node
			ruleToDelete1 := insertAppliedRuleWithMultipleConditions(t, db, "delete-me-1", projID, storage.Node)
			insertStagedRuleWithMultipleConditions(t, db, ruleToDelete1.ID, projID, ruleType, true)

			ruleToDelete2 := insertAppliedRuleWithMultipleConditions(t, db, "delete-me-2", projID, storage.Node)
			insertStagedRuleWithMultipleConditions(t, db, ruleToDelete2.ID, projID, ruleType, true)

			err := store.ApplyStagedRules(ctx)
			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_project_rules`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions`))
		}},
		{"when there are staged rules for update and delete that are a subset of applied rules, update or delete relevant rules", func(t *testing.T) {
			projID := "project-1"
			id1 := "project-1-rule"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			ruleType := storage.Node
			rule1 := insertAppliedRuleWithMultipleConditions(t, db, id1, projID, storage.Node)
			insertStagedRuleWithMultipleConditions(t, db, id1, projID, ruleType, true)

			condition1, err := storage.NewCondition(
				[]string{"chef-server-2"}, storage.ChefServer, storage.Equals)
			require.NoError(t, err)
			rule2, err := storage.NewRule("new-id-2", projID, "name2", ruleType,
				[]storage.Condition{condition1})
			require.NoError(t, err)
			insertAppliedRule(t, db, &rule2)

			condition2, err := storage.NewCondition(
				[]string{"tag1", "tag2"}, storage.ChefTag, storage.MemberOf)
			require.NoError(t, err)
			rule2.Conditions = []storage.Condition{condition1, condition2}
			rule2UpdatedName := "this name has been updated"
			rule2UpdatedType := storage.Event
			rule2.Name = rule2UpdatedName
			rule2.Type = rule2UpdatedType
			insertStagedRule(t, db, &rule2, false)

			condition3, err := storage.NewCondition(
				[]string{"role1"}, storage.ChefRole, storage.Equals)
			require.NoError(t, err)
			condition4, err := storage.NewCondition(
				[]string{"Event"}, storage.Environment, storage.Equals)
			require.NoError(t, err)
			condition5, err := storage.NewCondition(
				[]string{"org1", "org2"}, storage.Organization, storage.MemberOf)
			require.NoError(t, err)
			rule3, err := storage.NewRule("new-id-3", projID, "name3", ruleType,
				[]storage.Condition{condition3, condition4, condition5})
			require.NoError(t, err)
			insertAppliedRule(t, db, &rule3)

			err = store.ApplyStagedRules(ctx)
			require.NoError(t, err)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND project_id=project_db_id($3) AND type=$4`,
				rule1.ID, rule1.Name, rule1.ProjectID, rule1.Type.String()))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND project_id=project_db_id($3) AND type=$4`,
				rule2.ID, rule2UpdatedName, rule2.ProjectID, rule2UpdatedType.String()))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND project_id=project_db_id($3) AND type=$4`,
				rule3.ID, rule3.Name, rule3.ProjectID, rule3.Type.String()))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_project_rules r WHERE r.id=$1)`, rule2.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_project_rules r WHERE r.id=$1)`, rule3.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_project_rules r WHERE r.id=$1)`, rule1.ID))
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		db.Flush(t)
	}
}

func TestFetchAppliedRulesByProjectIDs(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := context.Background()
	cases := map[string]func(*testing.T){
		"when no rules or projects exist, returns an empty map": func(t *testing.T) {
			resp, err := store.FetchAppliedRulesByProjectIDs(ctx)
			require.NoError(t, err)
			assert.Equal(t, resp, map[string][]*storage.Rule{})
		},
		"when a project exists without rules, returns an empty map": func(t *testing.T) {
			insertTestProject(t, db, "project-1", "let's go jigglypuff - topsecret", storage.Custom)
			resp, err := store.FetchAppliedRulesByProjectIDs(ctx)
			require.NoError(t, err)
			require.Equal(t, resp, map[string][]*storage.Rule{})
		},
		"when a project exists with staged rules only, returns an empty map": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "rule-2", projID, storage.Node, false)
			resp, err := store.FetchAppliedRulesByProjectIDs(ctx)
			require.NoError(t, err)
			require.Equal(t, resp, map[string][]*storage.Rule{})
		},
		"when a project exists with applied rules only, returns a map of applied rules": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID, storage.Node)
			resp, err := store.FetchAppliedRulesByProjectIDs(ctx)
			require.NoError(t, err)
			require.Equal(t, 1, len(resp))
			require.ElementsMatch(t, resp[projID], []*storage.Rule{rule1, rule2})
		},
		"when multiple projects exists with staged and applied rules, returns a map of applied rules": func(t *testing.T) {
			projID1 := "project-1"
			insertTestProject(t, db, projID1, "let's go jigglypuff", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "rule-1", projID1, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "rule-2", projID1, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "staged-only", projID1, storage.Node, false)
			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID1, storage.Node)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID1, storage.Node)

			projID2 := "project-2"
			insertTestProject(t, db, projID2, "let's go pikachu", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "rule-3", projID2, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "rule-4", projID2, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "staged-only-2", projID2, storage.Node, false)
			rule3 := insertAppliedRuleWithMultipleConditions(t, db, "rule-3", projID2, storage.Node)
			rule4 := insertAppliedRuleWithMultipleConditions(t, db, "rule-4", projID2, storage.Node)

			resp, err := store.FetchAppliedRulesByProjectIDs(ctx)
			require.NoError(t, err)
			require.Equal(t, 2, len(resp))
			require.ElementsMatch(t, resp[projID1], []*storage.Rule{rule1, rule2})
			require.ElementsMatch(t, resp[projID2], []*storage.Rule{rule3, rule4})
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}
