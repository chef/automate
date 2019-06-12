package processor

import (
	"context"
	"fmt"
	"testing"

	"github.com/golang/mock/gomock"
	"github.com/stretchr/testify/assert"

	chef "github.com/chef/automate/api/external/ingest/request"
	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
)

// All the conditions must be true for a rule to be true (ANDed together).
// Only one rule has to be true for the project to match (ORed together).
// If there are no rules the project does not match
// If the rule does not have any conditions it does not match any resources.
func TestActionProjectRulesMatching(t *testing.T) {
	cases := []struct {
		description string
		action      *chef.Action
		rules       []*iam_v2.ProjectRule
		matching    bool
	}{
		{
			description: "A project with no rules does not match any resources",
			matching:    false,
			action:      &chef.Action{},
			rules:       []*iam_v2.ProjectRule{},
		},
		{
			description: "A project with one rule that does not have any conditions does not match any resources",
			matching:    false,
			action:      &chef.Action{},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_EVENT,
				},
			},
		},
		{
			description: "Single rule single condition matching incorrect rule type",
			matching:    false,
			action: &chef.Action{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_EVENT,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ENVIRONMENTS,
							Values:    []string{"production"},
						},
					},
				},
			},
		},
		{
			description: "A rule of type ProjectRuleTypes_NODE does not match any actions",
			matching:    false,
			action: &chef.Action{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_ORGS,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Two rules of type NODE and EVENT should match",
			matching:    true,
			action: &chef.Action{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_ORGS,
							Values: []string{"org_1"},
						},
					},
				},
				{
					Type: iam_v2.ProjectRuleTypes_EVENT,
					Conditions: []*iam_v2.Condition{
						{
							Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_ORGS,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},

		// Orgs
		{
			description: "Org: Single rule matching",
			matching:    true,
			action: &chef.Action{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_EVENT,
					Conditions: []*iam_v2.Condition{
						{
							Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_ORGS,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Org: Single rule differing case non-matching",
			matching:    false,
			action: &chef.Action{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "Org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_EVENT,
					Conditions: []*iam_v2.Condition{
						{
							Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_ORGS,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Org: Single rule non-matching",
			matching:    false,
			action: &chef.Action{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_ORGS,
							Values: []string{"org_2"},
						},
					},
				},
			},
		},
		{
			description: "Org: two on same field rule matching",
			matching:    true,
			action: &chef.Action{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_EVENT,
					Conditions: []*iam_v2.Condition{
						{
							Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_ORGS,
							Values: []string{"org_1", "org_2"},
						},
					},
				},
			},
		},

		// Chef Server
		{
			description: "Chef Server: Single rule matching",
			matching:    true,
			action: &chef.Action{
				RemoteHostname:   "chef_server_1",
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_EVENT,
					Conditions: []*iam_v2.Condition{
						{
							Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_SERVERS,
							Values: []string{"chef_server_1"},
						},
					},
				},
			},
		},
		{
			description: "Chef Server: Single rule differing case non-matching",
			matching:    false,
			action: &chef.Action{
				RemoteHostname:   "Chef_server_1",
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_EVENT,
					Conditions: []*iam_v2.Condition{
						{
							Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_SERVERS,
							Values: []string{"chef_server_1"},
						},
					},
				},
			},
		},
		{
			description: "Chef Server: Single rule non-matching",
			matching:    false,
			action: &chef.Action{
				RemoteHostname:   "chef_server_2",
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_EVENT,
					Conditions: []*iam_v2.Condition{
						{
							Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_SERVERS,
							Values: []string{"chef_server_1"},
						},
					},
				},
			},
		},
		{
			description: "Chef Server: two condition values on the same field one matching",
			matching:    true,
			action: &chef.Action{
				RemoteHostname:   "chef_server_1",
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_EVENT,
					Conditions: []*iam_v2.Condition{
						{
							Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_SERVERS,
							Values: []string{"chef_server_1", "chef_server_2"},
						},
					},
				},
			},
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("action match: %s", test.description),
			func(t *testing.T) {
				projectMatch := actionMatchesRules(test.action, test.rules)

				assert.Equal(t, test.matching, projectMatch, test.description)
			})
	}
}

// When a single message is in the inbox the `ListRulesForAllProjects` should be called once
func TestActionBundlerSingleMessage(t *testing.T) {
	inbox := make(chan message.ChefAction, 100)
	ctrl := gomock.NewController(t)
	authzClient := iam_v2.NewMockProjectsClient(ctrl)
	authzClient.EXPECT().ListRulesForAllProjects(gomock.Any(), gomock.Any()).Times(1).Return(
		&iam_v2.ListRulesForAllProjectsResp{}, nil)
	errc := make(chan error)

	inbox <- message.NewChefAction(context.Background(), &chef.Action{}, errc)
	close(inbox)
	out := actionBundleProjectTagger(inbox, authzClient)

	<-out

	ctrl.Finish()
}

// When 5 messages are in the inbox the `ListRulesForAllProjects` function is only called once.
// This is showing that the bundling of messages is working.
// Where before the authz call to `ListRulesForAllProjects` was called for each message.
func TestActionBundler5Messages(t *testing.T) {
	inbox := make(chan message.ChefAction, 100)
	ctrl := gomock.NewController(t)
	authzClient := iam_v2.NewMockProjectsClient(ctrl)
	authzClient.EXPECT().ListRulesForAllProjects(gomock.Any(), gomock.Any()).Times(1).Return(
		&iam_v2.ListRulesForAllProjectsResp{}, nil)
	errc := make(chan error)

	inbox <- message.NewChefAction(context.Background(), &chef.Action{}, errc)
	inbox <- message.NewChefAction(context.Background(), &chef.Action{}, errc)
	inbox <- message.NewChefAction(context.Background(), &chef.Action{}, errc)
	inbox <- message.NewChefAction(context.Background(), &chef.Action{}, errc)
	inbox <- message.NewChefAction(context.Background(), &chef.Action{}, errc)
	close(inbox)

	out := actionBundleProjectTagger(inbox, authzClient)

	<-out
	<-out
	<-out
	<-out
	<-out

	ctrl.Finish()
}

// A simple run through of the bundler project tagger processor.
// Two messages are sent through with only one matching a project.
func TestActionBundlerMatchProjectRule(t *testing.T) {
	testProjectName := "Test"
	orgName := "org_1"
	projectRules := map[string]*iam_v2.ProjectRules{}

	// Project 'Test' has an ingest rule for events of orgs matching 'org_1'
	// This will be returned in the `ListRulesForAllProjects` request
	projectRules[testProjectName] = &iam_v2.ProjectRules{
		Rules: []*iam_v2.ProjectRule{
			{
				Type: iam_v2.ProjectRuleTypes_EVENT,
				Conditions: []*iam_v2.Condition{
					{
						Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_ORGS,
						Values: []string{orgName},
					},
				},
			},
		},
	}
	authzClient := iam_v2.NewMockProjectsClient(gomock.NewController(t))
	authzClient.EXPECT().ListRulesForAllProjects(gomock.Any(), gomock.Any()).Return(
		&iam_v2.ListRulesForAllProjectsResp{ProjectRules: projectRules}, nil)
	errc := make(chan error)

	// Creating an ingest Chef Action that matches the project 'Test' rules
	action1 := message.NewChefAction(context.Background(), &chef.Action{
		OrganizationName: orgName,
	}, errc)

	// Creating an ingest Chef Action that does not matche the project 'Test' rules
	action2 := message.NewChefAction(context.Background(), &chef.Action{
		OrganizationName: "no_match",
	}, errc)

	// Creating the inbox to the 'actionBundleProjectTagger'
	inbox := make(chan message.ChefAction, 100)
	inbox <- action1
	inbox <- action2
	close(inbox)

	// creating the ActionProjectTagger with will tag both the actions in the inbox
	out := BuildActionProjectTagger(authzClient)(inbox)

	// Getting the tagged 'action1`
	processMsg1 := <-out
	assert.Equal(t, []string{testProjectName}, processMsg1.InternalChefAction.Projects)

	// Getting the tagged 'action2`
	processMsg2 := <-out
	assert.Equal(t, []string{}, processMsg2.InternalChefAction.Projects)
}
