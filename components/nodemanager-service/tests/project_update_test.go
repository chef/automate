package manager

import (
	"context"
	"fmt"
	"testing"
	"time"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/nodemanager-service/config"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestProjectUpdateDoesNotUpdateManagedNodes(t *testing.T) {
	ctx := context.Background()
	db, err := createPGDB()
	require.NoError(t, err)

	timestamp, err := ptypes.TimestampProto(time.Now())
	require.NoError(t, err)

	projectsData := []*nodes.ProjectsData{
		{Key: "environment", Values: []string{"env1"}},
	}

	projectRules := map[string]*iam_v2.ProjectRules{
		"targetProject": {
			Rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
							Values:    []string{"env1"},
						},
					},
				},
			},
		},
	}

	t.Run(fmt.Sprintf("project update: %s", "test that managed nodes do not get updated by project manager"),
		func(t *testing.T) {
			originalNode := &nodes.Node{
				Name: "test-node",
				Tags: []*common.Kv{
					{Key: "environment", Value: "env1"},
				},
			}

			// Create node, it will be automate managed node
			nodeID, err := db.AddNode(originalNode)
			require.NoError(t, err)
			defer db.DeleteNode(nodeID)

			// Update project
			jobIDs, err := db.UpdateProjectTags(ctx, projectRules)
			require.NoError(t, err)
			require.Equal(t, 1, len(jobIDs))

			waitForJobToComplete(ctx, t, db, jobIDs[0])

			// Get node
			processedNode, err := db.GetNode(ctx, nodeID)
			require.NoError(t, err)

			// ingest node scan now, still no change
			nodeScan := &manager.NodeMetadata{
				Uuid:        nodeID,
				LastContact: timestamp,
				RunData: &nodes.LastContactData{
					Id:      nodeID,
					EndTime: timestamp,
					Status:  nodes.LastContactData_PASSED,
				},
				ProjectsData: projectsData, // this shouldn't actually happen, b/c the report to
				// nodemanager function does not include projects data if the node has a job id (scan job)
			}

			// Ingest node
			err = db.ProcessIncomingNode(nodeScan)

			// get node again
			processedNode, err = db.GetNode(ctx, nodeID)
			require.NoError(t, err)

			assert.ElementsMatch(t, []string{}, processedNode.Projects)
		})

}

func TestProjectUpdate(t *testing.T) {
	ctx := context.Background()
	db, err := createPGDB()
	require.NoError(t, err)

	timestamp, err := ptypes.TimestampProto(time.Now())
	require.NoError(t, err)

	cases := []struct {
		description        string
		projectsData       []*nodes.ProjectsData
		projectRules       map[string]*iam_v2.ProjectRules
		originalProjectIDs []string
		expectedProjectIDs []string
	}{
		// Environment
		{
			description: "Environment - project updates with single rule, single matching condition",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "Environment - not updating project. wrong rule type",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_EVENT, // <- wrong type
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{},
			expectedProjectIDs: []string{},
		},
		{
			description: "Environment - project updates with single rule two matching conditions",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "organization_name", Values: []string{"org1"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "Environment - project does not update with single rule, " +
				"one non-matching and one matching condition",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "Environment - project updates with a single rule, single condition with " +
				"three values and only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env3", "env1", "env2"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "Environment - project updates with two rules and only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env2"},
								},
							},
						},
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "Environment - project updates with two projects and only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project7": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env2"},
								},
							},
						},
					},
				},
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "Environment - projects update with two matching projects",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "organization_name", Values: []string{"org2"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject2": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org2"},
								},
							},
						},
					},
				},
				"targetProject1": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject1", "targetProject2"},
		},
		{
			description: "Environment - project updates with all ProjectData variables set",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "roles", Values: []string{}},
				{Key: "policy_name", Values: []string{}},
				{Key: "policy_group", Values: []string{}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_tags", Values: []string{}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description:  "Environment - project does not update with the matching condition field missing",
			projectsData: []*nodes.ProjectsData{},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "Environment - the project IDs does not update with a project matching for " +
				"the same ID",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env2"},
								},
							},
						},
					},
				},
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"targetProject"},
			expectedProjectIDs: []string{"targetProject"},
		},

		// general
		{
			description: "project removed with no project rules and all ProjectsData variables set",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "roles", Values: []string{}},
				{Key: "policy_name", Values: []string{}},
				{Key: "policy_group", Values: []string{}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_tags", Values: []string{}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
			},
			projectRules:       map[string]*iam_v2.ProjectRules{},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description:        "project removed with no project rules and not ProjectsData variables set",
			projectsData:       []*nodes.ProjectsData{},
			projectRules:       map[string]*iam_v2.ProjectRules{},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},

		// Orgs
		{
			description: "Org - project updated with single condition and all ProjectsData variables set",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
				{Key: "policy_name", Values: []string{}},
				{Key: "policy_group", Values: []string{}},
				{Key: "roles", Values: []string{}},
				{Key: "chef_tags", Values: []string{}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "Org - not updating project, one rule with the wrong rule type",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "roles", Values: []string{}},
				{Key: "policy_name", Values: []string{}},
				{Key: "policy_group", Values: []string{}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_tags", Values: []string{}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_EVENT,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{},
			expectedProjectIDs: []string{},
		},
		{
			description: "Org - project updated with single rule, two matching conditions",
			projectsData: []*nodes.ProjectsData{
				{Key: "organization_name", Values: []string{"ink"}},
				{Key: "roles", Values: []string{"backend"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"ink"},
								},
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"backend"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"originalProject"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "Org - project removed with a single rule, one non-matching and one matching condition",
			projectsData: []*nodes.ProjectsData{
				{Key: "organization_name", Values: []string{"ink"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"ink"},
								},
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"backend"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "Org - project updated with a rule's condition having two values for a " +
				"field with one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "organization_name", Values: []string{"ink"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"ink", "paper"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "Org - project updated with two rules, only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "organization_name", Values: []string{"ink"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"ink"},
								},
							},
						},
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"paper"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "Org - project update with two project, only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "organization_name", Values: []string{"ink"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project7": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"paper"},
								},
							},
						},
					},
				},
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"ink"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description:  "Org - project removed with one condition matching a missing field",
			projectsData: []*nodes.ProjectsData{},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"ink"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "Org - project not updated with two rules one matching with the original " +
				"project ID and the other rule not matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "organization_name", Values: []string{"ink"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"paper"},
								},
							},
						},
					},
				},
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"ink"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"targetProject"},
			expectedProjectIDs: []string{"targetProject"},
		},

		// chef_server
		{
			description: "chef_server - project updated with one condition and all the " +
				"ProjectsData variables set",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
				{Key: "policy_name", Values: []string{}},
				{Key: "policy_group", Values: []string{}},
				{Key: "roles", Values: []string{}},
				{Key: "chef_tags", Values: []string{}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef_server.com"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "chef_server - project not updated with matching rule on the wrong type",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "roles", Values: []string{}},
				{Key: "policy_name", Values: []string{}},
				{Key: "policy_group", Values: []string{}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_tags", Values: []string{}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_EVENT, // <- wrong type
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef_server.com"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{},
			expectedProjectIDs: []string{},
		},
		{
			description: "chef_server - Single rule two matching conditions",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_server", Values: []string{"chef_server.com"}},
				{Key: "roles", Values: []string{"backend"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef_server.com"},
								},
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"backend"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "chef_server - Single rule, one non-matching condition",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_server", Values: []string{"chef_server.com"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef_server.com"},
								},
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"backend"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "chef_server - a rule's condition has two values for a field",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_server", Values: []string{"chef_server.com"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef_server.com", "automate_server.com"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "chef_server - two rules only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_server", Values: []string{"chef_server.com"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef_server.com"},
								},
							},
						},
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"automate_server.com"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "chef_server - two project only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_server", Values: []string{"automate_server.com"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project7": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef_server.com"},
								},
							},
						},
					},
				},
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"automate_server.com"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description:  "chef_server - missing field",
			projectsData: []*nodes.ProjectsData{},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef_server.com"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "chef_server - no project change",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_server", Values: []string{"chef_server.com"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"automate.com"},
								},
							},
						},
					},
				},
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef_server.com"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"targetProject"},
			expectedProjectIDs: []string{"targetProject"},
		},

		// policy_name
		{
			description: "policy_name - updating project all variables",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "policy_group", Values: []string{}},
				{Key: "roles", Values: []string{}},
				{Key: "chef_tags", Values: []string{}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "policy_name - not updating project. wrong rule type",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "roles", Values: []string{}},
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "policy_group", Values: []string{}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_tags", Values: []string{}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_EVENT,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{},
			expectedProjectIDs: []string{},
		},
		{
			description: "policy_name - Single rule two matching conditions",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "roles", Values: []string{"backend"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod"},
								},
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"backend"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "policy_name - Single rule, one non-matching condition",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "roles", Values: []string{"frontend"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod"},
								},
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"backend"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "policy_name - a rule's condition has two values for a field",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_name", Values: []string{"prod"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod", "dev"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "policy_name - two rules only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_name", Values: []string{"prod"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod"},
								},
							},
						},
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"dev"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "policy_name - two project only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_name", Values: []string{"prod"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod"},
								},
							},
						},
					},
				},
				"project7": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"dev"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description:  "policy_name - missing field",
			projectsData: []*nodes.ProjectsData{},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "policy_name - no project change",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_name", Values: []string{"prod"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"dev"},
								},
							},
						},
					},
				},
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"targetProject"},
			expectedProjectIDs: []string{"targetProject"},
		},

		// policy_group
		{
			description: "policy_group - updating project with all variables set",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "policy_group", Values: []string{"ts_sci_polygraph"}},
				{Key: "roles", Values: []string{}},
				{Key: "chef_tags", Values: []string{}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"ts_sci_polygraph"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "policy_group - project not updating, with a matching rule, wrong rule type",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "roles", Values: []string{}},
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "policy_group", Values: []string{"ts_sci_polygraph"}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_tags", Values: []string{}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_EVENT, // <- This is the wrong type
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"ts_sci_polygraph"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{},
			expectedProjectIDs: []string{},
		},
		{
			description: "policy_group - project updated, with a single rule two matching conditions",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_group", Values: []string{"ts_sci_polygraph"}},
				{Key: "roles", Values: []string{"backend"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"ts_sci_polygraph"},
								},
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"backend"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "policy_group - project removed, with a single rule, two conditions only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_group", Values: []string{"ts_sci_polygraph"}},
				{Key: "roles", Values: []string{"frontend"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"ts_sci_polygraph"},
								},
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"backend"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "policy_group - project update, with a rule's condition having two values for a field",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_group", Values: []string{"ts_sci_polygraph"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"ts_sci_polygraph", "classified"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "policy_group - project update, two rules only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_group", Values: []string{"ts_sci_polygraph"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"ts_sci_polygraph"},
								},
							},
						},
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"classified"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "policy_group - project updated, with two project only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_group", Values: []string{"ts_sci_polygraph"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"ts_sci_polygraph"},
								},
							},
						},
					},
				},
				"project7": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"classified"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description:  "policy_group - missing field",
			projectsData: []*nodes.ProjectsData{},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"ts_sci_polygraph"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "policy_group - no project change, with two projects only one matching with the " +
				"original ID",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_group", Values: []string{"ts_sci_polygraph"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"classified"},
								},
							},
						},
					},
				},
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"ts_sci_polygraph"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"targetProject"},
			expectedProjectIDs: []string{"targetProject"},
		},

		// roles
		{
			description: "roles - project updated, with one condition matching, all ProjectsData variables set",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "policy_group", Values: []string{}},
				{Key: "roles", Values: []string{"mysql"}},
				{Key: "chef_tags", Values: []string{}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"mysql"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "roles - project not updated, one matching rule, wrong rule type",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "roles", Values: []string{"mysql"}},
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "policy_group", Values: []string{}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_tags", Values: []string{}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_EVENT, // <- wrong type
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"mysql"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{},
			expectedProjectIDs: []string{},
		},
		{
			description: "roles - project updated, one condition with two ProjectsData roles",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "policy_group", Values: []string{}},
				{Key: "roles", Values: []string{"mysql", "ftp"}},
				{Key: "chef_tags", Values: []string{}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"ftp"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "roles - project removed, with single rule, multiple conditions with one non-matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "roles", Values: []string{"frontend", "mysql"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod"},
								},
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"backend"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "roles - project updated, a rule's condition has two values for a field",
			projectsData: []*nodes.ProjectsData{
				{Key: "roles", Values: []string{"mysql"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"mysql", "ftp"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "roles - project updated, two matching rule condition values",
			projectsData: []*nodes.ProjectsData{
				{Key: "roles", Values: []string{"mysql", "ftp"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"mysql", "ftp"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "roles - project updated, two rules only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "roles", Values: []string{"mysql"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"mysql"},
								},
							},
						},
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"dev"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "roles - project updated, with two rules, only one matching with two roles",
			projectsData: []*nodes.ProjectsData{
				{Key: "roles", Values: []string{"mysql", "ftp"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"mysql"},
								},
							},
						},
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"dev"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "roles - project updated, with two matching rules",
			projectsData: []*nodes.ProjectsData{
				{Key: "roles", Values: []string{"mysql", "ftp"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"mysql"},
								},
							},
						},
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"ftp"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "roles - project update, with two projects only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "roles", Values: []string{"mysql"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"mysql"},
								},
							},
						},
					},
				},
				"project7": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"vftp"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description:  "roles - project removed, with a condition matching a missing ProjectsData field",
			projectsData: []*nodes.ProjectsData{},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"mysql"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "roles - project not updated, with a rule matching with the original ID",
			projectsData: []*nodes.ProjectsData{
				{Key: "roles", Values: []string{"mysql"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"vftp"},
								},
							},
						},
					},
				},
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"mysql"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"targetProject"},
			expectedProjectIDs: []string{"targetProject"},
		},

		// chef_tags
		{
			description: "chef_tags - project updated, with all the ProjectsData variables set with one" +
				" matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "policy_group", Values: []string{}},
				{Key: "roles", Values: []string{"mysql"}},
				{Key: "chef_tags", Values: []string{"dev_sec"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"dev_sec"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "chef_tags - project not updated, with one rule with the wrong type",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "roles", Values: []string{"mysql"}},
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "policy_group", Values: []string{}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_tags", Values: []string{"dev_sec"}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_EVENT, // <- wrong type
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"dev_sec"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{},
			expectedProjectIDs: []string{},
		},
		{
			description: "chef_tags - project updated, ProjectsData has two chef tags with only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "organization_name", Values: []string{"org"}},
				{Key: "chef_server", Values: []string{"chef_server.com"}},
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "policy_group", Values: []string{}},
				{Key: "roles", Values: []string{"mysql", "ftp"}},
				{Key: "chef_tags", Values: []string{"dev_sec", "cos"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"cos"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "chef_tags - project updated, single rule, multiple conditions with one non-matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "chef_tags", Values: []string{"dev_sec", "cos"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod"},
								},
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"backend"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "chef_tags - project updated, a rule's condition has two values for a field",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_tags", Values: []string{"dev_sec"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"dev_sec", "cos"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "chef_tags - project updated, two matching rule condition values",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_tags", Values: []string{"dev_sec", "cos"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"dev_sec", "cos"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "chef_tags - project updated, two rules only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_tags", Values: []string{"dev_sec"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"dev_sec"},
								},
							},
						},
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"dev"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "chef_tags - project updated, with ProjectsData chef_tags field having two values " +
				"and only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_tags", Values: []string{"dev_sec", "cos"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"dev_sec"},
								},
							},
						},
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"dev"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "chef_tags - project ID updated, with one project, two matching rules",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_tags", Values: []string{"dev_sec", "cos"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"dev_sec"},
								},
							},
						},
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"cos"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "chef_tags - project ID updated, with two projects, only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_tags", Values: []string{"dev_sec"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"dev_sec"},
								},
							},
						},
					},
				},
				"project7": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"cos"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "chef_tags - project IDs removed, with one condition associated to a " +
				"missing ProjectsData field",
			projectsData: []*nodes.ProjectsData{},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"cos"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "chef_tags - project IDs not updated, with two projects only one matching " +
				"with the original ID",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_tags", Values: []string{"dev_sec", "cos"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"vftp"},
								},
							},
						},
					},
				},
				"targetProject": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"dev_sec"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"targetProject"},
			expectedProjectIDs: []string{"targetProject"},
		},
		{
			description: "chef_tags - project IDs updated with two matching projects",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_tags", Values: []string{"dev_sec", "cos"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"targetProject1": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"cos"},
								},
							},
						},
					},
				},
				"targetProject2": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_NODE,
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"dev_sec"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"targetProject2", "targetProject1"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("project update: %s", test.description),
			func(t *testing.T) {
				nodeID := createUUID()
				originalNode := &manager.NodeMetadata{
					Uuid:        nodeID,
					Projects:    test.originalProjectIDs,
					LastContact: timestamp,
					RunData: &nodes.LastContactData{
						Id:      createUUID(),
						EndTime: timestamp,
						Status:  nodes.LastContactData_PASSED,
					},
					ProjectsData: test.projectsData,
				}

				// Ingest node
				err = db.ProcessIncomingNode(originalNode)
				require.NoError(t, err)
				defer db.DeleteNode(nodeID)

				// Update project
				jobIDs, err := db.UpdateProjectTags(ctx, test.projectRules)
				require.NoError(t, err)
				require.Equal(t, 1, len(jobIDs))

				waitForJobToComplete(ctx, t, db, jobIDs[0])

				// Get Update node
				processedNode, err := db.GetNode(ctx, nodeID)
				require.NoError(t, err)

				assert.ElementsMatch(t, test.expectedProjectIDs, processedNode.Projects)
			})
	}
}

func waitForJobToComplete(ctx context.Context, t *testing.T, db *pgdb.DB, jobID string) {
	jobStatus, err := db.JobStatus(ctx, jobID)
	assert.NoError(t, err)
	for !jobStatus.Completed {
		time.Sleep(time.Millisecond * 5)
		jobStatus, err = db.JobStatus(ctx, jobID)
		assert.NoError(t, err)
		if err != nil {
			assert.FailNow(t, "testing job status")
		}
	}
}

func createPGDB() (*pgdb.DB, error) {
	return pgdb.New(&config.Postgres{
		Database:         "nodemanager_service",
		MigrationsPath:   "/src/components/nodemanager-service/pgdb/migration/sql",
		ConnectionString: "postgresql://nodemanager@127.0.0.1:10145/nodemanager_service?sslmode=verify-ca&sslcert=/hab/svc/nodemanager-service/config/service.crt&sslkey=/hab/svc/nodemanager-service/config/service.key&sslrootcert=/hab/svc/nodemanager-service/config/root_ca.crt",
	})
}

func createUUID() string {
	return uuid.Must(uuid.NewV4()).String()
}
