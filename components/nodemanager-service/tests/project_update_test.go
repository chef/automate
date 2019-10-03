package manager

import (
	"context"
	"fmt"
	"testing"
	"time"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/chef/automate/components/nodemanager-service/config"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

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
			description: "Environment - updating project",
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
									Values:    []string{"env1"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "Environment - not updating project. wrong rule type",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_EVENT,
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
			description: "Environment - Single rule two matching conditions",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "Environment - Single rule, one non-matching condition",
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
			description: "Environment - a rule's condition has two values for a field",
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
									Values:    []string{"env1", "env2"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "Environment - two rules only one matching",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "Environment - two project only one matching",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "Environment - two matching projects",
			projectsData: []*nodes.ProjectsData{
				{Key: "environment", Values: []string{"env1"}},
				{Key: "organization_name", Values: []string{"org2"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project7": {
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
			expectedProjectIDs: []string{"project3", "project7"},
		},
		{
			description: "Environment - updating project all variables",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description:  "Environment - missing field",
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
			description: "Environment - no project change",
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
				"project9": {
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
			expectedProjectIDs: []string{"project9"},
		},

		{
			description: "Environment - project removed all variables",
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
			description:        "Environment - project removed no variables",
			projectsData:       []*nodes.ProjectsData{},
			projectRules:       map[string]*iam_v2.ProjectRules{},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},

		// Orgs
		{
			description: "Org - updating project all variables",
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
				"project3": {
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "Org - not updating project. wrong rule type",
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
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "Org - Single rule two matching conditions",
			projectsData: []*nodes.ProjectsData{
				{Key: "organization_name", Values: []string{"ink"}},
				{Key: "roles", Values: []string{"backend"}},
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "Org - Single rule, one non-matching condition",
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
			description: "Org - a rule's condition has two values for a field",
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
									Values:    []string{"ink", "paper"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "Org - two rules only one matching",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "Org - two project only one matching",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description:  "Org - missing field",
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
			description: "Org - no project change",
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
				"project9": {
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
			expectedProjectIDs: []string{"project9"},
		},

		// chef_server
		{
			description: "chef_server - updating project all variables",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "chef_server - not updating project. wrong rule type",
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
			description: "chef_server - Single rule two matching conditions",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_server", Values: []string{"chef_server.com"}},
				{Key: "roles", Values: []string{"backend"}},
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
			expectedProjectIDs: []string{"project3"},
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
				"project3": {
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "chef_server - two rules only one matching",
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
			expectedProjectIDs: []string{"project3"},
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
				"project3": {
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
			expectedProjectIDs: []string{"project3"},
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
				"project9": {
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
			expectedProjectIDs: []string{"project9"},
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
			expectedProjectIDs: []string{"project3"},
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
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "policy_name - Single rule two matching conditions",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_name", Values: []string{"prod"}},
				{Key: "roles", Values: []string{"backend"}},
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
			expectedProjectIDs: []string{"project3"},
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
				"project3": {
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "policy_name - two rules only one matching",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "policy_name - two project only one matching",
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
			expectedProjectIDs: []string{"project3"},
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
				"project9": {
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
			expectedProjectIDs: []string{"project9"},
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "policy_group - not updating project. wrong rule type",
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
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "policy_group - Single rule two matching conditions",
			projectsData: []*nodes.ProjectsData{
				{Key: "policy_group", Values: []string{"ts_sci_polygraph"}},
				{Key: "roles", Values: []string{"backend"}},
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "policy_group - Single rule, one non-matching condition",
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
			description: "policy_group - a rule's condition has two values for a field",
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
									Values:    []string{"ts_sci_polygraph", "classified"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "policy_group - two rules only one matching",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "policy_group - two project only one matching",
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
			expectedProjectIDs: []string{"project3"},
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
			description: "policy_group - no project change",
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
				"project9": {
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
			expectedProjectIDs: []string{"project9"},
		},

		// roles
		{
			description: "roles - updating project all variables",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "roles - not updating project. wrong rule type",
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
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "roles - two roles",
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
				"project3": {
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "roles - Single rule, multiple conitions with one non-matching",
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
			description: "roles - a rule's condition has two values for a field",
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
									Values:    []string{"mysql", "ftp"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "roles - two matching rule condition values",
			projectsData: []*nodes.ProjectsData{
				{Key: "roles", Values: []string{"mysql", "ftp"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "roles - two rules only one matching",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "roles - two rules only one matching with two roles",
			projectsData: []*nodes.ProjectsData{
				{Key: "roles", Values: []string{"mysql", "ftp"}},
			},
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "roles - two matching rules",
			projectsData: []*nodes.ProjectsData{
				{Key: "roles", Values: []string{"mysql", "ftp"}},
			},
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "roles - two projects only one matching",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description:  "roles - missing field",
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
			description: "roles - no project change",
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
				"project9": {
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
			expectedProjectIDs: []string{"project9"},
		},

		// chef_tags
		{
			description: "chef_tags - updating project with all variables set",
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
				"project3": {
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "chef_tags - not updating project. wrong rule type",
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
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{},
		},
		{
			description: "chef_tags - two chef tags",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "chef_tags - Single rule, multiple conitions with one non-matching",
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
			description: "chef_tags - a rule's condition has two values for a field",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_tags", Values: []string{"dev_sec"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "chef_tags - two matching rule condition values",
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
									Values:    []string{"dev_sec", "cos"},
								},
							},
						},
					},
				},
			},
			originalProjectIDs: []string{"project9"},
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "chef_tags - two rules only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_tags", Values: []string{"dev_sec"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "chef_tags - two chef tags only one matching",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "chef_tags - two matching rules",
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description: "chef_tags - two projects only one matching",
			projectsData: []*nodes.ProjectsData{
				{Key: "chef_tags", Values: []string{"dev_sec"}},
			},
			projectRules: map[string]*iam_v2.ProjectRules{
				"project3": {
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
			expectedProjectIDs: []string{"project3"},
		},
		{
			description:  "chef_tags - missing field",
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
			description: "chef_tags - no project change",
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
				"project9": {
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
			expectedProjectIDs: []string{"project9"},
		},
		{
			description: "chef_tags - two matching projects",
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
									Values:    []string{"cos"},
								},
							},
						},
					},
				},
				"project9": {
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
			expectedProjectIDs: []string{"project9", "project3"},
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
	assert.Nil(t, err)
	for !jobStatus.Completed {
		time.Sleep(time.Millisecond * 5)
		jobStatus, err = db.JobStatus(ctx, jobID)
		assert.Nil(t, err)
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
