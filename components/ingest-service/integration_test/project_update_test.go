package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/authz"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	"github.com/stretchr/testify/assert"
)

func TestProjectUpdatePainlessElasticsearchScript(t *testing.T) {
	var (
		ctx = context.Background()
	)

	cases := []struct {
		description string
		node        iBackend.Node
		projects    map[string]*authz.ProjectRules
		projectIDs  []string
	}{
		// Environment
		{
			description: "Environment: Single rule matching condition",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					OrganizationName: "org1",
					Environment:      "env1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "Environment: a rule's condition has two values for a field",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					OrganizationName: "org1",
					Environment:      "env2",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1", "env2"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "Environment: Single rule two matching conditions",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					OrganizationName: "org1",
					Environment:      "env1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "Environment: Single rule, one non-matching condition",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					OrganizationName: "org2",
					Environment:      "env1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "Environment: two rules only one matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					OrganizationName: "org2",
					Environment:      "env1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env2"},
								},
							},
						},
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "Environment: two project only one matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					OrganizationName: "org2",
					Environment:      "env1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env2"},
								},
							},
						},
					},
				},
				"project3": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project3"},
		},
		{
			description: "Environment: two matching projects",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					OrganizationName: "org2",
					Environment:      "env1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org2"},
								},
							},
						},
					},
				},
				"project3": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_ENVIRONMENT,
									Values:    []string{"env1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project3", "project9"},
		},

		// Orgs
		{
			description: "Org: Single rule matching update",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					OrganizationName: "org1",
					Environment:      "env1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Type: authz.ProjectRuleTypes_NODE,
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "no matching nodes",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					OrganizationName: "org1",
					Environment:      "env1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org2"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "project rules match current node project tags",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					OrganizationName: "org1",
					Environment:      "env1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"old_tag": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"old_tag"},
		},

		// chefServers
		{
			description: "chefServers: Single rule matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					SourceFqdn: "chef-server.org",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef-server.org"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "chefServers: Single rule not matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					SourceFqdn: "chef-server2.org",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef-server.org"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "chefServers: Single rule differing case not matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					SourceFqdn: "Chef-server.org",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef-server.org"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "chefServers: Single rule with two values on a condition",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					SourceFqdn: "chef-server.org",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef-server.org", "chef-server2.org"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "chefServers: two rules both matching on different fields",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					SourceFqdn:       "chef-server.org",
					OrganizationName: "org1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef-server.org"},
								},
							},
						},
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "chefServers: two rules with only one matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					SourceFqdn:       "chef-server.org",
					OrganizationName: "org1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef-server2.org"},
								},
							},
						},
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},

		// roles
		{
			description: "roles: Single rule matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					Roles:      []string{"area_51"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"area_51"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "roles: Single rule not matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					Roles:      []string{"area_51"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"area_52"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "roles: Single rule differing case not matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					Roles:      []string{"Area_51"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"area_51"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "roles: Single rule with two values on a condition",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					Roles:      []string{"area_51"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"area_51", "area_52"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "roles: two rules both matching on different fields",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					Roles:            []string{"area_51"},
					OrganizationName: "org1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"area_51"},
								},
							},
						},
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "roles: two rules with only one matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					Roles:            []string{"area_51"},
					OrganizationName: "org1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"area_51"},
								},
							},
						},
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org2"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "roles: two conditions with only one matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					Roles:            []string{"area_51"},
					OrganizationName: "org1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"area_51"},
								},
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org2"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "roles: multiple roles one matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					Roles:      []string{"area_51", "area_52", "area_53"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"area_51"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "roles: multiple roles, none matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					Roles:      []string{"area_51", "area_52", "area_53"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"area_54"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "roles: setting the project to unassigned when there are no roles",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"area_54"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},

		// chefTags
		{
			description: "chefTags: Single rule matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					ChefTags:   []string{"area_51"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"area_51"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "chefTags: Single rule not matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					ChefTags:   []string{"area_51"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"area_52"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "chefTags: Single rule differing case not matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					ChefTags:   []string{"Area_51"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"area_51"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "chefTags: Single rule with two values on a condition",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					ChefTags:   []string{"area_51"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"area_51", "area_52"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "chefTags: two rules both matching on different fields",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					ChefTags:         []string{"area_51"},
					OrganizationName: "org1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"area_51"},
								},
							},
						},
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "chefTags: two rules with only one matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					ChefTags:         []string{"area_51"},
					OrganizationName: "org1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"area_51"},
								},
							},
						},
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org2"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "chefTags: two conditions with only one matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					ChefTags:         []string{"area_51"},
					OrganizationName: "org1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"area_51"},
								},
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org2"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "chefTags: multiple roles one matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					ChefTags:   []string{"area_51", "area_52", "area_53"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"area_51"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "chefTags: multiple roles, none matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					ChefTags:   []string{"area_51", "area_52", "area_53"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"area_54"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "chefTags: setting the project to unassigned when there are no chef tags",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_TAG,
									Values:    []string{"area_54"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},

		// policyGroups
		{
			description: "policyGroups: Single rule matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:  newUUID(),
					NodeName:    "node_1",
					PolicyGroup: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"prod"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "policyGroups: Single rule not matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:  newUUID(),
					NodeName:    "node_1",
					PolicyGroup: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"dev"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "policyGroups: Single rule differing case not matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:  newUUID(),
					NodeName:    "node_1",
					PolicyGroup: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"Prod"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "policyGroups: Single rule with two values on one condition",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:  newUUID(),
					NodeName:    "node_1",
					PolicyGroup: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"prod", "dev"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "policyGroups: two rules both matching on different fields",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					PolicyGroup:      "prod",
					OrganizationName: "org1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"prod"},
								},
							},
						},
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "policyGroups: two rules with only one matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					PolicyGroup:      "prod",
					OrganizationName: "org1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
									Values:    []string{"dev"},
								},
							},
						},
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},

		// policyNames
		{
			description: "policyNames: Single rule matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					PolicyName: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "policyNames: Single rule not matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					PolicyName: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"dev"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "policyNames: Single rule differing case not matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					PolicyName: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"Prod"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "policyNames: Single rule with two values on a condition",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					PolicyName: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod", "dev"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "policyNames: two rules both matching on different fields",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					PolicyName:       "prod",
					OrganizationName: "org1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"prod"},
								},
							},
						},
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "policyNames: two rules with only one matching",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					PolicyName:       "prod",
					OrganizationName: "org1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
									Values:    []string{"dev"},
								},
							},
						},
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},

		// General
		{
			description: "project rules does not have any rules",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					OrganizationName: "org1",
					Environment:      "env1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"old_tag": {
					Rules: []*authz.ProjectRule{},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "project rule does not have any conditions",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					NodeName:         "node_1",
					OrganizationName: "org1",
					Environment:      "env1",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*authz.ProjectRules{
				"old_tag": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{},
						},
					},
				},
			},
			projectIDs: []string{},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("node match: %s", test.description),
			func(t *testing.T) {
				suite.IngestNodes([]iBackend.Node{test.node})
				// Send a project rules update event
				esJobID, err := suite.ingest.UpdateNodeProjectTags(ctx, test.projects)
				assert.Nil(t, err)

				jobStatus, err := suite.ingest.JobStatus(ctx, esJobID)
				assert.Nil(t, err)
				for !jobStatus.Completed {
					time.Sleep(time.Millisecond * 5)
					jobStatus, err = suite.ingest.JobStatus(ctx, esJobID)
					assert.Nil(t, err)
					if err != nil {
						assert.FailNow(t, "testing job status")
					}
				}

				suite.RefreshIndices(mappings.NodeState.Index)

				// assert the node's project IDs
				actualNodes, err := suite.GetNodes(100)
				assert.Nil(t, err)
				assert.Equal(t, 1, len(actualNodes), "wrong number of nodes retrieved")

				actualNode := actualNodes[0]

				assert.ElementsMatch(t, test.projectIDs, actualNode.Projects)

				suite.DeleteAllDocuments()
			})
	}
}

func TestProjectUpdatePainlessElasticsearchScriptNoNodes(t *testing.T) {

	ctx := context.Background()
	projects := map[string]*authz.ProjectRules{
		"project 9": {
			Rules: []*authz.ProjectRule{
				{
					Conditions: []*authz.Condition{
						{
							Attribute: authz.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
							Values:    []string{"dev"},
						},
					},
				},
			},
		},
	}

	// Send a project rules update event
	esJobID, err := suite.ingest.UpdateNodeProjectTags(ctx, projects)
	assert.Nil(t, err)

	jobStatus, err := suite.ingest.JobStatus(ctx, esJobID)
	assert.Nil(t, err)
	for !jobStatus.Completed {
		time.Sleep(time.Millisecond * 5)
		jobStatus, err = suite.ingest.JobStatus(ctx, esJobID)
		assert.Nil(t, err)
		if err != nil {
			assert.FailNow(t, "testing job status")
		}
	}

	suite.RefreshIndices(mappings.NodeState.Index)

	// assert the node's project IDs
	actualNodes, err := suite.GetNodes(100)
	assert.Nil(t, err)
	assert.Equal(t, 0, len(actualNodes), "wrong number of nodes retrieved")

	suite.DeleteAllDocuments()
}
