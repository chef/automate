package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

func TestProjectUpdate(t *testing.T) {
	var (
		ctx = context.Background()
	)

	cases := []struct {
		description string
		report      *relaxting.ESInSpecReport
		summary     *relaxting.ESInSpecSummary
		projects    map[string]*authz.ProjectRules
		projectIDs  []string
	}{
		{
			description: "reports_projects_update_test.go => Environment: Single rule matching condition",
			report: &relaxting.ESInSpecReport{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				EndTime:     time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				EndTime:     time.Now(),
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
			description: "reports_projects_update_test.go => Environment: a rule's condition has two values for a field",
			report: &relaxting.ESInSpecReport{
				Environment: "env2",
				Projects:    []string{"old_tag"},
				EndTime:     time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env2",
				Projects:    []string{"old_tag"},
				EndTime:     time.Now(),
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
			description: "reports_projects_update_test.go => Environment: Single rule two matching conditions",
			report: &relaxting.ESInSpecReport{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				Roles:       []string{"backend"},
				EndTime:     time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				Roles:       []string{"backend"},
				EndTime:     time.Now(),
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
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"backend"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "reports_projects_update_test.go => Environment: Single rule, one non-matching condition",
			report: &relaxting.ESInSpecReport{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				Roles:       []string{"backend"},
				EndTime:     time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				Roles:       []string{"backend"},
				EndTime:     time.Now(),
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
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"frontend"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "reports_projects_update_test.go => Environment: two rules only one matching",
			report: &relaxting.ESInSpecReport{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				EndTime:     time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				EndTime:     time.Now(),
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
			description: "reports_projects_update_test.go => Environment: two project only one matching",
			report: &relaxting.ESInSpecReport{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				EndTime:     time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				EndTime:     time.Now(),
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
			description: "reports_projects_update_test.go => Environment: two matching projects",
			report: &relaxting.ESInSpecReport{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				Roles:       []string{"backend"},
				EndTime:     time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				Roles:       []string{"backend"},
				EndTime:     time.Now(),
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
									Values:    []string{"backend"},
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

		// roles
		{
			description: "reports_projects_update_test.go => roles: Single rule matching",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51"},
				EndTime:  time.Now(),
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
			description: "reports_projects_update_test.go => roles: Single rule not matching",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51"},
				EndTime:  time.Now(),
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
			description: "reports_projects_update_test.go => roles: Single rule differing case not matching",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				Roles:    []string{"Area_51"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				Roles:    []string{"Area_51"},
				EndTime:  time.Now(),
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
			description: "reports_projects_update_test.go => roles: Single rule with two values on a condition",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51"},
				EndTime:  time.Now(),
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
			description: "reports_projects_update_test.go => roles: multiple roles one matching",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51", "area_52", "area_53"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51", "area_52", "area_53"},
				EndTime:  time.Now(),
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
			description: "reports_projects_update_test.go => roles: multiple roles, none matching",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51", "area_52", "area_53"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51", "area_52", "area_53"},
				EndTime:  time.Now(),
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
			description: "reports_projects_update_test.go => roles: setting the project to unassigned when there are no roles",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				EndTime:  time.Now(),
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

		// ChefServers
		{
			description: "reports_projects_update_test.go => chefServers: Single rule matching",
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				SourceFQDN: "chef-server.org",
				EndTime:    time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				SourceFQDN: "chef-server.org",
				EndTime:    time.Now(),
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
			description: "reports_projects_update_test.go => chefServers: Single rule not matching",
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				SourceFQDN: "chef-server2.org",
				EndTime:    time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				SourceFQDN: "chef-server2.org",
				EndTime:    time.Now(),
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
			description: "reports_projects_update_test.go => chefServers: Single rule differing case not matching",
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				SourceFQDN: "Chef-server.org",
				EndTime:    time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				SourceFQDN: "Chef-server.org",
				EndTime:    time.Now(),
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
			description: "reports_projects_update_test.go => chefServers: Single rule with two values on a condition",
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				SourceFQDN: "chef-server.org",
				EndTime:    time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				SourceFQDN: "chef-server.org",
				EndTime:    time.Now(),
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
			description: "reports_projects_update_test.go => chefServers: two rules both matching on different fields",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				SourceFQDN:       "chef-server.org",
				OrganizationName: "org1",
				EndTime:          time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				SourceFQDN:       "chef-server.org",
				OrganizationName: "org1",
				EndTime:          time.Now(),
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
			description: "reports_projects_update_test.go => chefServers: two rules with only one matching",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				SourceFQDN:       "chef-server.org",
				OrganizationName: "org1",
				EndTime:          time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				SourceFQDN:       "chef-server.org",
				OrganizationName: "org1",
				EndTime:          time.Now(),
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

		// ChefOrgs
		{
			description: "reports_projects_update_test.go => Org: Single rule matching update",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				OrganizationName: "org1",
				EndTime:          time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				OrganizationName: "org1",
				EndTime:          time.Now(),
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
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
			projectIDs: []string{"project9"},
		},
		{
			description: "reports_projects_update_test.go => Org: no project matches",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				OrganizationName: "org1",
				EndTime:          time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				OrganizationName: "org1",
				EndTime:          time.Now(),
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
			description: "reports_projects_update_test.go => Org: one matching project",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				OrganizationName: "org1",
				EndTime:          time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				OrganizationName: "org1",
				EndTime:          time.Now(),
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

		// PolicyGroup
		{
			description: "reports_projects_update_test.go => policyGroups: Single rule matching",
			report: &relaxting.ESInSpecReport{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
				EndTime:     time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
				EndTime:     time.Now(),
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
			description: "reports_projects_update_test.go => policyGroups: Single rule not matching",
			report: &relaxting.ESInSpecReport{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
				EndTime:     time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
				EndTime:     time.Now(),
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
			description: "reports_projects_update_test.go => policyGroups: Single rule differing case not matching",
			report: &relaxting.ESInSpecReport{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
				EndTime:     time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
				EndTime:     time.Now(),
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
			description: "reports_projects_update_test.go => policyGroups: Single rule with two values on one condition",
			report: &relaxting.ESInSpecReport{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
				EndTime:     time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
				EndTime:     time.Now(),
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
			description: "reports_projects_update_test.go => policyGroups: two rules both matching on different fields",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				PolicyGroup:      "prod",
				OrganizationName: "org1",
				EndTime:          time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				PolicyGroup:      "prod",
				OrganizationName: "org1",
				EndTime:          time.Now(),
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
			description: "reports_projects_update_test.go => policyGroups: two rules with only one matching",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				PolicyGroup:      "prod",
				OrganizationName: "org1",
				EndTime:          time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				PolicyGroup:      "prod",
				OrganizationName: "org1",
				EndTime:          time.Now(),
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

		// PolicyName
		{
			description: "reports_projects_update_test.go => policyNames: Single rule matching",
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
				EndTime:    time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
				EndTime:    time.Now(),
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
			description: "reports_projects_update_test.go => policyNames: Single rule not matching",
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
				EndTime:    time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
				EndTime:    time.Now(),
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
			description: "reports_projects_update_test.go => policyNames: Single rule differing case not matching",
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
				EndTime:    time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
				EndTime:    time.Now(),
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
			description: "reports_projects_update_test.go => policyNames: Single rule with two values on a condition",
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
				EndTime:    time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
				EndTime:    time.Now(),
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
			description: "reports_projects_update_test.go => policyNames: two rules both matching on different fields",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				PolicyName:       "prod",
				OrganizationName: "org1",
				EndTime:          time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				PolicyName:       "prod",
				OrganizationName: "org1",
				EndTime:          time.Now(),
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
			description: "reports_projects_update_test.go => policyNames: a rule with two conditions with only one matching",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				PolicyName:       "prod",
				OrganizationName: "org1",
				EndTime:          time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				PolicyName:       "prod",
				OrganizationName: "org1",
				EndTime:          time.Now(),
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

		// ChefTags
		{
			description: "reports_projects_update_test.go => chefTags: Single rule matching",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51"},
				EndTime:  time.Now(),
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
			description: "reports_projects_update_test.go => chefTags: Single rule not matching",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51"},
				EndTime:  time.Now(),
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
			description: "reports_projects_update_test.go => chefTags: Single rule differing case not matching",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				ChefTags: []string{"Area_51"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				ChefTags: []string{"Area_51"},
				EndTime:  time.Now(),
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
			description: "reports_projects_update_test.go => chefTags: Single rule with two values on a condition",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51"},
				EndTime:  time.Now(),
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
			description: "reports_projects_update_test.go => chefTags: two rules both matching on different fields",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				ChefTags:         []string{"area_51"},
				OrganizationName: "org1",
				EndTime:          time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				ChefTags:         []string{"area_51"},
				OrganizationName: "org1",
				EndTime:          time.Now(),
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
			description: "reports_projects_update_test.go => chefTags: two rules with only one matching",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				ChefTags:         []string{"area_51"},
				OrganizationName: "org1",
				EndTime:          time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				ChefTags:         []string{"area_51"},
				OrganizationName: "org1",
				EndTime:          time.Now(),
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
			description: "reports_projects_update_test.go => chefTags: two conditions with only one matching",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				ChefTags:         []string{"area_51"},
				OrganizationName: "org1",
				EndTime:          time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				ChefTags:         []string{"area_51"},
				OrganizationName: "org1",
				EndTime:          time.Now(),
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
			description: "reports_projects_update_test.go => chefTags: multiple roles one matching",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51", "area_52", "area_53"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51", "area_52", "area_53"},
				EndTime:  time.Now(),
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
			description: "reports_projects_update_test.go => chefTags: multiple roles, none matching",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51", "area_52", "area_53"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51", "area_52", "area_53"},
				EndTime:  time.Now(),
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
			description: "reports_projects_update_test.go => chefTags: setting the project to unassigned when there are no chef tags",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				EndTime:  time.Now(),
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				EndTime:  time.Now(),
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
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("report match: %s", test.description),
			func(t *testing.T) {
				_, err := suite.InsertInspecReports([]*relaxting.ESInSpecReport{test.report})
				require.NoError(t, err)
				_, err = suite.InsertInspecSummaries([]*relaxting.ESInSpecSummary{test.summary})
				require.NoError(t, err)

				defer suite.DeleteAllDocuments()
				// Send a project rules update event
				esJobID, err := suite.ingesticESClient.UpdateReportProjectsTags(ctx, test.projects)
				require.NoError(t, err)

				suite.WaitForESJobToComplete(esJobID)

				suite.RefreshComplianceReportIndex()

				esJobID, err = suite.ingesticESClient.UpdateSummaryProjectsTags(ctx, test.projects)
				require.NoError(t, err)

				suite.WaitForESJobToComplete(esJobID)

				suite.RefreshComplianceSummaryIndex()

				reports, err := suite.GetAllReportsESInSpecReport()
				require.NoError(t, err)
				require.Equal(t, 1, len(reports))

				updatedReport := reports[0]

				assert.ElementsMatch(t, test.projectIDs, updatedReport.Projects)

				summaries, err := suite.GetAllSummaryESInSpecSummary()
				require.NoError(t, err)
				require.Equal(t, 1, len(summaries))

				updatedSummary := summaries[0]

				assert.ElementsMatch(t, test.projectIDs, updatedSummary.Projects)
			})
	}
}

func TestProjectUpdateNoReports(t *testing.T) {
	ctx := context.Background()
	projects := map[string]*authz.ProjectRules{
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
	}

	// Send a project rules update event
	esJobID, err := suite.ingesticESClient.UpdateReportProjectsTags(ctx, projects)
	require.NoError(t, err)

	suite.WaitForESJobToComplete(esJobID)

	suite.RefreshComplianceReportIndex()

	esJobID, err = suite.ingesticESClient.UpdateSummaryProjectsTags(ctx, projects)
	require.NoError(t, err)

	suite.WaitForESJobToComplete(esJobID)

	suite.RefreshComplianceSummaryIndex()

	reports, err := suite.GetAllReportsESInSpecReport()
	require.NoError(t, err)
	require.Equal(t, 0, len(reports))

	summaries, err := suite.GetAllSummaryESInSpecSummary()
	require.NoError(t, err)
	require.Equal(t, 0, len(summaries))
}
