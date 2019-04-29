package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	automate_event_type "github.com/chef/automate/components/event-service/server"
	project_update_tags "github.com/chef/automate/lib/authz"
	"github.com/golang/mock/gomock"
	"github.com/golang/protobuf/ptypes"
	_struct "github.com/golang/protobuf/ptypes/struct"
	"github.com/stretchr/testify/assert"
)

func TestProjectUpdate(t *testing.T) {
	var (
		ctx = context.Background()
	)

	cases := []struct {
		description string
		report      *relaxting.ESInSpecReport
		summary     *relaxting.ESInSpecSummary
		projects    map[string]*iam_v2.ProjectRules
		projectIDs  []string
	}{
		{
			description: "Environment: Single rule matching condition",
			report: &relaxting.ESInSpecReport{
				Environment: "env1",
				Projects:    []string{"old_tag"},
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env1",
				Projects:    []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ENVIRONMENTS,
									Values: []string{"env1"},
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
			report: &relaxting.ESInSpecReport{
				Environment: "env2",
				Projects:    []string{"old_tag"},
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env2",
				Projects:    []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ENVIRONMENTS,
									Values: []string{"env1", "env2"},
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
			report: &relaxting.ESInSpecReport{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				Roles:       []string{"backend"},
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				Roles:       []string{"backend"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ENVIRONMENTS,
									Values: []string{"env1"},
								},
								{
									Type:   iam_v2.ProjectRuleConditionTypes_ROLES,
									Values: []string{"backend"},
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
			report: &relaxting.ESInSpecReport{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				Roles:       []string{"backend"},
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				Roles:       []string{"backend"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ENVIRONMENTS,
									Values: []string{"env1"},
								},
								{
									Type:   iam_v2.ProjectRuleConditionTypes_ROLES,
									Values: []string{"frontend"},
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
			report: &relaxting.ESInSpecReport{
				Environment: "env1",
				Projects:    []string{"old_tag"},
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env1",
				Projects:    []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ENVIRONMENTS,
									Values: []string{"env2"},
								},
							},
						},
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ENVIRONMENTS,
									Values: []string{"env1"},
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
			report: &relaxting.ESInSpecReport{
				Environment: "env1",
				Projects:    []string{"old_tag"},
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env1",
				Projects:    []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ENVIRONMENTS,
									Values: []string{"env2"},
								},
							},
						},
					},
				},
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ENVIRONMENTS,
									Values: []string{"env1"},
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
			report: &relaxting.ESInSpecReport{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				Roles:       []string{"backend"},
			},
			summary: &relaxting.ESInSpecSummary{
				Environment: "env1",
				Projects:    []string{"old_tag"},
				Roles:       []string{"backend"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_ROLES,
									Values: []string{"backend"},
								},
							},
						},
					},
				},
				"project3": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ENVIRONMENTS,
									Values: []string{"env1"},
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
			description: "roles: Single rule matching",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51"},
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_ROLES,
									Values: []string{"area_51"},
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
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51"},
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_ROLES,
									Values: []string{"area_52"},
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
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				Roles:    []string{"Area_51"},
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				Roles:    []string{"Area_51"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_ROLES,
									Values: []string{"area_51"},
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
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51"},
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_ROLES,
									Values: []string{"area_51", "area_52"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "roles: multiple roles one matching",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51", "area_52", "area_53"},
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51", "area_52", "area_53"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_ROLES,
									Values: []string{"area_51"},
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
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51", "area_52", "area_53"},
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				Roles:    []string{"area_51", "area_52", "area_53"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_ROLES,
									Values: []string{"area_54"},
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
			description: "chefServers: Single rule matching",
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				SourceFQDN: "chef-server.org",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				SourceFQDN: "chef-server.org",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_SERVERS,
									Values: []string{"chef-server.org"},
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
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				SourceFQDN: "chef-server2.org",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				SourceFQDN: "chef-server2.org",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_SERVERS,
									Values: []string{"chef-server.org"},
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
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				SourceFQDN: "Chef-server.org",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				SourceFQDN: "Chef-server.org",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_SERVERS,
									Values: []string{"chef-server.org"},
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
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				SourceFQDN: "chef-server.org",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				SourceFQDN: "chef-server.org",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_SERVERS,
									Values: []string{"chef-server.org", "chef-server2.org"},
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
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				SourceFQDN:       "chef-server.org",
				OrganizationName: "org1",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				SourceFQDN:       "chef-server.org",
				OrganizationName: "org1",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_SERVERS,
									Values: []string{"chef-server.org"},
								},
							},
						},
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ORGS,
									Values: []string{"org1"},
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
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				SourceFQDN:       "chef-server.org",
				OrganizationName: "org1",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				SourceFQDN:       "chef-server.org",
				OrganizationName: "org1",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_SERVERS,
									Values: []string{"chef-server2.org"},
								},
							},
						},
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ORGS,
									Values: []string{"org1"},
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
			description: "Org: Single rule matching update",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				OrganizationName: "org1",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				OrganizationName: "org1",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ORGS,
									Values: []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "Org: no project matches",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				OrganizationName: "org1",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				OrganizationName: "org1",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ORGS,
									Values: []string{"org2"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "Org: one matching project",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				OrganizationName: "org1",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				OrganizationName: "org1",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"old_tag": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ORGS,
									Values: []string{"org1"},
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
			description: "policyGroups: Single rule matching",
			report: &relaxting.ESInSpecReport{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_POLICY_GROUP,
									Values: []string{"prod"},
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
			report: &relaxting.ESInSpecReport{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_POLICY_GROUP,
									Values: []string{"dev"},
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
			report: &relaxting.ESInSpecReport{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_POLICY_GROUP,
									Values: []string{"Prod"},
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
			report: &relaxting.ESInSpecReport{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:    []string{"old_tag"},
				PolicyGroup: "prod",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_POLICY_GROUP,
									Values: []string{"prod", "dev"},
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
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				PolicyGroup:      "prod",
				OrganizationName: "org1",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				PolicyGroup:      "prod",
				OrganizationName: "org1",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_POLICY_GROUP,
									Values: []string{"prod"},
								},
							},
						},
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ORGS,
									Values: []string{"org1"},
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
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				PolicyGroup:      "prod",
				OrganizationName: "org1",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				PolicyGroup:      "prod",
				OrganizationName: "org1",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_POLICY_GROUP,
									Values: []string{"dev"},
								},
							},
						},
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ORGS,
									Values: []string{"org1"},
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
			description: "policyNames: Single rule matching",
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_POLICY_NAME,
									Values: []string{"prod"},
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
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_POLICY_NAME,
									Values: []string{"dev"},
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
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_POLICY_NAME,
									Values: []string{"Prod"},
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
			report: &relaxting.ESInSpecReport{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:   []string{"old_tag"},
				PolicyName: "prod",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_POLICY_NAME,
									Values: []string{"prod", "dev"},
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
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				PolicyName:       "prod",
				OrganizationName: "org1",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				PolicyName:       "prod",
				OrganizationName: "org1",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_POLICY_NAME,
									Values: []string{"prod"},
								},
							},
						},
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ORGS,
									Values: []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "policyNames: a rule with two conditions with only one matching",
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				PolicyName:       "prod",
				OrganizationName: "org1",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				PolicyName:       "prod",
				OrganizationName: "org1",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_POLICY_NAME,
									Values: []string{"dev"},
								},
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ORGS,
									Values: []string{"org1"},
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
			description: "chefTags: Single rule matching",
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51"},
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_TAGS,
									Values: []string{"area_51"},
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
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51"},
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_TAGS,
									Values: []string{"area_52"},
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
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				ChefTags: []string{"Area_51"},
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				ChefTags: []string{"Area_51"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_TAGS,
									Values: []string{"area_51"},
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
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51"},
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_TAGS,
									Values: []string{"area_51", "area_52"},
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
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				ChefTags:         []string{"area_51"},
				OrganizationName: "org1",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				ChefTags:         []string{"area_51"},
				OrganizationName: "org1",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_TAGS,
									Values: []string{"area_51"},
								},
							},
						},
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ORGS,
									Values: []string{"org1"},
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
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				ChefTags:         []string{"area_51"},
				OrganizationName: "org1",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				ChefTags:         []string{"area_51"},
				OrganizationName: "org1",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_TAGS,
									Values: []string{"area_51"},
								},
							},
						},
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ORGS,
									Values: []string{"org2"},
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
			report: &relaxting.ESInSpecReport{
				Projects:         []string{"old_tag"},
				ChefTags:         []string{"area_51"},
				OrganizationName: "org1",
			},
			summary: &relaxting.ESInSpecSummary{
				Projects:         []string{"old_tag"},
				ChefTags:         []string{"area_51"},
				OrganizationName: "org1",
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_TAGS,
									Values: []string{"area_51"},
								},
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_ORGS,
									Values: []string{"org2"},
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
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51", "area_52", "area_53"},
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51", "area_52", "area_53"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_TAGS,
									Values: []string{"area_51"},
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
			report: &relaxting.ESInSpecReport{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51", "area_52", "area_53"},
			},
			summary: &relaxting.ESInSpecSummary{
				Projects: []string{"old_tag"},
				ChefTags: []string{"area_51", "area_52", "area_53"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Type:   iam_v2.ProjectRuleConditionTypes_CHEF_TAGS,
									Values: []string{"area_54"},
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
				suite.InsertInspecReports([]*relaxting.ESInSpecReport{test.report})
				suite.InsertInspecSummaries([]*relaxting.ESInSpecSummary{test.summary})

				// Send a project rules update event
				esJobID, err := suite.ingesticESClient.UpdateReportProjectsTags(ctx, test.projects)
				assert.Nil(t, err)

				suite.WaitForESJobToComplete(esJobID)

				suite.RefreshComplianceReportIndex()

				esJobID, err = suite.ingesticESClient.UpdateSummaryProjectsTags(ctx, test.projects)
				assert.Nil(t, err)

				suite.WaitForESJobToComplete(esJobID)

				suite.RefreshComplianceSummaryIndex()

				reports, err := suite.GetAllReportsESInSpecReport()
				assert.NoError(t, err)
				assert.Equal(t, 1, len(reports))

				updatedReport := reports[0]

				assert.ElementsMatch(t, test.projectIDs, updatedReport.Projects)

				summaries, err := suite.GetAllSummaryESInSpecSummary()
				assert.NoError(t, err)
				assert.Equal(t, 1, len(summaries))

				updatedSummary := summaries[0]

				assert.ElementsMatch(t, test.projectIDs, updatedSummary.Projects)

				suite.DeleteAllDocuments()
			})
	}
}

func TestErrorWhenProjectUpdateIDNotSent(t *testing.T) {
	event := &automate_event.EventMsg{
		EventID:   "lskdjflsdkfj",
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdate},
		Published: ptypes.TimestampNow(),
	}

	_, err := suite.ComplianceIngestServer.HandleEvent(context.Background(), event)
	assert.Error(t, err)

	event = &automate_event.EventMsg{
		EventID:   "lskdjflsdkfj",
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdate},
		Published: ptypes.TimestampNow(),
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{},
		},
	}
	assert.Error(t, err)

	event = &automate_event.EventMsg{
		EventID:   "lskdjflsdkfj",
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdate},
		Published: ptypes.TimestampNow(),
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: {
					Kind: &_struct.Value_StringValue{
						StringValue: "",
					},
				},
			},
		},
	}
	assert.Error(t, err)
}

func TestStartProjectUpdateWhenIDIsSent(t *testing.T) {
	var lastEventSent *automate_event.EventMsg
	localSuite := NewLocalSuite(t)
	defer localSuite.GlobalTeardown()
	localSuite.EventServiceClientMock.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().DoAndReturn(
		func(ctx context.Context, in *automate_event.PublishRequest) (*automate_event.PublishResponse, error) {
			lastEventSent = in.Msg
			return &automate_event.PublishResponse{}, nil
		})
	localSuite.ProjectsClientMock.EXPECT().ListProjectRules(gomock.Any(), gomock.Any()).AnyTimes().Return(
		&iam_v2.ProjectCollectionRulesResp{}, nil)
	event := &automate_event.EventMsg{
		EventID:   "lskdjflsdkfj",
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdate},
		Published: ptypes.TimestampNow(),
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: {
					Kind: &_struct.Value_StringValue{
						StringValue: "TestNoErrorWhenProjectUpdateIDIsSent",
					},
				},
			},
		},
	}

	_, err := localSuite.ComplianceIngestServer.HandleEvent(context.Background(), event)
	assert.NoError(t, err)

	// Wait for job to complete
	for {
		time.Sleep(time.Millisecond * 100)
		if lastEventSent.Type.Name != automate_event_type.ProjectRulesUpdateStatus {
			continue
		}

		if lastEventSent.Data.Fields["Completed"].GetBoolValue() {
			break
		}
	}
}

func TestTwoUpdateSameTimeFailureEvent(t *testing.T) {
	var lastEventSent *automate_event.EventMsg
	localSuite := NewLocalSuite(t)
	defer localSuite.GlobalTeardown()
	localSuite.ProjectsClientMock.EXPECT().ListProjectRules(gomock.Any(), gomock.Any()).AnyTimes().Return(
		&iam_v2.ProjectCollectionRulesResp{}, nil)
	localSuite.EventServiceClientMock.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().DoAndReturn(
		func(ctx interface{}, in *automate_event.PublishRequest) (*automate_event.PublishResponse, error) {
			lastEventSent = in.Msg
			return &automate_event.PublishResponse{}, nil
		})
	event1 := &automate_event.EventMsg{
		EventID:   "1",
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdate},
		Published: ptypes.TimestampNow(),
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: {
					Kind: &_struct.Value_StringValue{
						StringValue: "one",
					},
				},
			},
		},
	}

	_, err := localSuite.ComplianceIngestServer.HandleEvent(context.Background(), event1)
	assert.NoError(t, err)

	assert.True(t, lastEventSent == nil ||
		lastEventSent.Type.Name == automate_event_type.ProjectRulesUpdateStatus)

	event2 := &automate_event.EventMsg{
		EventID:   "2",
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdate},
		Published: ptypes.TimestampNow(),
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: {
					Kind: &_struct.Value_StringValue{
						StringValue: "two",
					},
				},
			},
		},
	}

	_, err = localSuite.ComplianceIngestServer.HandleEvent(context.Background(), event2)
	assert.NoError(t, err)
	assert.NotNil(t, lastEventSent)
	assert.Equal(t, automate_event_type.ProjectRulesUpdateFailed, lastEventSent.Type.Name)
	assert.True(t, len(lastEventSent.Data.Fields["message"].GetStringValue()) > 0)

	assert.Equal(t, "two", lastEventSent.Data.Fields[project_update_tags.ProjectUpdateIDTag].GetStringValue())

	// Wait for job to complete
	for {
		time.Sleep(time.Millisecond * 100)
		if lastEventSent == nil || lastEventSent.Type.Name != automate_event_type.ProjectRulesUpdateStatus {
			continue
		}

		if lastEventSent.Data.Fields["Completed"].GetBoolValue() {
			break
		}
	}
}
