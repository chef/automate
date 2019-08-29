package processor

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/golang/mock/gomock"
	"github.com/stretchr/testify/assert"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

// All the conditions must be true for a rule to be true (ANDed together).
// Only one rule has to be true for the project to match (ORed together).
// If there are no rules the project does not match
// If the rule does not have any conditions it does not match any resources.
func TestReportProjectRulesMatching(t *testing.T) {
	cases := []struct {
		description string
		report      *relaxting.ESInSpecReport
		rules       []*iam_v2.ProjectRule
		matching    bool
	}{
		{
			description: "A project with no rules does not match any resources",
			matching:    false,
			report:      &relaxting.ESInSpecReport{},
			rules:       []*iam_v2.ProjectRule{},
		},
		{
			description: "A project with one rules that does not have any conditions does not match any resources",
			matching:    false,
			report:      &relaxting.ESInSpecReport{},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
				},
			},
		},
		{
			description: "Single rule single condition matching incorrect rule type",
			matching:    false,
			report:      &relaxting.ESInSpecReport{},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_EVENT,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
							Values:    []string{"production"},
						},
					},
				},
			},
		},

		// Environment
		{
			description: "Environment: Single rule single condition; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				Environment: "production",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
							Values:    []string{"production"},
						},
					},
				},
			},
		},
		{
			description: "Environment: a rule's condition has two values for a field; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				Environment: "production",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
							Values:    []string{"production", "dev"},
						},
					},
				},
			},
		},
		{
			description: "Environment: two values in different fields only one matching in different rules matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				Environment: "production",
				Roles:       []string{"south-side"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
							Values:    []string{"production"},
						},
					},
				},
				{
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    []string{"north-side"},
						},
					},
				},
			},
		},
		{
			description: "Environment: two values in different fields only one matching same rule; non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				Environment: "production",
				Roles:       []string{"south-side"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
							Values:    []string{"production"},
						},
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    []string{"north-side"},
						},
					},
				},
			},
		},
		{
			description: "Environment: Single rule different case non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				Environment: "Production",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
							Values:    []string{"production"},
						},
					},
				},
			},
		},

		// Orgs
		{
			description: "Org: Single rule matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
							Values:    []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Org: Single rule differing case non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				OrganizationName: "Org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
							Values:    []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Org: Single rule non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
							Values:    []string{"org_2"},
						},
					},
				},
			},
		},
		{
			description: "Org: two on same field rule matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
							Values:    []string{"org_1", "org_2"},
						},
					},
				},
			},
		},

		// Chef Server
		{
			description: "Chef Server: Single rule matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				SourceFQDN: "chef_server_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
							Values:    []string{"chef_server_1"},
						},
					},
				},
			},
		},
		{
			description: "Chef Server: Single rule differing case non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				SourceFQDN: "Chef_server_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
							Values:    []string{"chef_server_1"},
						},
					},
				},
			},
		},
		{
			description: "Chef Server: Single rule non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				SourceFQDN: "Chef_server_2",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
							Values:    []string{"chef_server_1"},
						},
					},
				},
			},
		},
		{
			description: "Chef Server: two condition values on the same field one matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				SourceFQDN: "chef_server_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
							Values:    []string{"chef_server_1", "chef_server_2"},
						},
					},
				},
			},
		},

		// Role
		{
			description: "Role: Single rule single condition matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				Roles: []string{"area_51"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Role: Single value in rule condition, multiple node roles; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				Roles: []string{"area_51", "vandenberg", "hunter army airfield"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Role: Two rules with one condition with one matching, multiple node roles; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				Roles: []string{"area_51", "vandenberg", "hunter army airfield"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    []string{"area_51"},
						},
					},
				},
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    []string{"area_54"},
						},
					},
				},
			},
		},
		{
			description: "Role: One rule with one condition with two values with only one matching, multiple node roles; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				Roles: []string{"area_51", "vandenberg", "hunter army airfield"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    []string{"area_51", "area_54"},
						},
					},
				},
			},
		},
		{
			description: "Role: One rule with two conditions with only one matching, multiple node roles; non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				Roles: []string{"area_51", "vandenberg", "hunter army airfield"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    []string{"area_51"},
						},
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    []string{"area_54"},
						},
					},
				},
			},
		},
		{
			description: "Role: Single rule differing case non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				Roles: []string{"AREA_51"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Role: Single rule non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				Roles: []string{"area_52"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Role: single rule, two values on the same condition; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				Roles: []string{"area_52"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    []string{"area_51", "area_52"},
						},
					},
				},
			},
		},
		{
			description: "Role: one rule with only one matching condition; non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				Environment: "production 2",
				Roles:       []string{"area_52", "area_49"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    []string{"area_52"},
						},
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
							Values:    []string{"production"},
						},
					},
				},
			},
		},

		// Chef tag
		{
			description: "Chef Tags: Single rule one condition; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				Environment: "production 2",
				ChefTags:    []string{"area_51"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
							Values:    []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tags: Single rule multiple node chef tags matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				Environment: "production 2",
				ChefTags:    []string{"area_51", "vandenberg", "hunter army airfield"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
							Values:    []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tags: Single rule differing case non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				Environment: "production 2",
				ChefTags:    []string{"AREA_51"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
							Values:    []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tags: Single rule non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				ChefTags: []string{"area_52"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
							Values:    []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tags: two values on same conditions's field; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				ChefTags: []string{"area_52"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
							Values:    []string{"area_51", "area_52"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tag: two values on same condition, multiple chef tag; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				ChefTags: []string{"area_52", "vandenberg", "hunter army airfield"},
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
							Values:    []string{"area_51", "area_52"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tag: two rules on different fields; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				ChefTags:         []string{"area_52"},
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
							Values:    []string{"area_52"},
						},
					},
				},
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
							Values:    []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tag: two rules with only one having conditions that match; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				ChefTags:         []string{"area_52"},
				OrganizationName: "org_2",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_TAG,
							Values:    []string{"area_52"},
						},
					},
				},
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
							Values:    []string{"org_1"},
						},
					},
				},
			},
		},

		// Policy Group
		{
			description: "Policy Group: Single rule matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				PolicyGroup: "PolicyGroup",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
							Values:    []string{"PolicyGroup"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: Single rule differing case non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				PolicyGroup: "PolicyGroup",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
							Values:    []string{"policygroup"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: Single rule non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				PolicyGroup: "PolicyGroup",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
							Values:    []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: Single rule with a condition with two values on same field; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				PolicyGroup: "area_52",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
							Values:    []string{"area_51", "area_52"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: two rules with two having matching conditions on different fields; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				PolicyGroup:      "area_52",
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
							Values:    []string{"area_52"},
						},
					},
				},
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
							Values:    []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: two rules on different fields with only one matching; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				PolicyGroup:      "area_52",
				OrganizationName: "org_2",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
							Values:    []string{"area_52"},
						},
					},
				},
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
							Values:    []string{"org_1"},
						},
					},
				},
			},
		},

		// Policy Name
		{
			description: "😒Policy Name: Single rule matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				PolicyName: "PolicyName",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
							Values:    []string{"PolicyName"},
						},
					},
				},
			},
		},
		{
			description: "Policy Name: Single rule differing case non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				PolicyName: "PolicyGroup",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
							Values:    []string{"policygroup"},
						},
					},
				},
			},
		},
		{
			description: "Policy Name: Single rule non-matching",
			matching:    false,
			report: &relaxting.ESInSpecReport{
				PolicyName: "area_52",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
							Values:    []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Policy Name: Single rule with two values on a condition; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				PolicyName: "area_52",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
							Values:    []string{"area_51", "area_52"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: two rules both matching on different fields; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				PolicyName:       "area_52",
				OrganizationName: "org_1",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
							Values:    []string{"area_52"},
						},
					},
				},
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
							Values:    []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: two rules with only one matching; matching",
			matching:    true,
			report: &relaxting.ESInSpecReport{
				PolicyName:       "area_52",
				OrganizationName: "org_2",
			},
			rules: []*iam_v2.ProjectRule{
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
							Values:    []string{"area_52"},
						},
					},
				},
				{
					Type: iam_v2.ProjectRuleTypes_NODE,
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
							Values:    []string{"org_1"},
						},
					},
				},
			},
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("node match: %s", test.description),
			func(t *testing.T) {
				projectMatch := reportMatchesRules(test.report, test.rules)

				assert.Equal(t, test.matching, projectMatch, test.description)
			})
	}
}

// When 5 messages are in the inbox the ListRulesForAllProjects function is only called once.
func TestBundler5Messages(t *testing.T) {
	numberOfMessages := 5
	inbox := make(chan message.Compliance, numberOfMessages)
	listProjectRulesCount := 0
	authzClient := iam_v2.NewMockProjectsClient(gomock.NewController(t))
	authzClient.EXPECT().ListRulesForAllProjects(gomock.Any(), gomock.Any()).DoAndReturn(
		func(ctx interface{}, in interface{}) (*iam_v2.ListRulesForAllProjectsResp, error) {
			listProjectRulesCount++
			return &iam_v2.ListRulesForAllProjectsResp{}, nil
		})
	done := make(chan error)
	ctx := context.Background()

	for i := 0; i < numberOfMessages; i++ {
		inbox <- message.Compliance{
			QueueTime:     time.Now(),
			InspecReport:  &relaxting.ESInSpecReport{Environment: "", Roles: []string{}},
			InspecSummary: &relaxting.ESInSpecSummary{},
			Ctx:           ctx,
			Done:          done,
		}
	}
	close(inbox)

	out := reportProjectTagger(inbox, authzClient)

	for i := 0; i < numberOfMessages; i++ {
		<-out
	}

	assert.Equal(t, 1, listProjectRulesCount)
}

// A simple run through of the bundle project tagger processor.
// Two messages are sent through with only one matching a project.
func TestBundlerMatchProjectRule(t *testing.T) {
	inbox := make(chan message.Compliance, 2)
	testProjectName := "Test"
	environmentsName := "env_1"
	projectRules := map[string]*iam_v2.ProjectRules{}
	projectRules[testProjectName] = &iam_v2.ProjectRules{
		Rules: []*iam_v2.ProjectRule{
			{
				Type: iam_v2.ProjectRuleTypes_NODE,
				Conditions: []*iam_v2.Condition{
					{
						Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
						Values:    []string{environmentsName},
					},
				},
			},
		},
	}
	authzClient := iam_v2.NewMockProjectsClient(gomock.NewController(t))
	authzClient.EXPECT().ListRulesForAllProjects(gomock.Any(), gomock.Any()).Return(
		&iam_v2.ListRulesForAllProjectsResp{ProjectRules: projectRules}, nil)

	done := make(chan error)
	ctx := context.Background()

	report1 := message.Compliance{
		QueueTime:     time.Now(),
		InspecReport:  &relaxting.ESInSpecReport{Environment: environmentsName, Roles: []string{}},
		InspecSummary: &relaxting.ESInSpecSummary{},
		Ctx:           ctx,
		Done:          done,
	}

	report2 := message.Compliance{
		QueueTime:     time.Now(),
		InspecReport:  &relaxting.ESInSpecReport{Environment: "no_match", Roles: []string{}},
		InspecSummary: &relaxting.ESInSpecSummary{},
		Ctx:           ctx,
		Done:          done,
	}

	inbox <- report1
	inbox <- report2
	close(inbox)

	out := BundleReportProjectTagger(authzClient)(inbox)

	processMsg1 := <-out
	assert.Equal(t, []string{testProjectName}, processMsg1.InspecReport.Projects)
	assert.Equal(t, []string{testProjectName}, processMsg1.InspecSummary.Projects)

	processMsg2 := <-out
	assert.Equal(t, []string{}, processMsg2.InspecReport.Projects)
}

func TestBundlerMatchProjectRuleEventRuleType(t *testing.T) {
	inbox := make(chan message.Compliance, 2)
	testProjectName := "Test"
	environmentsName := "env_1"
	projectRules := map[string]*iam_v2.ProjectRules{}
	projectRules[testProjectName] = &iam_v2.ProjectRules{
		Rules: []*iam_v2.ProjectRule{
			{
				Type: iam_v2.ProjectRuleTypes_EVENT,
				Conditions: []*iam_v2.Condition{
					{
						Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
						Values:    []string{environmentsName},
					},
				},
			},
		},
	}
	authzClient := iam_v2.NewMockProjectsClient(gomock.NewController(t))
	authzClient.EXPECT().ListRulesForAllProjects(gomock.Any(), gomock.Any()).Return(
		&iam_v2.ListRulesForAllProjectsResp{ProjectRules: projectRules}, nil)

	done := make(chan error)
	ctx := context.Background()

	report1 := message.Compliance{
		QueueTime:     time.Now(),
		InspecReport:  &relaxting.ESInSpecReport{Environment: environmentsName, Roles: []string{}},
		InspecSummary: &relaxting.ESInSpecSummary{},
		Ctx:           ctx,
		Done:          done,
	}

	report2 := message.Compliance{
		QueueTime:     time.Now(),
		InspecReport:  &relaxting.ESInSpecReport{Environment: "no_match", Roles: []string{}},
		InspecSummary: &relaxting.ESInSpecSummary{},
		Ctx:           ctx,
		Done:          done,
	}

	inbox <- report1
	inbox <- report2
	close(inbox)

	out := BundleReportProjectTagger(authzClient)(inbox)

	processMsg1 := <-out
	assert.Equal(t, []string{}, processMsg1.InspecReport.Projects)
	assert.Equal(t, []string{}, processMsg1.InspecSummary.Projects)

	processMsg2 := <-out
	assert.Equal(t, []string{}, processMsg2.InspecReport.Projects)
	assert.Equal(t, []string{}, processMsg2.InspecSummary.Projects)
}

// A scan job report should not be tagged.
// A report is a scan job if it has a JobID.
func TestBundlerWithScanJobReport(t *testing.T) {
	inbox := make(chan message.Compliance, 100)
	testProjectName := "Test"
	environmentsName := "env_1"
	projectRules := map[string]*iam_v2.ProjectRules{}
	projectRules[testProjectName] = &iam_v2.ProjectRules{
		Rules: []*iam_v2.ProjectRule{
			{
				Type: iam_v2.ProjectRuleTypes_NODE,
				Conditions: []*iam_v2.Condition{
					{
						Attribute: iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT,
						Values:    []string{environmentsName},
					},
				},
			},
		},
	}
	authzClient := iam_v2.NewMockProjectsClient(gomock.NewController(t))
	authzClient.EXPECT().ListRulesForAllProjects(gomock.Any(), gomock.Any()).Return(
		&iam_v2.ListRulesForAllProjectsResp{ProjectRules: projectRules}, nil)
	// {
	// 	listProjectRulesCount: 0,
	// 	projectRules:          projectRules}
	done := make(chan error)
	ctx := context.Background()

	report1 := message.Compliance{
		QueueTime:     time.Now(),
		InspecReport:  &relaxting.ESInSpecReport{Environment: environmentsName, Roles: []string{}, JobID: "scan job"},
		InspecSummary: &relaxting.ESInSpecSummary{},
		Ctx:           ctx,
		Done:          done,
	}

	report2 := message.Compliance{
		QueueTime:     time.Now(),
		InspecReport:  &relaxting.ESInSpecReport{Environment: "no_match", Roles: []string{}, JobID: "scan job"},
		InspecSummary: &relaxting.ESInSpecSummary{},
		Ctx:           ctx,
		Done:          done,
	}

	report3 := message.Compliance{
		QueueTime:     time.Now(),
		InspecReport:  &relaxting.ESInSpecReport{Environment: environmentsName, Roles: []string{}},
		InspecSummary: &relaxting.ESInSpecSummary{},
		Ctx:           ctx,
		Done:          done,
	}

	inbox <- report1
	inbox <- report2
	inbox <- report3
	close(inbox)

	out := BundleReportProjectTagger(authzClient)(inbox)

	processMsg1 := <-out
	assert.Equal(t, ([]string)(nil), processMsg1.InspecReport.Projects)
	assert.Equal(t, ([]string)(nil), processMsg1.InspecSummary.Projects)

	processMsg2 := <-out
	assert.Equal(t, ([]string)(nil), processMsg2.InspecReport.Projects)
	assert.Equal(t, ([]string)(nil), processMsg2.InspecSummary.Projects)

	processMsg3 := <-out
	assert.Equal(t, []string{testProjectName}, processMsg3.InspecReport.Projects)
	assert.Equal(t, []string{testProjectName}, processMsg3.InspecSummary.Projects)
}
