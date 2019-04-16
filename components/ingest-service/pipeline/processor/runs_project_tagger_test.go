package processor

import (
	"context"
	"fmt"
	"testing"

	"github.com/golang/mock/gomock"
	"github.com/stretchr/testify/assert"

	chef "github.com/chef/automate/api/external/ingest/request"
	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	rules_tags "github.com/chef/automate/lib/authz"
)

// All the conditions must be true for a rule to be true (ANDed together).
// Only one rule has to be true for the project to match (ORed together).
// If there are no rules the project does not match
// If the rule does not have any conditions it does not match any resources.
func TestNodeProjectRulesMatching(t *testing.T) {
	cases := []struct {
		description string
		node        backend.Node
		rules       []*iam_v2.ProjectRule
		matching    bool
	}{
		{
			description: "A project with no rules does not match any resources",
			matching:    false,
			node:        backend.Node{},
			rules:       []*iam_v2.ProjectRule{},
		},
		{
			description: "A project with one rules that does not have any conditions does not match any resources",
			matching:    false,
			node:        backend.Node{},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{},
			},
		},

		// Environment
		{
			description: "Environment: Single rule single condition; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Environment: "production",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefEnvironmentsTag,
							Values: []string{"production"},
						},
					},
				},
			},
		},
		{
			description: "Environment: a rule's condition has two values for a field; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Environment: "production",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefEnvironmentsTag,
							Values: []string{"production", "dev"},
						},
					},
				},
			},
		},
		{
			description: "Environment: two values in different fields only one matching in different rules matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Environment:      "production",
					OrganizationName: "org_2",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefEnvironmentsTag,
							Values: []string{"production"},
						},
					},
				},
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Environment: two values in different fields only one matching same rule; non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Environment:      "production",
					OrganizationName: "org_2",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefEnvironmentsTag,
							Values: []string{"production"},
						},
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Environment: Single rule different case non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Environment: "Production",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefEnvironmentsTag,
							Values: []string{"production"},
						},
					},
				},
			},
		},

		// Orgs
		{
			description: "Org: Single rule matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					OrganizationName: "org_1",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Org: Single rule differing case non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					OrganizationName: "Org_1",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Org: Single rule non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					OrganizationName: "org_1",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_2"},
						},
					},
				},
			},
		},
		{
			description: "Org: two on same field rule matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					OrganizationName: "org_1",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
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
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					SourceFqdn: "chef_server_1",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefServersTag,
							Values: []string{"chef_server_1"},
						},
					},
				},
			},
		},
		{
			description: "Chef Server: Single rule differing case non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					SourceFqdn: "Chef_server_1",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefServersTag,
							Values: []string{"chef_server_1"},
						},
					},
				},
			},
		},
		{
			description: "Chef Server: Single rule non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					SourceFqdn: "chef_server_2",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefServersTag,
							Values: []string{"chef_server_1"},
						},
					},
				},
			},
		},
		{
			description: "Chef Server: two condition values on the same field one matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					SourceFqdn: "chef_server_1",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefServersTag,
							Values: []string{"chef_server_1", "chef_server_2"},
						},
					},
				},
			},
		},

		// Role
		{
			description: "Role: Single rule single condition matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Roles: []string{"area_51"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.RolesTag,
							Values: []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Role: Single value in rule condition, multiple node roles; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Roles: []string{"area_51", "vandenberg", "hunter army airfield"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.RolesTag,
							Values: []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Role: Two rules with one condition with one matching, multiple node roles; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Roles: []string{"area_51", "vandenberg", "hunter army airfield"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.RolesTag,
							Values: []string{"area_51"},
						},
					},
				},
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.RolesTag,
							Values: []string{"area_54"},
						},
					},
				},
			},
		},
		{
			description: "Role: One rule with one condition with two values with only one matching, multiple node roles; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Roles: []string{"area_51", "vandenberg", "hunter army airfield"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.RolesTag,
							Values: []string{"area_51", "area_54"},
						},
					},
				},
			},
		},
		{
			description: "Role: One rule with two conditions with only one matching, multiple node roles; non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Roles: []string{"area_51", "vandenberg", "hunter army airfield"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.RolesTag,
							Values: []string{"area_51"},
						},
						&iam_v2.Condition{
							Type:   rules_tags.RolesTag,
							Values: []string{"area_54"},
						},
					},
				},
			},
		},
		{
			description: "Role: Single rule differing case non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Roles: []string{"AREA_51"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.RolesTag,
							Values: []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Role: Single rule non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Roles: []string{"area_52"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.RolesTag,
							Values: []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Role: single rule, two values on the same condition; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Roles: []string{"area_52"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.RolesTag,
							Values: []string{"area_51", "area_52"},
						},
					},
				},
			},
		},
		{
			description: "Role: two rules with matching conditions; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Roles:            []string{"area_52"},
					OrganizationName: "org_1",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.RolesTag,
							Values: []string{"area_52"},
						},
					},
				},
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Role: two rules with only one having a matching condition; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Roles:            []string{"area_52"},
					OrganizationName: "org_2",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.RolesTag,
							Values: []string{"area_52"},
						},
					},
				},
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Role: one rule with only one matching condition; non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					Roles:            []string{"area_52", "area_49"},
					OrganizationName: "org_2",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.RolesTag,
							Values: []string{"area_52"},
						},
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},

		// Chef tag
		{
			description: "Chef Tags: Single rule one condition; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					ChefTags: []string{"area_51"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefTagsTag,
							Values: []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tags: Single rule multiple node chef tags matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					ChefTags: []string{"area_51", "vandenberg", "hunter army airfield"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefTagsTag,
							Values: []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tags: Single rule differing case non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					ChefTags: []string{"AREA_51"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefTagsTag,
							Values: []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tags: Single rule non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					ChefTags: []string{"area_52"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefTagsTag,
							Values: []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tags: two values on same conditions's field; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					ChefTags: []string{"area_52"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefTagsTag,
							Values: []string{"area_51", "area_52"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tag: two values on same condition, multiple chef tag; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					ChefTags: []string{"area_52", "vandenberg", "hunter army airfield"},
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefTagsTag,
							Values: []string{"area_51", "area_52"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tag: two rules on different fields; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					ChefTags:         []string{"area_52"},
					OrganizationName: "org_1",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefTagsTag,
							Values: []string{"area_52"},
						},
					},
				},
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Chef Tag: two rules with only one having conditions that match; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					ChefTags:         []string{"area_52"},
					OrganizationName: "org_2",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefTagsTag,
							Values: []string{"area_52"},
						},
					},
				},
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},

		// Policy Group
		{
			description: "Policy Group: Single rule matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					PolicyGroup: "PolicyGroup",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.PolicyGroupTag,
							Values: []string{"PolicyGroup"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: Single rule differing case non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					PolicyGroup: "PolicyGroup",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.PolicyGroupTag,
							Values: []string{"policygroup"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: Single rule non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					PolicyGroup: "area_52",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.PolicyGroupTag,
							Values: []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: Single rule with a condition with two values on same field; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					PolicyGroup: "area_52",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.PolicyGroupTag,
							Values: []string{"area_51", "area_52"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: two rules with two having matching conditions on different fields; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					PolicyGroup:      "area_52",
					OrganizationName: "org_1",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.PolicyGroupTag,
							Values: []string{"area_52"},
						},
					},
				},
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: two rules on different fields with only one matching; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					PolicyGroup:      "area_52",
					OrganizationName: "org_2",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.PolicyGroupTag,
							Values: []string{"area_52"},
						},
					},
				},
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},

		// Policy Name
		{
			description: "😒Policy Name: Single rule matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					PolicyName: "PolicyName",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.PolicyNameTag,
							Values: []string{"PolicyName"},
						},
					},
				},
			},
		},
		{
			description: "Policy Name: Single rule differing case non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					PolicyName: "PolicyGroup",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.PolicyNameTag,
							Values: []string{"policygroup"},
						},
					},
				},
			},
		},
		{
			description: "Policy Name: Single rule non-matching",
			matching:    false,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					PolicyName: "area_52",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.PolicyNameTag,
							Values: []string{"area_51"},
						},
					},
				},
			},
		},
		{
			description: "Policy Name: Single rule with two values on a condition; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					PolicyName: "area_52",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.PolicyNameTag,
							Values: []string{"area_51", "area_52"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: two rules both matching on different fields; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					PolicyName:       "area_52",
					OrganizationName: "org_1",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.PolicyNameTag,
							Values: []string{"area_52"},
						},
					},
				},
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_1"},
						},
					},
				},
			},
		},
		{
			description: "Policy Group: two rules with only one matching; matching",
			matching:    true,
			node: backend.Node{
				NodeInfo: backend.NodeInfo{
					PolicyName:       "area_52",
					OrganizationName: "org_2",
				},
			},
			rules: []*iam_v2.ProjectRule{
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.PolicyNameTag,
							Values: []string{"area_52"},
						},
					},
				},
				&iam_v2.ProjectRule{
					Conditions: []*iam_v2.Condition{
						&iam_v2.Condition{
							Type:   rules_tags.ChefOrgsTag,
							Values: []string{"org_1"},
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
				projectMatch := nodeMatchesRules(test.node, test.rules)

				assert.Equal(t, test.matching, projectMatch, test.description)
			})
	}
}

func TestBundlerSingleMessage(t *testing.T) {
	inbox := make(chan message.ChefRun, 100)
	listProjectRulesCount := 0
	authzClient := iam_v2.NewMockProjectsClient(gomock.NewController(t))
	authzClient.EXPECT().ListProjectRules(gomock.Any(), gomock.Any()).DoAndReturn(
		func(ctx interface{}, in interface{}) (*iam_v2.ProjectCollectionRulesResp, error) {
			listProjectRulesCount++
			return &iam_v2.ProjectCollectionRulesResp{}, nil
		})
	errc := make(chan error)

	inbox <- message.NewChefRun(context.Background(), &chef.Run{}, errc)
	close(inbox)
	out := runBundleProjectTagger(inbox, authzClient)

	<-out

	assert.Equal(t, 1, listProjectRulesCount)
}

// When 5 messages are in the inbox the ListProjectRules function is only called once.
func TestBundler5Messages(t *testing.T) {
	inbox := make(chan message.ChefRun, 100)
	listProjectRulesCount := 0
	authzClient := iam_v2.NewMockProjectsClient(gomock.NewController(t))
	authzClient.EXPECT().ListProjectRules(gomock.Any(), gomock.Any()).DoAndReturn(
		func(ctx interface{}, in interface{}) (*iam_v2.ProjectCollectionRulesResp, error) {
			listProjectRulesCount++
			return &iam_v2.ProjectCollectionRulesResp{}, nil
		})
	errc := make(chan error)

	inbox <- message.NewChefRun(context.Background(), &chef.Run{}, errc)
	inbox <- message.NewChefRun(context.Background(), &chef.Run{}, errc)
	inbox <- message.NewChefRun(context.Background(), &chef.Run{}, errc)
	inbox <- message.NewChefRun(context.Background(), &chef.Run{}, errc)
	inbox <- message.NewChefRun(context.Background(), &chef.Run{}, errc)
	close(inbox)

	out := runBundleProjectTagger(inbox, authzClient)

	<-out
	<-out
	<-out
	<-out
	<-out

	assert.Equal(t, 1, listProjectRulesCount)
}

// A simple run through of the bundle project tagger processor.
// Two messages are sent through with only one matching a project.
func TestBundlerMatchProjectRule(t *testing.T) {
	inbox := make(chan message.ChefRun, 100)
	testProjectName := "Test"
	orgName := "org_1"
	projectRules := map[string]*iam_v2.ProjectRules{}
	projectRules[testProjectName] = &iam_v2.ProjectRules{
		Rules: []*iam_v2.ProjectRule{
			&iam_v2.ProjectRule{
				Conditions: []*iam_v2.Condition{
					&iam_v2.Condition{
						Type:   rules_tags.ChefOrgsTag,
						Values: []string{orgName},
					},
				},
			},
		},
	}
	authzClient := iam_v2.NewMockProjectsClient(gomock.NewController(t))
	authzClient.EXPECT().ListProjectRules(gomock.Any(), gomock.Any()).Return(
		&iam_v2.ProjectCollectionRulesResp{ProjectRules: projectRules}, nil)
	errc := make(chan error)

	chefRun1 := message.NewChefRun(context.Background(), &chef.Run{}, errc)
	chefRun1.Node.OrganizationName = orgName

	chefRun2 := message.NewChefRun(context.Background(), &chef.Run{}, errc)
	chefRun2.Node.OrganizationName = "no_match"

	inbox <- chefRun1
	inbox <- chefRun2
	close(inbox)

	out := BuildRunProjectTagger(authzClient)(inbox)

	processMsg1 := <-out
	assert.Equal(t, []string{testProjectName}, processMsg1.Node.Projects)

	processMsg2 := <-out
	assert.Equal(t, []string{}, processMsg2.Node.Projects)
}
