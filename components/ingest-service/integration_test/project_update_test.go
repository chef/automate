package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	automate_event_type "github.com/chef/automate/components/event-service/server"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	project_update_tags "github.com/chef/automate/lib/authz"
	rules_tags "github.com/chef/automate/lib/authz"
	"github.com/golang/mock/gomock"
	"github.com/golang/protobuf/ptypes"
	_struct "github.com/golang/protobuf/ptypes/struct"
	"github.com/stretchr/testify/assert"
)

func TestProjectUpdatePainlessElasticsearchScript(t *testing.T) {
	var (
		ctx = context.Background()
	)

	cases := []struct {
		description string
		node        iBackend.Node
		projects    map[string]*iam_v2.ProjectRules
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefEnvironmentsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefEnvironmentsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefEnvironmentsTag,
									Values: []string{"env1"},
								},
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefEnvironmentsTag,
									Values: []string{"env1"},
								},
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
									Values: []string{"org1"},
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefEnvironmentsTag,
									Values: []string{"env2"},
								},
							},
						},
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefEnvironmentsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefEnvironmentsTag,
									Values: []string{"env2"},
								},
							},
						},
					},
				},
				"project3": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefEnvironmentsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
									Values: []string{"org2"},
								},
							},
						},
					},
				},
				"project3": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefEnvironmentsTag,
									Values: []string{"env1"},
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"old_tag": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
									Values: []string{"org1"},
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefServersTag,
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
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					SourceFqdn: "chef-server2.org",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefServersTag,
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
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					SourceFqdn: "Chef-server.org",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefServersTag,
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
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					SourceFqdn: "chef-server.org",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefServersTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefServersTag,
									Values: []string{"chef-server.org"},
								},
							},
						},
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefServersTag,
									Values: []string{"chef-server2.org"},
								},
							},
						},
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
									Values: []string{"org1"},
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.RolesTag,
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
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					Roles:      []string{"Area_51"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
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
									Type:   rules_tags.ChefOrgsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
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
									Type:   rules_tags.ChefOrgsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.RolesTag,
									Values: []string{"area_51"},
								},
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefTagsTag,
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
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					ChefTags:   []string{"Area_51"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefTagsTag,
									Values: []string{"area_51"},
								},
							},
						},
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefTagsTag,
									Values: []string{"area_51"},
								},
							},
						},
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefTagsTag,
									Values: []string{"area_51"},
								},
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
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
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					ChefTags:   []string{"area_51", "area_52", "area_53"},
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefTagsTag,
									Values: []string{"area_54"},
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.PolicyGroupTag,
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
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:  newUUID(),
					NodeName:    "node_1",
					PolicyGroup: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.PolicyGroupTag,
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
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:  newUUID(),
					NodeName:    "node_1",
					PolicyGroup: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.PolicyGroupTag,
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
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:  newUUID(),
					NodeName:    "node_1",
					PolicyGroup: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.PolicyGroupTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.PolicyGroupTag,
									Values: []string{"prod"},
								},
							},
						},
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.PolicyGroupTag,
									Values: []string{"dev"},
								},
							},
						},
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
									Values: []string{"org1"},
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.PolicyNameTag,
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
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					PolicyName: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.PolicyNameTag,
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
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					PolicyName: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.PolicyNameTag,
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
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
					NodeName:   "node_1",
					PolicyName: "prod",
				},
				Projects: []string{"old_tag"},
				Exists:   true,
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.PolicyNameTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.PolicyNameTag,
									Values: []string{"prod"},
								},
							},
						},
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
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
			projects: map[string]*iam_v2.ProjectRules{
				"project9": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.PolicyNameTag,
									Values: []string{"dev"},
								},
							},
						},
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{
								&iam_v2.Condition{
									Type:   rules_tags.ChefOrgsTag,
									Values: []string{"org1"},
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
			projects: map[string]*iam_v2.ProjectRules{
				"old_tag": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{},
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
			projects: map[string]*iam_v2.ProjectRules{
				"old_tag": &iam_v2.ProjectRules{
					Rules: []*iam_v2.ProjectRule{
						&iam_v2.ProjectRule{
							Conditions: []*iam_v2.Condition{},
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
						assert.FailNow(t, "testing job complete")
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

func TestErrorWhenProjectUpdateIDNotSent(t *testing.T) {
	event := &automate_event.EventMsg{
		EventID:   "lskdjflsdkfj",
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdate},
		Published: ptypes.TimestampNow(),
	}

	_, err := suite.EventHandlerServer.HandleEvent(context.Background(), event)
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
				project_update_tags.ProjectUpdateIDTag: &_struct.Value{
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
	localSuite.eventServiceClientMock.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().DoAndReturn(
		func(ctx context.Context, in *automate_event.PublishRequest) (*automate_event.PublishResponse, error) {
			lastEventSent = in.Msg
			return &automate_event.PublishResponse{}, nil
		})
	localSuite.projectsClient.EXPECT().ListProjectRules(gomock.Any(), gomock.Any()).AnyTimes().Return(
		&iam_v2.ProjectCollectionRulesResp{}, nil)

	event := &automate_event.EventMsg{
		EventID:   "lskdjflsdkfj",
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdate},
		Published: ptypes.TimestampNow(),
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: &_struct.Value{
					Kind: &_struct.Value_StringValue{
						StringValue: "TestNoErrorWhenProjectUpdateIDIsSent",
					},
				},
			},
		},
	}

	_, err := localSuite.EventHandlerServer.HandleEvent(context.Background(), event)
	assert.NoError(t, err)

	// Wait for job to complete
	for {
		time.Sleep(time.Millisecond * 100)
		assert.Equal(t, lastEventSent.Type.Name, automate_event_type.ProjectRulesUpdateStatus)
		if lastEventSent.Type.Name != automate_event_type.ProjectRulesUpdateStatus {
			assert.FailNow(t, "")
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
	localSuite.eventServiceClientMock.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().DoAndReturn(
		func(ctx context.Context, in *automate_event.PublishRequest) (*automate_event.PublishResponse, error) {
			lastEventSent = in.Msg
			return &automate_event.PublishResponse{}, nil
		})
	localSuite.projectsClient.EXPECT().ListProjectRules(gomock.Any(), gomock.Any()).AnyTimes().Return(
		&iam_v2.ProjectCollectionRulesResp{}, nil)
	event1 := &automate_event.EventMsg{
		EventID:   "1",
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdate},
		Published: ptypes.TimestampNow(),
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: &_struct.Value{
					Kind: &_struct.Value_StringValue{
						StringValue: "one",
					},
				},
			},
		},
	}

	_, err := localSuite.EventHandlerServer.HandleEvent(context.Background(), event1)
	assert.NoError(t, err)

	assert.True(t, lastEventSent == nil ||
		lastEventSent.Type.Name == automate_event_type.ProjectRulesUpdateStatus)

	event2 := &automate_event.EventMsg{
		EventID:   "2",
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdate},
		Published: ptypes.TimestampNow(),
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: &_struct.Value{
					Kind: &_struct.Value_StringValue{
						StringValue: "two",
					},
				},
			},
		},
	}

	_, err = localSuite.EventHandlerServer.HandleEvent(context.Background(), event2)
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
