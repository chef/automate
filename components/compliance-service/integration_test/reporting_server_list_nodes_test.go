package integration_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"golang.org/x/net/context"

	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

func TestListNodesFiltering(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	ctx := context.Background()

	cases := []struct {
		description string
		reports     []*relaxting.ESInSpecReport
		query       reporting.Query
		expectedIds []string
	}{
		// organization
		{
			description: "Filter out one of the nodes by 'organization'",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:           "1",
					OrganizationName: "org1",
				},
				{
					NodeID:           "2",
					OrganizationName: "org2",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "organization",
						Values: []string{"org1"},
					},
				},
			},
			expectedIds: []string{"1"},
		},
		{
			description: "Filter out all of the nodes by 'organization'",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:           "1",
					OrganizationName: "org1",
				},
				{
					NodeID:           "2",
					OrganizationName: "org2",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "organization",
						Values: []string{"org3"},
					},
				},
			},
			expectedIds: []string{},
		},

		// chef_server
		{
			description: "Filter out one of the nodes by chef servers",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					SourceFQDN: "org1",
				},
				{
					NodeID:     "2",
					SourceFQDN: "org2",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "chef_server",
						Values: []string{"org1"},
					},
				},
			},
			expectedIds: []string{"1"},
		},
		{
			description: "Filter out all of the nodes by chef servers",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					SourceFQDN: "org1",
				},
				{
					NodeID:     "2",
					SourceFQDN: "org2",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "chef_server",
						Values: []string{"org3"},
					},
				},
			},
			expectedIds: []string{},
		},

		// chef tags
		{
			description: "Filter out one of the nodes with chef tags",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					ChefTags: []string{"org1"},
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "chef_tags",
						Values: []string{"org1"},
					},
				},
			},
			expectedIds: []string{"1"},
		},
		{
			description: "Filter out one of the nodes with multiple tags use chef tags",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					ChefTags: []string{"org1", "org3"},
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "chef_tags",
						Values: []string{"org1"},
					},
				},
			},
			expectedIds: []string{"1"},
		},
		{
			description: "Filter out all of the nodes by chef tags",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					ChefTags: []string{"org1"},
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2", "org4"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "chef_tags",
						Values: []string{"org3"},
					},
				},
			},
			expectedIds: []string{},
		},

		// policy_group
		{
			description: "Filter out one of the nodes with the policy group",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					PolicyGroup: "org1",
				},
				{
					NodeID:      "2",
					PolicyGroup: "org2",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "policy_group",
						Values: []string{"org1"},
					},
				},
			},
			expectedIds: []string{"1"},
		},
		{
			description: "Filter out all of the nodes by policy group",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					PolicyGroup: "org1",
				},
				{
					NodeID:      "2",
					PolicyGroup: "org2",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "policy_group",
						Values: []string{"org3"},
					},
				},
			},
			expectedIds: []string{},
		},

		// policy_name
		{
			description: "Filter out one of the nodes with the policy name",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					PolicyName: "org1",
				},
				{
					NodeID:     "2",
					PolicyName: "org2",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "policy_name",
						Values: []string{"org1"},
					},
				},
			},
			expectedIds: []string{"1"},
		},
		{
			description: "Filter out all of the nodes by policy name",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					PolicyName: "org1",
				},
				{
					NodeID:     "2",
					PolicyName: "org2",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "policy_name",
						Values: []string{"org3"},
					},
				},
			},
			expectedIds: []string{},
		},
	}

	for _, test := range cases {
		t.Run(test.description, func(t *testing.T) {
			_, err := suite.InsertInspecReports(test.reports)
			require.NoError(t, err)
			defer suite.DeleteAllDocuments()

			response, err := server.ListNodes(ctx, &test.query)
			require.NoError(t, err)
			require.NotNil(t, response)

			actualIds := make([]string, len(response.Nodes))
			for i, node := range response.Nodes {
				actualIds[i] = node.Id
			}

			assert.ElementsMatch(t, test.expectedIds, actualIds)
		})
	}
}

func TestListNodesWildcardFiltering(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	ctx := context.Background()

	cases := []struct {
		description string
		reports     []*relaxting.ESInSpecReport
		query       reporting.Query
		expectedIds []string
	}{
		// chef server
		{
			description: "chef server: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					SourceFQDN: "a2-prod",
				},
				{
					NodeID:     "2",
					SourceFQDN: "a2-dev",
				},
				{
					NodeID:     "3",
					SourceFQDN: "a1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "chef_server",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "chef server: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "2",
					SourceFQDN: "a2-dev",
				},
				{
					NodeID:     "3",
					SourceFQDN: "a1-dev",
				},
				{
					NodeID:     "4",
					SourceFQDN: "b1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "chef_server",
						Values: []string{"a?-dev"},
					},
				},
			},
			expectedIds: []string{"3", "2"},
		},
		{
			description: "chef server: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					SourceFQDN: "A2-prod",
				},
				{
					NodeID:     "2",
					SourceFQDN: "a2-Dev",
				},
				{
					NodeID:     "3",
					SourceFQDN: "A1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "chef_server",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "chef_server: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					SourceFQDN: "A2-prod",
				},
				{
					NodeID:     "2",
					SourceFQDN: "a2-Dev",
				},
				{
					NodeID:     "3",
					SourceFQDN: "A1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "chef_server",
						Values: []string{"A2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},

		// chef tags
		{
			description: "chef tags: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					ChefTags: []string{"a2-prod"},
				},
				{
					NodeID:   "2",
					ChefTags: []string{"a2-dev", "chef-server"},
				},
				{
					NodeID:   "3",
					ChefTags: []string{"a1-dev"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "chef_tags",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "chef tags: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "2",
					ChefTags: []string{"a2-dev"},
				},
				{
					NodeID:   "3",
					ChefTags: []string{"a1-dev", "chef-server"},
				},
				{
					NodeID:   "4",
					ChefTags: []string{"b1-dev"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "chef_tags",
						Values: []string{"a?-dev"},
					},
				},
			},
			expectedIds: []string{"3", "2"},
		},
		{
			description: "chef_tags: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					ChefTags: []string{"A2-prod"},
				},
				{
					NodeID:   "2",
					ChefTags: []string{"a2-Dev"},
				},
				{
					NodeID:   "3",
					ChefTags: []string{"A1-dev"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "chef_tags",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "chef_tags: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					ChefTags: []string{"A2-prod"},
				},
				{
					NodeID:   "2",
					ChefTags: []string{"a2-Dev"},
				},
				{
					NodeID:   "3",
					ChefTags: []string{"A1-dev"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "chef_tags",
						Values: []string{"A2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},

		// environment
		{
			description: "environment: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					Environment: "a2-prod",
				},
				{
					NodeID:      "2",
					Environment: "a2-dev",
				},
				{
					NodeID:      "3",
					Environment: "a1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "environment",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "environment: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "2",
					Environment: "a2-dev",
				},
				{
					NodeID:      "3",
					Environment: "a1-dev",
				},
				{
					NodeID:      "4",
					Environment: "b1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "environment",
						Values: []string{"a?-dev"},
					},
				},
			},
			expectedIds: []string{"3", "2"},
		},
		{
			description: "environment: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					Environment: "A2-prod",
				},
				{
					NodeID:      "2",
					Environment: "a2-Dev",
				},
				{
					NodeID:      "3",
					Environment: "A1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "environment",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "environment: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					Environment: "A2-prod",
				},
				{
					NodeID:      "2",
					Environment: "a2-Dev",
				},
				{
					NodeID:      "3",
					Environment: "A1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "environment",
						Values: []string{"A2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},

		// node
		{
			description: "node: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					NodeName: "a2-prod",
				},
				{
					NodeID:   "2",
					NodeName: "a2-dev",
				},
				{
					NodeID:   "3",
					NodeName: "a1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "node_name",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "node: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "2",
					NodeName: "a2-dev",
				},
				{
					NodeID:   "3",
					NodeName: "a1-dev",
				},
				{
					NodeID:   "4",
					NodeName: "b1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "node_name",
						Values: []string{"a?-dev"},
					},
				},
			},
			expectedIds: []string{"3", "2"},
		},
		{
			description: "node: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					NodeName: "A2-prod",
				},
				{
					NodeID:   "2",
					NodeName: "a2-Dev",
				},
				{
					NodeID:   "3",
					NodeName: "A1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "node_name",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "node: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					NodeName: "A2-prod",
				},
				{
					NodeID:   "2",
					NodeName: "a2-Dev",
				},
				{
					NodeID:   "3",
					NodeName: "A1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "node_name",
						Values: []string{"A2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},

		// organization
		{
			description: "organization: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:           "1",
					OrganizationName: "a2-prod",
				},
				{
					NodeID:           "2",
					OrganizationName: "a2-dev",
				},
				{
					NodeID:           "3",
					OrganizationName: "a1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "organization",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "organization: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:           "2",
					OrganizationName: "a2-dev",
				},
				{
					NodeID:           "3",
					OrganizationName: "a1-dev",
				},
				{
					NodeID:           "4",
					OrganizationName: "b1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "organization",
						Values: []string{"a?-dev"},
					},
				},
			},
			expectedIds: []string{"3", "2"},
		},
		{
			description: "organization: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:           "1",
					OrganizationName: "A2-prod",
				},
				{
					NodeID:           "2",
					OrganizationName: "a2-Dev",
				},
				{
					NodeID:           "3",
					OrganizationName: "A1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "organization",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "organization: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:           "1",
					OrganizationName: "A2-prod",
				},
				{
					NodeID:           "2",
					OrganizationName: "a2-Dev",
				},
				{
					NodeID:           "3",
					OrganizationName: "A1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "organization",
						Values: []string{"A2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},

		// platform
		{
			description: "platform: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "a2-prod",
					},
				},
				{
					NodeID: "2",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "a2-dev",
					},
				},
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "a1-dev",
					},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "platform",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "platform: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "2",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "a2-dev",
					},
				},
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "a1-dev",
					},
				},
				{
					NodeID: "4",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "b1-dev",
					},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "platform",
						Values: []string{"a?-dev"},
					},
				},
			},
			expectedIds: []string{"3", "2"},
		},
		{
			description: "platform: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "A2-prod",
					},
				},
				{
					NodeID: "2",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "a2-Dev",
					},
				},
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "A1-dev",
					},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "platform",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "platform: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "A2-prod",
					},
				},
				{
					NodeID: "2",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "a2-Dev",
					},
				},
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "A1-dev",
					},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "platform",
						Values: []string{"A2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},

		// Policy group
		{
			description: "policy_group: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					PolicyGroup: "a2-prod",
				},
				{
					NodeID:      "2",
					PolicyGroup: "a2-dev",
				},
				{
					NodeID:      "3",
					PolicyGroup: "a1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "policy_group",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "policy_group: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "2",
					PolicyGroup: "a2-dev",
				},
				{
					NodeID:      "3",
					PolicyGroup: "a1-dev",
				},
				{
					NodeID:      "4",
					PolicyGroup: "b1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "policy_group",
						Values: []string{"a?-dev"},
					},
				},
			},
			expectedIds: []string{"3", "2"},
		},
		{
			description: "policy_group: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					PolicyGroup: "A2-prod",
				},
				{
					NodeID:      "2",
					PolicyGroup: "a2-Dev",
				},
				{
					NodeID:      "3",
					PolicyGroup: "A1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "policy_group",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "policy_group: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					PolicyGroup: "A2-prod",
				},
				{
					NodeID:      "2",
					PolicyGroup: "a2-Dev",
				},
				{
					NodeID:      "3",
					PolicyGroup: "A1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "policy_group",
						Values: []string{"A2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},

		// Policy Name
		{
			description: "policy_name: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					PolicyName: "a2-prod",
				},
				{
					NodeID:     "2",
					PolicyName: "a2-dev",
				},
				{
					NodeID:     "3",
					PolicyName: "a1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "policy_name",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "policy_name: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "2",
					PolicyName: "a2-dev",
				},
				{
					NodeID:     "3",
					PolicyName: "a1-dev",
				},
				{
					NodeID:     "4",
					PolicyName: "b1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "policy_name",
						Values: []string{"a?-dev"},
					},
				},
			},
			expectedIds: []string{"3", "2"},
		},
		{
			description: "policy_name: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					PolicyName: "A2-prod",
				},
				{
					NodeID:     "2",
					PolicyName: "a2-Dev",
				},
				{
					NodeID:     "3",
					PolicyName: "A1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "policy_name",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "policy_name: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					PolicyName: "A2-prod",
				},
				{
					NodeID:     "2",
					PolicyName: "a2-Dev",
				},
				{
					NodeID:     "3",
					PolicyName: "A1-dev",
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "policy_name",
						Values: []string{"A2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},

		// Recipe
		{
			description: "recipe: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:  "1",
					Recipes: []string{"a2-prod"},
				},
				{
					NodeID:  "2",
					Recipes: []string{"a2-dev", "chef-server"},
				},
				{
					NodeID:  "3",
					Recipes: []string{"a1-dev"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "recipe",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "recipe: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:  "2",
					Recipes: []string{"a2-dev"},
				},
				{
					NodeID:  "3",
					Recipes: []string{"a1-dev", "chef-server"},
				},
				{
					NodeID:  "4",
					Recipes: []string{"b1-dev"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "recipe",
						Values: []string{"a?-dev"},
					},
				},
			},
			expectedIds: []string{"3", "2"},
		},
		{
			description: "recipe: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:  "1",
					Recipes: []string{"A2-prod"},
				},
				{
					NodeID:  "2",
					Recipes: []string{"a2-Dev"},
				},
				{
					NodeID:  "3",
					Recipes: []string{"A1-dev"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "recipe",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "recipe: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:  "1",
					Recipes: []string{"A2-prod"},
				},
				{
					NodeID:  "2",
					Recipes: []string{"a2-Dev"},
				},
				{
					NodeID:  "3",
					Recipes: []string{"A1-dev"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "recipe",
						Values: []string{"A2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},

		// Roles
		{
			description: "roles: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Roles:  []string{"a2-prod"},
				},
				{
					NodeID: "2",
					Roles:  []string{"a2-dev", "chef-server"},
				},
				{
					NodeID: "3",
					Roles:  []string{"a1-dev"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "role",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "roles: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "2",
					Roles:  []string{"a2-dev"},
				},
				{
					NodeID: "3",
					Roles:  []string{"a1-dev", "chef-server"},
				},
				{
					NodeID: "4",
					Roles:  []string{"b1-dev"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "role",
						Values: []string{"a?-dev"},
					},
				},
			},
			expectedIds: []string{"3", "2"},
		},
		{
			description: "role: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Roles:  []string{"A2-prod"},
				},
				{
					NodeID: "2",
					Roles:  []string{"a2-Dev"},
				},
				{
					NodeID: "3",
					Roles:  []string{"A1-dev"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "role",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "role: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Roles:  []string{"A2-prod"},
				},
				{
					NodeID: "2",
					Roles:  []string{"a2-Dev"},
				},
				{
					NodeID: "3",
					Roles:  []string{"A1-dev"},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "role",
						Values: []string{"A2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},

		// Profile
		{
			description: "Profile: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "a2-prod",
						},
					},
				},
				{
					NodeID: "2",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "a2-dev",
						},
					},
				},
				{
					NodeID: "3",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "a1-dev",
						},
					},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "profile_name",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "Profile: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "A2-prod",
						},
					},
				},
				{
					NodeID: "2",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "A2-dev",
						},
					},
				},
				{
					NodeID: "3",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "a1-dev",
						},
					},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "profile_name",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "Profile: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "A2-prod",
						},
					},
				},
				{
					NodeID: "2",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "a2-dev",
						},
					},
				},
				{
					NodeID: "3",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "a1-dev",
						},
					},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "profile_name",
						Values: []string{"A2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},

		// control
		{
			description: "control: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-prod",
								},
							},
						},
					},
				},
				{
					NodeID: "2",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-dev",
								},
							},
						},
					},
				},
				{
					NodeID: "3",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a1-dev",
								},
							},
						},
					},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "control_name",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "control: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "A2-prod",
								},
							},
						},
					},
				},
				{
					NodeID: "2",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "A2-dev",
								},
							},
						},
					},
				},
				{
					NodeID: "3",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a1-dev",
								},
							},
						},
					},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "control_name",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "control: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-prod",
								},
							},
						},
					},
				},
				{
					NodeID: "2",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "A2-dev",
								},
							},
						},
					},
				},
				{
					NodeID: "3",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a1-dev",
								},
							},
						},
					},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "control_name",
						Values: []string{"A2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "control: '*' wildcard one node with two matching controls",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-prod",
								},
								{
									Title: "a2-test",
								},
							},
						},
					},
				},
				{
					NodeID: "2",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-dev",
								},
							},
						},
					},
				},
				{
					NodeID: "3",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a1-dev",
								},
							},
						},
					},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "control_name",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "control: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "2",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-dev",
								},
							},
						},
					},
				},
				{
					NodeID: "3",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a1-dev",
								},
							},
						},
					},
				},
				{
					NodeID: "4",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "b1-dev",
								},
							},
						},
					},
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "control_name",
						Values: []string{"a?-dev"},
					},
				},
			},
			expectedIds: []string{"3", "2"},
		},
	}

	for _, test := range cases {
		t.Run(test.description, func(t *testing.T) {
			_, err := suite.InsertInspecReports(test.reports)
			require.NoError(t, err)
			defer suite.DeleteAllDocuments()

			response, err := server.ListNodes(ctx, &test.query)
			require.NoError(t, err)
			require.NotNil(t, response)

			actualIds := make([]string, len(response.Nodes))
			for i, node := range response.Nodes {
				actualIds[i] = node.Id
			}

			assert.ElementsMatch(t, test.expectedIds, actualIds)
		})
	}
}

func TestListNodesProjectFiltering(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	nodeIds := []string{newUUID(), newUUID(), newUUID(), newUUID(), newUUID(), newUUID()}
	reports := []*relaxting.ESInSpecReport{
		{
			NodeID:   nodeIds[0],
			Projects: []string{},
		},
		{
			NodeID:   nodeIds[1],
			Projects: []string{"project1"},
		},
		{
			NodeID:   nodeIds[2],
			Projects: []string{"project1", "project2"},
		},
		{
			NodeID:   nodeIds[3],
			Projects: []string{"project2"},
		},
		{
			NodeID:   nodeIds[4],
			Projects: []string{"project2", "project3"},
		},
		{
			NodeID:   nodeIds[5],
			Projects: []string{"project3"},
		},
	}

	reportIds, err := suite.InsertInspecReports(reports)
	require.NoError(t, err)

	defer suite.DeleteAllDocuments()

	require.Len(t, reportIds, len(reports))

	successCases := []struct {
		description     string
		allowedProjects []string
		expectedIds     []string
	}{
		{
			description:     "Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			expectedIds:     nodeIds,
		},
		{
			description:     "Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},
			expectedIds:     nodeIds[1:3],
		},
		{
			description:     "Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},
			expectedIds:     nodeIds[1:5],
		},
		{
			description:     "Projects: user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},
			expectedIds:     []string{},
		},
		{
			description:     "Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},
			expectedIds:     nodeIds[:3],
		},
		{
			description:     "Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},
			expectedIds:     nodeIds[:5],
		},
		{
			description:     "Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},
			expectedIds:     nodeIds[:1],
		},
		{
			description:     "Projects: user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			expectedIds:     nodeIds[:1],
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			response, err := server.ListNodes(ctx, &reporting.Query{})

			assert.NoError(t, err)
			require.NotNil(t, response)

			actualIds := make([]string, len(response.Nodes))
			for i, report := range response.Nodes {
				actualIds[i] = report.Id
			}

			assert.ElementsMatch(t, test.expectedIds, actualIds)
		})
	}
}
