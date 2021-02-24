package integration_test

import (
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/interservice/compliance/reporting"
	authzConstants "github.com/chef/automate/components/authz-service/constants"
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
			description: "reporting_server_list_nodes_test.go => Filter out one of the nodes by 'organization'",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:           "1",
					OrganizationName: "org1",
					EndTime:          time.Now(),
				},
				{
					NodeID:           "2",
					OrganizationName: "org2",
					EndTime:          time.Now(),
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
			description: "reporting_server_list_nodes_test.go => Filter out all of the nodes by 'organization'",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:           "1",
					OrganizationName: "org1",
					EndTime:          time.Now(),
				},
				{
					NodeID:           "2",
					OrganizationName: "org2",
					EndTime:          time.Now(),
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
			description: "reporting_server_list_nodes_test.go => Filter out one of the nodes by chef servers",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					SourceFQDN: "org1",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "2",
					SourceFQDN: "org2",
					EndTime:    time.Now(),
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
			description: "reporting_server_list_nodes_test.go => Filter out all of the nodes by chef servers",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					SourceFQDN: "org1",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "2",
					SourceFQDN: "org2",
					EndTime:    time.Now(),
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

		// inspec_version
		{
			description: "reporting_server_list_nodes_test.go => Filter out one of the nodes by chef servers",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:        "1",
					InSpecVersion: "3.1.0",
					EndTime:       time.Now(),
				},
				{
					NodeID:        "2",
					InSpecVersion: "3.1.3",
					EndTime:       time.Now(),
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "inspec_version",
						Values: []string{"3.1.0"},
					},
				},
			},
			expectedIds: []string{"1"},
		},
		{
			description: "reporting_server_list_nodes_test.go => Filter out all of the nodes by chef servers",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:        "1",
					InSpecVersion: "3.1.0",
					EndTime:       time.Now(),
				},
				{
					NodeID:        "2",
					InSpecVersion: "3.1.3",
					EndTime:       time.Now(),
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "inspec_version",
						Values: []string{"3.1.4"},
					},
				},
			},
			expectedIds: []string{},
		},

		// chef tags
		{
			description: "reporting_server_list_nodes_test.go => Filter out one of the nodes with chef tags",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					ChefTags: []string{"org1"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2"},
					EndTime:  time.Now(),
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
			description: "reporting_server_list_nodes_test.go => Filter out one of the nodes with multiple tags use chef tags",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					ChefTags: []string{"org1", "org3"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2"},
					EndTime:  time.Now(),
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
			description: "reporting_server_list_nodes_test.go => Filter out all of the nodes by chef tags",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					ChefTags: []string{"org1"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2", "org4"},
					EndTime:  time.Now(),
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
			description: "reporting_server_list_nodes_test.go => Filter out one of the nodes with the policy group",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					PolicyGroup: "org1",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "2",
					PolicyGroup: "org2",
					EndTime:     time.Now(),
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
			description: "reporting_server_list_nodes_test.go => Filter out all of the nodes by policy group",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					PolicyGroup: "org1",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "2",
					PolicyGroup: "org2",
					EndTime:     time.Now(),
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
			description: "reporting_server_list_nodes_test.go => Filter out one of the nodes with the policy name",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					PolicyName: "org1",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "2",
					PolicyName: "org2",
					EndTime:    time.Now(),
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
			description: "reporting_server_list_nodes_test.go => Filter out all of the nodes by policy name",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					PolicyName: "org1",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "2",
					PolicyName: "org2",
					EndTime:    time.Now(),
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

		// platform
		{
			description: "reporting_server_list_nodes_test.go => Filter out one of the nodes with platform",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "org1",
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "2",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "org2",
					},
					EndTime: time.Now(),
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "platform",
						Values: []string{"org1"},
					},
				},
			},
			expectedIds: []string{"1"},
		},
		{
			description: "reporting_server_list_nodes_test.go => Filter out all of the nodes by platform",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "org1",
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "2",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "org2",
					},
					EndTime: time.Now(),
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "platform",
						Values: []string{"org3"},
					},
				},
			},
			expectedIds: []string{},
		},

		// platform_with_version
		{
			description: "reporting_server_list_nodes_test.go => Filter out one of the nodes with platform_with_version",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "org1",
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "2",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "org2",
					},
					EndTime: time.Now(),
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "platform_with_version",
						Values: []string{"org1"},
					},
				},
			},
			expectedIds: []string{"1"},
		},
		{
			description: "reporting_server_list_nodes_test.go => Filter out all of the nodes by platform_with_version",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "org1",
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "2",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "org2",
					},
					EndTime: time.Now(),
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "platform_with_version",
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

		// inspec_version
		{
			description: "reporting_server_list_nodes_test.go => inspec_version: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:        "1",
					InSpecVersion: "3.1.0",
					EndTime:       time.Now(),
				},
				{
					NodeID:        "2",
					InSpecVersion: "3.1.3",
					EndTime:       time.Now(),
				},
				{
					NodeID:        "3",
					InSpecVersion: "4.1.3",
					EndTime:       time.Now(),
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "inspec_version",
						Values: []string{"3.*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "reporting_server_list_nodes_test.go => inspec_version: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:        "1",
					InSpecVersion: "3.1.0",
					EndTime:       time.Now(),
				},
				{
					NodeID:        "2",
					InSpecVersion: "3.1.3",
					EndTime:       time.Now(),
				},
				{
					NodeID:        "3",
					InSpecVersion: "4.1.3",
					EndTime:       time.Now(),
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "inspec_version",
						Values: []string{"?.1.3"},
					},
				},
			},
			expectedIds: []string{"3", "2"},
		},

		// chef server
		{
			description: "reporting_server_list_nodes_test.go => chef server: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					SourceFQDN: "a2-prod",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "2",
					SourceFQDN: "a2-dev",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "3",
					SourceFQDN: "a1-dev",
					EndTime:    time.Now(),
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
			description: "reporting_server_list_nodes_test.go => chef server: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "2",
					SourceFQDN: "a2-dev",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "3",
					SourceFQDN: "a1-dev",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "4",
					SourceFQDN: "b1-dev",
					EndTime:    time.Now(),
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
			description: "reporting_server_list_nodes_test.go => chef server: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					SourceFQDN: "A2-prod",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "2",
					SourceFQDN: "a2-Dev",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "3",
					SourceFQDN: "A1-dev",
					EndTime:    time.Now(),
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
			description: "reporting_server_list_nodes_test.go => chef_server: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					SourceFQDN: "A2-prod",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "2",
					SourceFQDN: "a2-Dev",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "3",
					SourceFQDN: "A1-dev",
					EndTime:    time.Now(),
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
			description: "reporting_server_list_nodes_test.go => chef tags: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					ChefTags: []string{"a2-prod"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "2",
					ChefTags: []string{"a2-dev", "chef-server"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "3",
					ChefTags: []string{"a1-dev"},
					EndTime:  time.Now(),
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
			description: "reporting_server_list_nodes_test.go => chef tags: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "2",
					ChefTags: []string{"a2-dev"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "3",
					ChefTags: []string{"a1-dev", "chef-server"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "4",
					ChefTags: []string{"b1-dev"},
					EndTime:  time.Now(),
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
			description: "reporting_server_list_nodes_test.go => chef_tags: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					ChefTags: []string{"A2-prod"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "2",
					ChefTags: []string{"a2-Dev"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "3",
					ChefTags: []string{"A1-dev"},
					EndTime:  time.Now(),
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
			description: "reporting_server_list_nodes_test.go => chef_tags: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					ChefTags: []string{"A2-prod"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "2",
					ChefTags: []string{"a2-Dev"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "3",
					ChefTags: []string{"A1-dev"},
					EndTime:  time.Now(),
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
			description: "reporting_server_list_nodes_test.go => environment: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					Environment: "a2-prod",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "2",
					Environment: "a2-dev",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "3",
					Environment: "a1-dev",
					EndTime:     time.Now(),
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
			description: "reporting_server_list_nodes_test.go => environment: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "2",
					Environment: "a2-dev",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "3",
					Environment: "a1-dev",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "4",
					Environment: "b1-dev",
					EndTime:     time.Now(),
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
			description: "reporting_server_list_nodes_test.go => environment: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					Environment: "A2-prod",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "2",
					Environment: "a2-Dev",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "3",
					Environment: "A1-dev",
					EndTime:     time.Now(),
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
			description: "reporting_server_list_nodes_test.go => environment: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					Environment: "A2-prod",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "2",
					Environment: "a2-Dev",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "3",
					Environment: "A1-dev",
					EndTime:     time.Now(),
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
			description: "reporting_server_list_nodes_test.go => node: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					NodeName: "a2-prod",
					EndTime:  time.Now(),
				},
				{
					NodeID:   "2",
					NodeName: "a2-dev",
					EndTime:  time.Now(),
				},
				{
					NodeID:   "3",
					NodeName: "a1-dev",
					EndTime:  time.Now(),
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
			description: "reporting_server_list_nodes_test.go => node: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "2",
					NodeName: "a2-dev",
					EndTime:  time.Now(),
				},
				{
					NodeID:   "3",
					NodeName: "a1-dev",
					EndTime:  time.Now(),
				},
				{
					NodeID:   "4",
					NodeName: "b1-dev",
					EndTime:  time.Now(),
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
			description: "reporting_server_list_nodes_test.go => node: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					NodeName: "A2-prod",
					EndTime:  time.Now(),
				},
				{
					NodeID:   "2",
					NodeName: "a2-Dev",
					EndTime:  time.Now(),
				},
				{
					NodeID:   "3",
					NodeName: "A1-dev",
					EndTime:  time.Now(),
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
			description: "reporting_server_list_nodes_test.go => node: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:   "1",
					NodeName: "A2-prod",
					EndTime:  time.Now(),
				},
				{
					NodeID:   "2",
					NodeName: "a2-Dev",
					EndTime:  time.Now(),
				},
				{
					NodeID:   "3",
					NodeName: "A1-dev",
					EndTime:  time.Now(),
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
			description: "reporting_server_list_nodes_test.go => organization: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:           "1",
					OrganizationName: "a2-prod",
					EndTime:          time.Now(),
				},
				{
					NodeID:           "2",
					OrganizationName: "a2-dev",
					EndTime:          time.Now(),
				},
				{
					NodeID:           "3",
					OrganizationName: "a1-dev",
					EndTime:          time.Now(),
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
			description: "reporting_server_list_nodes_test.go => organization: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:           "2",
					OrganizationName: "a2-dev",
					EndTime:          time.Now(),
				},
				{
					NodeID:           "3",
					OrganizationName: "a1-dev",
					EndTime:          time.Now(),
				},
				{
					NodeID:           "4",
					OrganizationName: "b1-dev",
					EndTime:          time.Now(),
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
			description: "reporting_server_list_nodes_test.go => organization: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:           "1",
					OrganizationName: "A2-prod",
					EndTime:          time.Now(),
				},
				{
					NodeID:           "2",
					OrganizationName: "a2-Dev",
					EndTime:          time.Now(),
				},
				{
					NodeID:           "3",
					OrganizationName: "A1-dev",
					EndTime:          time.Now(),
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
			description: "reporting_server_list_nodes_test.go => organization: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:           "1",
					OrganizationName: "A2-prod",
					EndTime:          time.Now(),
				},
				{
					NodeID:           "2",
					OrganizationName: "a2-Dev",
					EndTime:          time.Now(),
				},
				{
					NodeID:           "3",
					OrganizationName: "A1-dev",
					EndTime:          time.Now(),
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
			description: "reporting_server_list_nodes_test.go => platform: '*' wildcard",
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => platform: '?' wildcard",
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => platform: case insensitive wildcard 1",
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => platform: case insensitive wildcard 2",
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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

		// platform_with_version
		{
			description: "reporting_server_list_nodes_test.go => platform_with_version: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "a2-prod",
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "2",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "a2-dev",
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "a1-dev",
					},
					EndTime: time.Now(),
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "platform_with_version",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "reporting_server_list_nodes_test.go => platform_with_version: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "2",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "a2-dev",
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "a1-dev",
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "4",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "b1-dev",
					},
					EndTime: time.Now(),
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "platform_with_version",
						Values: []string{"a?-dev"},
					},
				},
			},
			expectedIds: []string{"3", "2"},
		},
		{
			description: "reporting_server_list_nodes_test.go => platform_with_version: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "A2-prod",
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "2",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "a2-Dev",
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "A1-dev",
					},
					EndTime: time.Now(),
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "platform_with_version",
						Values: []string{"a2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},
		{
			description: "reporting_server_list_nodes_test.go => platform_with_version: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "A2-prod",
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "2",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "a2-Dev",
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "A1-dev",
					},
					EndTime: time.Now(),
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "platform_with_version",
						Values: []string{"A2-*"},
					},
				},
			},
			expectedIds: []string{"1", "2"},
		},

		// Policy group
		{
			description: "reporting_server_list_nodes_test.go => policy_group: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					PolicyGroup: "a2-prod",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "2",
					PolicyGroup: "a2-dev",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "3",
					PolicyGroup: "a1-dev",
					EndTime:     time.Now(),
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
			description: "reporting_server_list_nodes_test.go => policy_group: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "2",
					PolicyGroup: "a2-dev",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "3",
					PolicyGroup: "a1-dev",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "4",
					PolicyGroup: "b1-dev",
					EndTime:     time.Now(),
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
			description: "reporting_server_list_nodes_test.go => policy_group: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					PolicyGroup: "A2-prod",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "2",
					PolicyGroup: "a2-Dev",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "3",
					PolicyGroup: "A1-dev",
					EndTime:     time.Now(),
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
			description: "reporting_server_list_nodes_test.go => policy_group: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:      "1",
					PolicyGroup: "A2-prod",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "2",
					PolicyGroup: "a2-Dev",
					EndTime:     time.Now(),
				},
				{
					NodeID:      "3",
					PolicyGroup: "A1-dev",
					EndTime:     time.Now(),
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
			description: "reporting_server_list_nodes_test.go => policy_name: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					PolicyName: "a2-prod",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "2",
					PolicyName: "a2-dev",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "3",
					PolicyName: "a1-dev",
					EndTime:    time.Now(),
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
			description: "reporting_server_list_nodes_test.go => policy_name: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "2",
					PolicyName: "a2-dev",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "3",
					PolicyName: "a1-dev",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "4",
					PolicyName: "b1-dev",
					EndTime:    time.Now(),
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
			description: "reporting_server_list_nodes_test.go => policy_name: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					PolicyName: "A2-prod",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "2",
					PolicyName: "a2-Dev",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "3",
					PolicyName: "A1-dev",
					EndTime:    time.Now(),
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
			description: "reporting_server_list_nodes_test.go => policy_name: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:     "1",
					PolicyName: "A2-prod",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "2",
					PolicyName: "a2-Dev",
					EndTime:    time.Now(),
				},
				{
					NodeID:     "3",
					PolicyName: "A1-dev",
					EndTime:    time.Now(),
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
			description: "reporting_server_list_nodes_test.go => recipe: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:  "1",
					Recipes: []string{"a2-prod"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "2",
					Recipes: []string{"a2-dev", "chef-server"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "3",
					Recipes: []string{"a1-dev"},
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => recipe: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:  "2",
					Recipes: []string{"a2-dev"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "3",
					Recipes: []string{"a1-dev", "chef-server"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "4",
					Recipes: []string{"b1-dev"},
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => recipe: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:  "1",
					Recipes: []string{"A2-prod"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "2",
					Recipes: []string{"a2-Dev"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "3",
					Recipes: []string{"A1-dev"},
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => recipe: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:  "1",
					Recipes: []string{"A2-prod"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "2",
					Recipes: []string{"a2-Dev"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "3",
					Recipes: []string{"A1-dev"},
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => roles: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:  "1",
					Roles:   []string{"a2-prod"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "2",
					Roles:   []string{"a2-dev", "chef-server"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "3",
					Roles:   []string{"a1-dev"},
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => roles: '?' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:  "2",
					Roles:   []string{"a2-dev"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "3",
					Roles:   []string{"a1-dev", "chef-server"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "4",
					Roles:   []string{"b1-dev"},
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => role: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:  "1",
					Roles:   []string{"A2-prod"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "2",
					Roles:   []string{"a2-Dev"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "3",
					Roles:   []string{"A1-dev"},
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => role: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID:  "1",
					Roles:   []string{"A2-prod"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "2",
					Roles:   []string{"a2-Dev"},
					EndTime: time.Now(),
				},
				{
					NodeID:  "3",
					Roles:   []string{"A1-dev"},
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => Profile: '*' wildcard",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "a2-prod",
						},
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "2",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "a2-dev",
						},
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "3",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "a1-dev",
						},
					},
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => Profile: case insensitive wildcard 1",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "A2-prod",
						},
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "2",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "A2-dev",
						},
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "3",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "a1-dev",
						},
					},
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => Profile: case insensitive wildcard 2",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "A2-prod",
						},
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "2",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "a2-dev",
						},
					},
					EndTime: time.Now(),
				},
				{
					NodeID: "3",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Title: "a1-dev",
						},
					},
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => control: '*' wildcard",
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => control: case insensitive wildcard 1",
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => control: case insensitive wildcard 2",
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => control: '*' wildcard one node with two matching controls",
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
			description: "reporting_server_list_nodes_test.go => control: '?' wildcard",
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
					EndTime: time.Now(),
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
			EndTime:  time.Now(),
		},
		{
			NodeID:   nodeIds[1],
			Projects: []string{"project1"},
			EndTime:  time.Now(),
		},
		{
			NodeID:   nodeIds[2],
			Projects: []string{"project1", "project2"},
			EndTime:  time.Now(),
		},
		{
			NodeID:   nodeIds[3],
			Projects: []string{"project2"},
			EndTime:  time.Now(),
		},
		{
			NodeID:   nodeIds[4],
			Projects: []string{"project2", "project3"},
			EndTime:  time.Now(),
		},
		{
			NodeID:   nodeIds[5],
			Projects: []string{"project3"},
			EndTime:  time.Now(),
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
			description:     "reporting_server_list_nodes_test.go => Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			expectedIds:     nodeIds,
		},
		{
			description:     "reporting_server_list_nodes_test.go => Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},
			expectedIds:     nodeIds[1:3],
		},
		{
			description:     "reporting_server_list_nodes_test.go => Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},
			expectedIds:     nodeIds[1:5],
		},
		{
			description:     "reporting_server_list_nodes_test.go => Projects: user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},
			expectedIds:     []string{},
		},
		{
			description:     "reporting_server_list_nodes_test.go => Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},
			expectedIds:     nodeIds[:3],
		},
		{
			description:     "reporting_server_list_nodes_test.go => Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},
			expectedIds:     nodeIds[:5],
		},
		{
			description:     "reporting_server_list_nodes_test.go => Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},
			expectedIds:     nodeIds[:1],
		},
		{
			description:     "reporting_server_list_nodes_test.go => Projects: user has access to unassigned reports",
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
