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
	}

	for _, test := range cases {
		t.Run(test.description, func(t *testing.T) {
			suite.InsertInspecReports(test.reports)
			defer suite.DeleteAllDocuments()

			response, err := server.ListNodes(ctx, &test.query)
			assert.NoError(t, err)
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

	reportIds := suite.InsertInspecReports(reports)

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
