package integration_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/compliance-service/api/reporting"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/lib/grpc/auth_context"
)

func contextWithProjects(projects []string) context.Context {
	ctx := context.Background()
	return auth_context.NewContext(ctx, []string{}, projects, "", "", "")
}

func TestReadReport(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	report := relaxting.ESInSpecReport{Projects: []string{"project1", "project2"}}
	reportIds := suite.InsertInspecReports([]*relaxting.ESInSpecReport{&report})

	defer suite.DeleteAllDocuments()

	require.Len(t, reportIds, 1)

	reportId := reportIds[0]

	successCases := []struct {
		description     string
		allowedProjects []string
		expectedId      string
	}{
		{
			description:     "Projects: user has access to all projects",
			allowedProjects: []string{"*"},
			expectedId:      reportId,
		},
		{
			description:     "Projects: user has access to all projects a report belongs to",
			allowedProjects: []string{"project1", "project2"},
			expectedId:      reportId,
		},
		{
			description:     "Projects: user has access to one project a report belongs to",
			allowedProjects: []string{"project1"},
			expectedId:      reportId,
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			response, err := server.ReadReport(ctx, &reporting.Query{Id: reportId})

			assert.NoError(t, err)
			require.NotNil(t, response)
			assert.Equal(t, test.expectedId, response.Id)
		})
	}

	description := "Projects: user does not have access to any projects a report belongs to"
	allowedProjects := []string{"project3"}
	t.Run(description, func(t *testing.T) {
		ctx := contextWithProjects(allowedProjects)

		response, err := server.ReadReport(ctx, &reporting.Query{Id: reportId})

		assert.Error(t, err)
		assert.Nil(t, response)
	})
}
