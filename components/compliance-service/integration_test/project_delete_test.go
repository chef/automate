package integration_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

var (
	ctx               = context.Background()
	projectIDToDelete = "target_project"
)

func TestReportProjectDelete(t *testing.T) {

	cases := []struct {
		description        string
		existingProjectIDs []string
		projectIDToDelete  string
		expectedProjectIDs []string
	}{
		{
			description:        "Deleting the last project tag",
			existingProjectIDs: []string{"target_project"},
			expectedProjectIDs: []string{},
		},
		{
			description:        "Deleting a project that does not exist",
			existingProjectIDs: []string{},
			expectedProjectIDs: []string{},
		},
		{
			description:        "Deleting a project on a report with multiple projects",
			existingProjectIDs: []string{"project3", "target_project", "project9"},
			expectedProjectIDs: []string{"project3", "project9"},
		},
	}

	for _, test := range cases {
		t.Run(test.description, func(t *testing.T) {

			report := &relaxting.ESInSpecReport{
				Projects: test.existingProjectIDs,
			}

			_, err := suite.InsertInspecReports([]*relaxting.ESInSpecReport{report})
			require.NoError(t, err)

			defer suite.DeleteAllDocuments()

			esJobID, err := suite.ingesticESClient.DeleteReportProjectTag(ctx, projectIDToDelete)
			require.NoError(t, err)

			suite.WaitForESJobToComplete(esJobID)

			suite.RefreshComplianceReportIndex()

			// get the reports
			reports, err := suite.GetAllReportsESInSpecReport()
			require.NoError(t, err)
			require.Equal(t, 1, len(reports))

			updatedReport := reports[0]

			assert.ElementsMatch(t, test.expectedProjectIDs, updatedReport.Projects)
		})
	}
}

func TestReportProjectDeleteNoReports(t *testing.T) {
	t.Run("Deleting a project when there are no reports", func(t *testing.T) {
		esJobID, err := suite.ingesticESClient.DeleteReportProjectTag(ctx, projectIDToDelete)
		assert.NoError(t, err)

		suite.WaitForESJobToComplete(esJobID)

		suite.RefreshComplianceReportIndex()

		reports, err := suite.GetAllReportsESInSpecReport()
		require.NoError(t, err)
		require.Equal(t, 0, len(reports))
	})
}

func TestSummaryProjectDelete(t *testing.T) {
	cases := []struct {
		description        string
		existingProjectIDs []string
		projectIDToDelete  string
		expectedProjectIDs []string
	}{
		{
			description:        "Deleting the last project tag",
			existingProjectIDs: []string{"target_project"},
			expectedProjectIDs: []string{},
		},
		{
			description:        "Deleting a project that does not exist",
			existingProjectIDs: []string{},
			expectedProjectIDs: []string{},
		},
		{
			description:        "Deleting a project on a summary with multiple projects",
			existingProjectIDs: []string{"project3", "target_project", "project9"},
			expectedProjectIDs: []string{"project3", "project9"},
		},
	}

	for _, test := range cases {
		t.Run(test.description, func(t *testing.T) {

			summary := &relaxting.ESInSpecSummary{
				Projects: test.existingProjectIDs,
			}

			_, err := suite.InsertInspecSummaries([]*relaxting.ESInSpecSummary{summary})
			require.NoError(t, err)

			defer suite.DeleteAllDocuments()

			esJobID, err := suite.ingesticESClient.DeleteSummaryProjectTag(ctx, projectIDToDelete)
			require.NoError(t, err)

			suite.WaitForESJobToComplete(esJobID)

			suite.RefreshComplianceSummaryIndex()

			// get the summaries
			summaries, err := suite.GetAllSummaryESInSpecSummary()
			require.NoError(t, err)
			require.Equal(t, 1, len(summaries))

			updatedSummary := summaries[0]

			assert.ElementsMatch(t, test.expectedProjectIDs, updatedSummary.Projects)
		})
	}
}

func TestSummaryProjectDeleteNoSummaries(t *testing.T) {
	t.Run("Deleting a project when there are no summaries", func(t *testing.T) {
		esJobID, err := suite.ingesticESClient.DeleteSummaryProjectTag(ctx, projectIDToDelete)
		assert.NoError(t, err)

		suite.WaitForESJobToComplete(esJobID)

		suite.RefreshComplianceSummaryIndex()

		summaries, err := suite.GetAllSummaryESInSpecSummary()
		require.NoError(t, err)
		require.Equal(t, 0, len(summaries))
	})
}

func TestProjectDelete(t *testing.T) {
	t.Run("Deleting a project from summaries and reports", func(t *testing.T) {
		// ingest both summary and report

		// call DeleteProjectTag(ctx, projectToDelete)

		// wait

		// refresh

		// assert project is removed from both summary and report
	})
}
