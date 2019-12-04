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
			existingProjectIDs: []string{projectIDToDelete},
			expectedProjectIDs: []string{},
		},
		{
			description:        "Deleting a project that does not exist",
			existingProjectIDs: []string{},
			expectedProjectIDs: []string{},
		},
		{
			description:        "Deleting a project on a report with multiple projects",
			existingProjectIDs: []string{"project3", projectIDToDelete, "project9"},
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
		suite.DeleteAllDocuments()
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
			existingProjectIDs: []string{projectIDToDelete},
			expectedProjectIDs: []string{},
		},
		{
			description:        "Deleting a project that does not exist",
			existingProjectIDs: []string{},
			expectedProjectIDs: []string{},
		},
		{
			description:        "Deleting a project on a summary with multiple projects",
			existingProjectIDs: []string{"project3", projectIDToDelete, "project9"},
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
		suite.DeleteAllDocuments()
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
	t.Run("Deleting a project from multiple summaries and reports with different projects", func(t *testing.T) {
		oneProjectReportNode := "one-proj-rep"
		multiProjectReportNode := "multi-proj-rep"
		noProjectReportNode := "no-proj-rep"

		reports := []*relaxting.ESInSpecReport{
			{
				NodeID:   oneProjectReportNode,
				Projects: []string{projectIDToDelete},
			},
			{
				NodeID:   multiProjectReportNode,
				Projects: []string{"project3", projectIDToDelete, "project9"},
			},
			{
				NodeID:   noProjectReportNode,
				Projects: []string{},
			},
		}

		oneProjectSummaryNode := "one-proj-sum"
		multiProjectSummaryNode := "multi-proj-sum"
		noProjectSummaryNode := "no-proj-sum"

		summaries := []*relaxting.ESInSpecSummary{
			{
				NodeID:   oneProjectSummaryNode,
				Projects: []string{projectIDToDelete},
			},
			{
				NodeID:   multiProjectSummaryNode,
				Projects: []string{"project4", projectIDToDelete, "project8"},
			},
			{
				NodeID:   noProjectSummaryNode,
				Projects: []string{},
			},
		}

		_, err := suite.InsertInspecReports(reports)
		require.NoError(t, err)
		_, err = suite.InsertInspecSummaries(summaries)
		require.NoError(t, err)

		esJobIDs, err := suite.ingesticESClient.DeleteProjectTag(ctx, projectIDToDelete)
		require.NoError(t, err)

		waitForESJobsToComplete(esJobIDs)
		suite.RefreshComplianceReportIndex()
		suite.RefreshComplianceSummaryIndex()

		updatedReports, err := suite.GetAllReportsESInSpecReport()
		require.NoError(t, err)
		require.Equal(t, 3, len(updatedReports))

		reportMap := make(map[string]*relaxting.ESInSpecReport, len(updatedReports))
		for _, report := range updatedReports {
			reportMap[report.NodeID] = report
		}

		updatedSummaries, err := suite.GetAllSummaryESInSpecSummary()
		require.NoError(t, err)
		require.Equal(t, 3, len(updatedSummaries))

		summaryMap := make(map[string]*relaxting.ESInSpecSummary, len(updatedSummaries))
		for _, summary := range updatedSummaries {
			summaryMap[summary.NodeID] = summary
		}

		assert.Equal(t, []string{}, reportMap[noProjectReportNode].Projects)
		assert.Equal(t, []string{}, reportMap[oneProjectReportNode].Projects)
		assert.Equal(t, []string{}, summaryMap[noProjectSummaryNode].Projects)
		assert.Equal(t, []string{}, summaryMap[oneProjectSummaryNode].Projects)

		assert.ElementsMatch(t, []string{"project3", "project9"}, reportMap[multiProjectReportNode].Projects)
		assert.ElementsMatch(t, []string{"project4", "project8"}, summaryMap[multiProjectSummaryNode].Projects)
	})
	suite.DeleteAllDocuments()
}

func waitForESJobsToComplete(jobs []string) {
	for _, job := range jobs {
		suite.WaitForESJobToComplete(job)
	}
}
