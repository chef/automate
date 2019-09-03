package integration_test

import (
	"context"
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

func TestReportingListSuggestionsFiltering(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	ctx := context.Background()

	cases := []struct {
		description   string
		summaries     []*relaxting.ESInSpecSummary
		request       reporting.SuggestionRequest
		expectedTerms []string
	}{
		// organization
		{
			description: "Only two orgs are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:           "1",
					OrganizationName: "org1",
				},
				{
					NodeID:           "2",
					OrganizationName: "org2",
				},
				{
					NodeID:           "3",
					OrganizationName: "1/75th Airborne Rangers",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "organization",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "All orgs are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:           "1",
					OrganizationName: "org1",
				},
				{
					NodeID:           "2",
					OrganizationName: "org2",
				},
				{
					NodeID:           "3",
					OrganizationName: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "organization",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "No orgs are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:           "1",
					OrganizationName: "org1",
				},
				{
					NodeID:           "2",
					OrganizationName: "org2",
				},
				{
					NodeID:           "3",
					OrganizationName: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "organization",
				Text: "bob",
			},
			expectedTerms: []string{},
		},

		// inspec_version
		{
			description: "Only two versions are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:        "1",
					InSpecVersion: "3.1.0",
				},
				{
					NodeID:        "2",
					InSpecVersion: "3.1.1",
				},
				{
					NodeID:        "3",
					InSpecVersion: "4.1.0",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "inspec_version",
				Text: "3.1",
			},
			expectedTerms: []string{"3.1.0", "3.1.1"},
		},
		{
			description: "All inspec_version are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:        "1",
					InSpecVersion: "3.1.0",
				},
				{
					NodeID:        "2",
					InSpecVersion: "3.1.1",
				},
				{
					NodeID:        "3",
					InSpecVersion: "4.1.0",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "inspec_version",
				Text: "",
			},
			expectedTerms: []string{"3.1.0", "3.1.1", "4.1.0"},
		},
		{
			description: "No inspec_version are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:        "1",
					InSpecVersion: "3.1.0",
				},
				{
					NodeID:        "2",
					InSpecVersion: "3.1.1",
				},
				{
					NodeID:        "3",
					InSpecVersion: "4.1.0",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "inspec_version",
				Text: "1.0",
			},
			expectedTerms: []string{},
		},

		// chef_server
		{
			description: "Only two chef servers are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:     "1",
					SourceFQDN: "org1",
				},
				{
					NodeID:     "2",
					SourceFQDN: "org2",
				},
				{
					NodeID:     "3",
					SourceFQDN: "bob",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_server",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "All chef servers are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:     "1",
					SourceFQDN: "org1",
				},
				{
					NodeID:     "2",
					SourceFQDN: "org2",
				},
				{
					NodeID:     "3",
					SourceFQDN: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_server",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "No chef servers are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:     "1",
					SourceFQDN: "org1",
				},
				{
					NodeID:     "2",
					SourceFQDN: "org2",
				},
				{
					NodeID:     "3",
					SourceFQDN: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_server",
				Text: "bob",
			},
			expectedTerms: []string{},
		},

		// chef_tags
		{
			description: "Only two chef tags are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:   "1",
					ChefTags: []string{"org1", "org4"},
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2", "org3"},
				},
				{
					NodeID:   "3",
					ChefTags: []string{"bob"},
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_tags",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2", "org3", "org4"},
		},
		{
			description: "All chef tags are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:   "1",
					ChefTags: []string{"org1", "org4"},
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2", "org5"},
				},
				{
					NodeID:   "3",
					ChefTags: []string{"org3", "org6"},
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_tags",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3", "org4", "org5", "org6"},
		},
		{
			description: "No chef tags are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:   "1",
					ChefTags: []string{"org1", "org4"},
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2", "org5"},
				},
				{
					NodeID:   "3",
					ChefTags: []string{"org3", "org6"},
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_tags",
				Text: "bob",
			},
			expectedTerms: []string{},
		},

		// policy_group
		{
			description: "Only two policy groups are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:      "1",
					PolicyGroup: "org1",
				},
				{
					NodeID:      "2",
					PolicyGroup: "org2",
				},
				{
					NodeID:      "3",
					PolicyGroup: "bob",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_group",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "All policy groups are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:      "1",
					PolicyGroup: "org1",
				},
				{
					NodeID:      "2",
					PolicyGroup: "org2",
				},
				{
					NodeID:      "3",
					PolicyGroup: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_group",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "No policy groups are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:      "1",
					PolicyGroup: "org1",
				},
				{
					NodeID:      "2",
					PolicyGroup: "org2",
				},
				{
					NodeID:      "3",
					PolicyGroup: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_group",
				Text: "bob",
			},
			expectedTerms: []string{},
		},

		// policy_name
		{
			description: "Only two policy names are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:     "1",
					PolicyName: "org1",
				},
				{
					NodeID:     "2",
					PolicyName: "org2",
				},
				{
					NodeID:     "3",
					PolicyName: "bob",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_name",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "All policy names are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:     "1",
					PolicyName: "org1",
				},
				{
					NodeID:     "2",
					PolicyName: "org2",
				},
				{
					NodeID:     "3",
					PolicyName: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_name",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "No policy names are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:     "1",
					PolicyName: "org1",
				},
				{
					NodeID:     "2",
					PolicyName: "org2",
				},
				{
					NodeID:     "3",
					PolicyName: "org3",
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_name",
				Text: "bob",
			},
			expectedTerms: []string{},
		},
	}

	for _, test := range cases {
		t.Run(test.description, func(t *testing.T) {
			_, err := suite.InsertInspecSummaries(test.summaries)
			require.NoError(t, err)
			defer suite.DeleteAllDocuments()

			response, err := server.ListSuggestions(ctx, &test.request)
			require.NoError(t, err)
			require.NotNil(t, response)

			actualTerms := make([]string, len(response.Suggestions))
			for i, suggestion := range response.Suggestions {
				actualTerms[i] = suggestion.Text
			}

			assert.ElementsMatch(t, test.expectedTerms, actualTerms)
		})
	}
}

// The terms (sorted alphabetically) after the first 500 are not included in the suggestions.
func TestReportingListSuggestionsLargeArrayValues(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	ctx := context.Background()

	terms := make([]string, 500)

	for index := range terms {
		terms[index] = fmt.Sprintf("aaa-%d", index)
	}

	for count := 0; count < 20; count++ {
		terms = append(terms, fmt.Sprintf("zzz-%d", count))
	}

	cases := []struct {
		description string
		summary     *relaxting.ESInSpecSummary
		request     *reporting.SuggestionRequest
	}{
		{
			description: "chef_tags",
			summary: &relaxting.ESInSpecSummary{
				ChefTags: terms,
			},
			request: &reporting.SuggestionRequest{
				Type: "chef_tags",
			},
		},
		{
			description: "recipe",
			summary: &relaxting.ESInSpecSummary{
				Recipes: terms,
			},
			request: &reporting.SuggestionRequest{
				Type: "recipe",
			},
		},
		{
			description: "roles",
			summary: &relaxting.ESInSpecSummary{
				Roles: terms,
			},
			request: &reporting.SuggestionRequest{
				Type: "role",
			},
		},
	}

	for _, test := range cases {
		t.Run(test.description, func(t *testing.T) {

			// Ingest the reports
			_, err := suite.InsertInspecSummaries([]*relaxting.ESInSpecSummary{test.summary})
			require.NoError(t, err)
			defer suite.DeleteAllDocuments()

			// last term alphabetically and lowercase
			expectedTerm := "zzz-9"

			// defaults to 10
			test.request.Size = 2
			// term to give suggestions for
			test.request.Text = "Zzz-9"

			// Make the request for suggestions
			response, err := server.ListSuggestions(ctx, test.request)
			require.NoError(t, err)
			require.NotNil(t, response)

			// abstract the terms from the response
			actualTerms := make([]string, len(response.Suggestions))
			for i, suggestion := range response.Suggestions {
				actualTerms[i] = suggestion.Text
			}

			// Check to see if last term alphabetically is a returned suggestion
			assert.Contains(t, actualTerms, expectedTerm)
		})
	}
}

func TestReportingListSuggestions(t *testing.T) {
	suite.DeleteAllDocuments()

	reportFileName := "../ingest/examples/compliance-success-tiny-report.json"
	everythingCtx := contextWithProjects([]string{authzConstants.AllProjectsExternalID})

	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})

	n := 5

	reportIds := make([]string, n)

	for i := 0; i < n; i++ {
		err := suite.ingestReport(reportFileName, func(r *compliance.Report) {
			id := newUUID()

			r.Environment = id
			r.NodeName = id
			r.Platform.Name = id
			r.Profiles[0].Controls = r.Profiles[0].Controls[:1]
			r.Profiles[0].Controls[0].Id = id
			r.Profiles[0].Controls[0].Title = id
			r.Profiles = r.Profiles[:1]
			r.Profiles[0].Sha256 = id
			r.Profiles[0].Title = id
			r.Recipes = []string{id}
			r.ReportUuid = id
			r.Roles = []string{id}

			reportIds[i] = id
		})

		require.NoError(t, err)
	}

	defer suite.DeleteAllDocuments()

	waitFor(func() bool {
		response, _ := server.ListReports(everythingCtx, &reporting.Query{})

		return response != nil && len(response.Reports) == n
	})

	suite.RefreshComplianceSummaryIndex()
	suite.RefreshComplianceReportIndex()

	reportsProjects := map[string][]string{
		"project1": reportIds[1:3],
		"project2": reportIds[2:5],
		"project3": reportIds[3:],
	}

	projectRules := map[string]*iam_v2.ProjectRules{}
	for k, v := range reportsProjects {
		projectRules[k] = &iam_v2.ProjectRules{
			Rules: []*iam_v2.ProjectRule{
				{
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    v,
						},
					},
				},
			},
		}
	}

	// Send a project rules update event
	esJobID, err := suite.ingesticESClient.UpdateReportProjectsTags(everythingCtx, projectRules)
	assert.Nil(t, err)

	suite.WaitForESJobToComplete(esJobID)

	suite.RefreshComplianceReportIndex()

	esJobID, err = suite.ingesticESClient.UpdateSummaryProjectsTags(everythingCtx, projectRules)
	assert.Nil(t, err)

	suite.WaitForESJobToComplete(esJobID)

	suite.RefreshComplianceSummaryIndex()

	successCases := []struct {
		description     string
		allowedProjects []string
		expectedIds     []string
	}{
		{
			description:     "user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			expectedIds:     reportIds,
		},
		{
			description:     "user has access to one project with reports",
			allowedProjects: []string{"project1"},
			expectedIds:     reportIds[1:3],
		},
		{
			description:     "user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},
			expectedIds:     reportIds[1:5],
		},
		{
			description:     "user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},
			expectedIds:     []string{},
		},
		{
			description:     "user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:3],
		},
		{
			description:     "user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:5],
		},
		{
			description:     "user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:1],
		},
		{
			description:     "user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:1],
		},
	}

	suggestionTypes := []string{
		"environment",
		"node",
		"platform",
		"profile",
		"recipe",
		"role",
		"control",
	}

	for _, suggestionType := range suggestionTypes {
		for _, test := range successCases {
			t.Run(fmt.Sprintf("Projects: %q suggestions, %s", suggestionType, test.description), func(t *testing.T) {
				testCtx := contextWithProjects(test.allowedProjects)
				response, err := server.ListSuggestions(testCtx, &reporting.SuggestionRequest{Type: suggestionType})

				assert.NoError(t, err)
				require.NotNil(t, response)

				actualValues := make([]string, len(response.Suggestions))
				for i, suggestion := range response.Suggestions {
					actualValues[i] = suggestion.Text
				}

				assert.ElementsMatch(t, test.expectedIds, actualValues)
			})
		}
	}
}
