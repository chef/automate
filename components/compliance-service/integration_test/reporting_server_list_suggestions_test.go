package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/api/interservice/compliance/reporting"
	authzConstants "github.com/chef/automate/components/authz-service/constants"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
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
			description: "reporting_server_list_suggestions_test.go => Only two orgs are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID:           "3",
					OrganizationName: "1/75th Airborne Rangers",
					EndTime:          time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "organization",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => All orgs are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID:           "3",
					OrganizationName: "org3",
					EndTime:          time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "organization",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => No orgs are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID:           "3",
					OrganizationName: "org3",
					EndTime:          time.Now(),
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
			description: "reporting_server_list_suggestions_test.go => Only two versions are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:        "1",
					InSpecVersion: "3.1.0",
					EndTime:       time.Now(),
				},
				{
					NodeID:        "2",
					InSpecVersion: "3.1.1",
					EndTime:       time.Now(),
				},
				{
					NodeID:        "3",
					InSpecVersion: "4.1.0",
					EndTime:       time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "inspec_version",
				Text: "3.1",
			},
			expectedTerms: []string{"3.1.0", "3.1.1"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => All inspec_version are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:        "1",
					InSpecVersion: "3.1.0",
					EndTime:       time.Now(),
				},
				{
					NodeID:        "2",
					InSpecVersion: "3.1.1",
					EndTime:       time.Now(),
				},
				{
					NodeID:        "3",
					InSpecVersion: "4.1.0",
					EndTime:       time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "inspec_version",
				Text: "",
			},
			expectedTerms: []string{"3.1.0", "3.1.1", "4.1.0"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => No inspec_version are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:        "1",
					InSpecVersion: "3.1.0",
					EndTime:       time.Now(),
				},
				{
					NodeID:        "2",
					InSpecVersion: "3.1.1",
					EndTime:       time.Now(),
				},
				{
					NodeID:        "3",
					InSpecVersion: "4.1.0",
					EndTime:       time.Now(),
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
			description: "reporting_server_list_suggestions_test.go => Only two chef servers are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID:     "3",
					SourceFQDN: "bob",
					EndTime:    time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_server",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => All chef servers are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID:     "3",
					SourceFQDN: "org3",
					EndTime:    time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_server",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => No chef servers are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID:     "3",
					SourceFQDN: "org3",
					EndTime:    time.Now(),
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
			description: "reporting_server_list_suggestions_test.go => Only two chef tags are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:   "1",
					ChefTags: []string{"org1", "org4"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2", "org3"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "3",
					ChefTags: []string{"bob"},
					EndTime:  time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_tags",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2", "org3", "org4"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => All chef tags are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:   "1",
					ChefTags: []string{"org1", "org4"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2", "org5"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "3",
					ChefTags: []string{"org3", "org6"},
					EndTime:  time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "chef_tags",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3", "org4", "org5", "org6"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => No chef tags are returned",
			summaries: []*relaxting.ESInSpecSummary{
				{
					NodeID:   "1",
					ChefTags: []string{"org1", "org4"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "2",
					ChefTags: []string{"org2", "org5"},
					EndTime:  time.Now(),
				},
				{
					NodeID:   "3",
					ChefTags: []string{"org3", "org6"},
					EndTime:  time.Now(),
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
			description: "reporting_server_list_suggestions_test.go => Only two policy groups are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID:      "3",
					PolicyGroup: "bob",
					EndTime:     time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_group",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => All policy groups are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID:      "3",
					PolicyGroup: "org3",
					EndTime:     time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_group",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => No policy groups are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID:      "3",
					PolicyGroup: "org3",
					EndTime:     time.Now(),
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
			description: "reporting_server_list_suggestions_test.go => Only two policy names are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID:     "3",
					PolicyName: "bob",
					EndTime:    time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_name",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => All policy names are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID:     "3",
					PolicyName: "org3",
					EndTime:    time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_name",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => No policy names are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID:     "3",
					PolicyName: "org3",
					EndTime:    time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "policy_name",
				Text: "bob",
			},
			expectedTerms: []string{},
		},

		// platform
		{
			description: "reporting_server_list_suggestions_test.go => Only two platform names are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "bob",
					},
					EndTime: time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "platform",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => All platform names are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "org3",
					},
					EndTime: time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "platform",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => No pratform names are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Name: "org3",
					},
					EndTime: time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "platform",
				Text: "bob",
			},
			expectedTerms: []string{},
		},
		// platform_with_version
		{
			description: "reporting_server_list_suggestions_test.go => Only two platform_with_version names are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "bob",
					},
					EndTime: time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "platform_with_version",
				Text: "or",
			},
			expectedTerms: []string{"org1", "org2"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => All platform_with_version names are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "org3",
					},
					EndTime: time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "platform_with_version",
				Text: "",
			},
			expectedTerms: []string{"org1", "org2", "org3"},
		},
		{
			description: "reporting_server_list_suggestions_test.go => No platform_with_version names are returned",
			summaries: []*relaxting.ESInSpecSummary{
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
				{
					NodeID: "3",
					Platform: struct {
						Name    string `json:"name"`
						Release string `json:"release"`
						Full    string `json:"full"`
					}{
						Full: "org3",
					},
					EndTime: time.Now(),
				},
			},
			request: reporting.SuggestionRequest{
				Type: "platform",
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
			description: "reporting_server_list_suggestions_test.go => chef_tags",
			summary: &relaxting.ESInSpecSummary{
				ChefTags: terms,
				EndTime:  time.Now(),
			},
			request: &reporting.SuggestionRequest{
				Type: "chef_tags",
			},
		},
		{
			description: "reporting_server_list_suggestions_test.go => recipe",
			summary: &relaxting.ESInSpecSummary{
				Recipes: terms,
				EndTime: time.Now(),
			},
			request: &reporting.SuggestionRequest{
				Type: "recipe",
			},
		},
		{
			description: "reporting_server_list_suggestions_test.go => roles",
			summary: &relaxting.ESInSpecSummary{
				Roles:   terms,
				EndTime: time.Now(),
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

func TestReportingListSuggestionsUsersAccess(t *testing.T) {
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
		response, _ := server.ListReports(everythingCtx, &reporting.Query{
			Filters: []*reporting.ListFilter{
				{Type: "end_time", Values: []string{"2018-10-25T23:18:41Z"}},
			},
		})

		return response != nil && len(response.Reports) == n
	})

	suite.RefreshComplianceSummaryIndex()
	suite.RefreshComplianceReportIndex()

	reportsProjects := map[string][]string{
		"project1": reportIds[1:3],
		"project2": reportIds[2:5],
		"project3": reportIds[3:],
	}

	projectRules := map[string]*authz.ProjectRules{}
	for k, v := range reportsProjects {
		projectRules[k] = &authz.ProjectRules{
			Rules: []*authz.ProjectRule{
				{
					Conditions: []*authz.Condition{
						{
							Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
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
			description:     "reporting_server_list_suggestions_test.go => user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			expectedIds:     reportIds,
		},
		{
			description:     "reporting_server_list_suggestions_test.go => user has access to one project with reports",
			allowedProjects: []string{"project1"},
			expectedIds:     reportIds[1:3],
		},
		{
			description:     "reporting_server_list_suggestions_test.go => user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},
			expectedIds:     reportIds[1:5],
		},
		{
			description:     "reporting_server_list_suggestions_test.go => user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},
			expectedIds:     []string{},
		},
		{
			description:     "reporting_server_list_suggestions_test.go => user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:3],
		},
		{
			description:     "reporting_server_list_suggestions_test.go => user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:5],
		},
		{
			description:     "reporting_server_list_suggestions_test.go => user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:1],
		},
		{
			description:     "reporting_server_list_suggestions_test.go => user has access to unassigned reports",
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
				response, err := server.ListSuggestions(testCtx, &reporting.SuggestionRequest{
					Type: suggestionType,
					Filters: []*reporting.ListFilter{
						{Type: "end_time", Values: []string{"2018-10-25T18:18:41Z"}},
					},
				})

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
