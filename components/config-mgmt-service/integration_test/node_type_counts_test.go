package integration_test

import (
	"context"
	"strconv"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/chef/automate/components/config-mgmt-service/backend"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNodeFieldValueCounts(t *testing.T) {
	cases := []struct {
		description      string
		searchTerms      []string
		filter           []string
		start            string
		end              string
		nodes            []iBackend.Node
		expectedResponse []backend.FieldCount
	}{
		{
			description: "Two search terms with the same two filter terms",
			searchTerms: []string{"platform", "status"},
			filter:      []string{"status:failure", "platform:windows"},
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						Platform: "windows",
						Status:   "failure",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						Platform: "linux",
						Status:   "failure",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						Platform: "windows",
						Status:   "successful",
					},
				},
			},
			expectedResponse: []backend.FieldCount{
				{
					Field: "platform",
					Terms: []backend.TermCount{
						{
							Term:  "windows",
							Count: 1,
						},
						{
							Term:  "linux",
							Count: 1,
						},
					},
				},
				{
					Field: "status",
					Terms: []backend.TermCount{
						{
							Term:  "failure",
							Count: 1,
						},
						{
							Term:  "successful",
							Count: 1,
						},
					},
				},
			},
		},
		{
			description: "Two fields with two different values for each",
			searchTerms: []string{"platform", "status"},
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						Platform: "windows",
						Status:   "failure",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						Platform: "linux",
						Status:   "failure",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						Platform: "windows",
						Status:   "successful",
					},
				},
			},
			expectedResponse: []backend.FieldCount{
				{
					Field: "platform",
					Terms: []backend.TermCount{
						{
							Term:  "windows",
							Count: 2,
						},
						{
							Term:  "linux",
							Count: 1,
						},
					},
				},
				{
					Field: "status",
					Terms: []backend.TermCount{
						{
							Term:  "failure",
							Count: 2,
						},
						{
							Term:  "successful",
							Count: 1,
						},
					},
				},
			},
		},
		{
			description: "Status filter with platform value counts",
			searchTerms: []string{"platform"},
			filter:      []string{"status:failure"},
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						Platform: "windows",
						Status:   "failure",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						Platform: "linux",
						Status:   "failure",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						Platform: "redhat",
						Status:   "successful",
					},
				},
			},
			expectedResponse: []backend.FieldCount{
				{
					Field: "platform",
					Terms: []backend.TermCount{
						{
							Term:  "windows",
							Count: 1,
						},
						{
							Term:  "linux",
							Count: 1,
						},
					},
				},
			},
		},
		{
			description: "filtering on the same field counting values for",
			searchTerms: []string{"platform"},
			filter:      []string{"platform:windows"},
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						Platform: "windows",
						Status:   "failure",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						Platform: "linux",
						Status:   "failure",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						Platform: "redhat",
						Status:   "successful",
					},
				},
			},
			expectedResponse: []backend.FieldCount{
				{
					Field: "platform",
					Terms: []backend.TermCount{
						{
							Term:  "windows",
							Count: 1,
						},
						{
							Term:  "linux",
							Count: 1,
						},
						{
							Term:  "redhat",
							Count: 1,
						},
					},
				},
			},
		},
		{
			description: "No nodes",
			searchTerms: []string{"platform"},
			nodes:       []iBackend.Node{},
			expectedResponse: []backend.FieldCount{
				{
					Field: "platform",
					Terms: []backend.TermCount{},
				},
			},
		},
		{
			description: "Start Date filter",
			searchTerms: []string{"platform"},
			start:       time.Now().Add(-time.Hour * 24).Format(time.RFC3339),
			nodes: []iBackend.Node{
				{
					Checkin: time.Now().Add(-time.Hour * 12),
					NodeInfo: iBackend.NodeInfo{
						Platform: "windows",
					},
				},
				{
					Checkin: time.Now().AddDate(0, 0, -3),
					NodeInfo: iBackend.NodeInfo{
						Platform: "redhat",
					},
				},
				{
					Checkin: time.Now().AddDate(0, 0, -4),
					NodeInfo: iBackend.NodeInfo{
						Platform: "ubuntu",
					},
				},
			},
			expectedResponse: []backend.FieldCount{
				{
					Field: "platform",
					Terms: []backend.TermCount{
						{
							Term:  "windows",
							Count: 1,
						},
					},
				},
			},
		},
		{
			description: "End Date filter",
			searchTerms: []string{"platform"},
			end:         time.Now().Add(-time.Hour * 24).Format(time.RFC3339),
			nodes: []iBackend.Node{
				{
					Checkin: time.Now().Add(-time.Hour * 12),
					NodeInfo: iBackend.NodeInfo{
						Platform: "windows",
					},
				},
				{
					Checkin: time.Now().AddDate(0, 0, -3),
					NodeInfo: iBackend.NodeInfo{
						Platform: "redhat",
					},
				},
				{
					Checkin: time.Now().AddDate(0, 0, -4),
					NodeInfo: iBackend.NodeInfo{
						Platform: "ubuntu",
					},
				},
			},
			expectedResponse: []backend.FieldCount{
				{
					Field: "platform",
					Terms: []backend.TermCount{
						{
							Term:  "redhat",
							Count: 1,
						},
						{
							Term:  "ubuntu",
							Count: 1,
						},
					},
				},
			},
		},
		{
			description: "Start and End Date filter",
			searchTerms: []string{"platform"},
			start:       time.Now().Add(-time.Hour * 24 * 5).Format(time.RFC3339),
			end:         time.Now().Add(-time.Hour * 24).Format(time.RFC3339),
			nodes: []iBackend.Node{
				{
					Checkin: time.Now().Add(-time.Hour * 12),
					NodeInfo: iBackend.NodeInfo{
						Platform: "windows",
					},
				},
				{
					Checkin: time.Now().AddDate(0, 0, -3),
					NodeInfo: iBackend.NodeInfo{
						Platform: "redhat",
					},
				},
				{
					Checkin: time.Now().AddDate(0, 0, -14),
					NodeInfo: iBackend.NodeInfo{
						Platform: "ubuntu",
					},
				},
			},
			expectedResponse: []backend.FieldCount{
				{
					Field: "platform",
					Terms: []backend.TermCount{
						{
							Term:  "redhat",
							Count: 1,
						},
					},
				},
			},
		},
	}

	for _, testCase := range cases {
		t.Run(testCase.description, func(t *testing.T) {
			// Adding required nodes
			for index := range testCase.nodes {
				nodeID := newUUID()
				testCase.nodes[index].Exists = true
				testCase.nodes[index].NodeInfo.EntityUuid = nodeID
				testCase.nodes[index].NodeName = strconv.Itoa(index)
			}

			suite.IngestNodes(testCase.nodes)
			defer suite.DeleteAllDocuments()

			if testCase.filter == nil {
				testCase.filter = []string{}
			}

			actualResponse, err := cfgmgmt.GetNodesFieldValueCounts(context.Background(),
				&request.NodesFieldValueCounts{
					Terms:  testCase.searchTerms,
					Filter: testCase.filter,
					Start:  testCase.start,
					End:    testCase.end,
				})
			require.NoError(t, err)

			require.Equal(t, len(testCase.expectedResponse), len(actualResponse.Fields))
			for index := range actualResponse.Fields {

				assert.Equal(t, testCase.expectedResponse[index].Field, actualResponse.Fields[index].Field)
				assert.Equal(t, testCase.searchTerms[index], actualResponse.Fields[index].Field)

				expectedTerms := testCase.expectedResponse[index].Terms
				actualTerms := actualResponse.Fields[index].Terms
				require.Equal(t, len(expectedTerms), len(actualTerms),
					"field term lengths for %q do not match", testCase.searchTerms[index])

				for _, term := range expectedTerms {
					count, found := find(term.Term, actualTerms)
					require.Truef(t, found, "term %q not found", term.Term)
					assert.Equal(t, term.Count, count)
				}
			}
		})
	}
}

func TestNodeFieldValueCountsError(t *testing.T) {
	cases := []struct {
		description string
		searchTerms []string
		start       string
		end         string
	}{
		{
			description: "Start date is after End Date filter",
			searchTerms: []string{"platform"},
			start:       time.Now().Add(-time.Hour * 24).Format(time.RFC3339),
			end:         time.Now().Add(-time.Hour * 24 * 5).Format(time.RFC3339),
		},
	}

	for _, testCase := range cases {
		t.Run(testCase.description, func(t *testing.T) {

			_, err := cfgmgmt.GetNodesFieldValueCounts(context.Background(),
				&request.NodesFieldValueCounts{
					Terms: testCase.searchTerms,
					Start: testCase.start,
					End:   testCase.end,
				})
			require.Error(t, err)
		})
	}
}

func find(needle string, haystack []*response.TermCount) (int, bool) {
	for _, term := range haystack {
		if term.Term == needle {
			return int(term.Count), true
		}
	}
	return 0, false
}
