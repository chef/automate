package integration_test

import (
	"strconv"
	"testing"

	"github.com/chef/automate/components/config-mgmt-service/backend"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNodeFieldValueCounts(t *testing.T) {
	cases := []struct {
		description      string
		searchTerms      []string
		filter           map[string][]string
		nodes            []iBackend.Node
		expectedResponse []backend.FieldCount
	}{
		{
			description: "three same platform",
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
				testCase.filter = map[string][]string{}
			}

			actualResponse, err := esBackend.GetNodesFieldTypes(testCase.filter, testCase.searchTerms)
			require.NoError(t, err)

			require.Equal(t, len(testCase.expectedResponse), len(actualResponse))
			for index := range actualResponse {

				assert.Equal(t, testCase.expectedResponse[index].Field, actualResponse[index].Field)
				assert.Equal(t, testCase.searchTerms[index], actualResponse[index].Field)

				expectedTerms := testCase.expectedResponse[index].Terms
				actualTerms := actualResponse[index].Terms
				require.Equal(t, len(expectedTerms), len(actualTerms))

				for _, term := range expectedTerms {
					count, found := find(term.Term, actualTerms)
					require.Truef(t, found, "term %q not found", term.Term)
					assert.Equal(t, term.Count, count)
				}
			}
		})
	}
}

func find(needle string, haystack []backend.TermCount) (int, bool) {
	for _, term := range haystack {
		if term.Term == needle {
			return term.Count, true
		}
	}
	return 0, false
}
