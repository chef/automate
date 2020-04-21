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

func TestGetNodeMetadataCounts(t *testing.T) {
	cases := []struct {
		description      string
		types            []string
		filter           []string
		start            string
		end              string
		nodes            []iBackend.Node
		expectedResponse []backend.TypeCount
	}{
		{
			description: "Two search terms with the same two filter terms",
			types:       []string{"platform", "status"},
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
			expectedResponse: []backend.TypeCount{
				{
					Type: "platform",
					Values: []backend.ValueCount{
						{
							Value: "windows",
							Count: 1,
						},
						{
							Value: "linux",
							Count: 1,
						},
					},
				},
				{
					Type: "status",
					Values: []backend.ValueCount{
						{
							Value: "failure",
							Count: 1,
						},
						{
							Value: "successful",
							Count: 1,
						},
					},
				},
			},
		},
		{
			description: "Two fields with two different values for each",
			types:       []string{"platform", "status"},
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
			expectedResponse: []backend.TypeCount{
				{
					Type: "platform",
					Values: []backend.ValueCount{
						{
							Value: "windows",
							Count: 2,
						},
						{
							Value: "linux",
							Count: 1,
						},
					},
				},
				{
					Type: "status",
					Values: []backend.ValueCount{
						{
							Value: "failure",
							Count: 2,
						},
						{
							Value: "successful",
							Count: 1,
						},
					},
				},
			},
		},
		{
			description: "Status filter with platform value counts",
			types:       []string{"platform"},
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
			expectedResponse: []backend.TypeCount{
				{
					Type: "platform",
					Values: []backend.ValueCount{
						{
							Value: "windows",
							Count: 1,
						},
						{
							Value: "linux",
							Count: 1,
						},
					},
				},
			},
		},
		{
			description: "filtering on the same field counting values for",
			types:       []string{"platform"},
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
			expectedResponse: []backend.TypeCount{
				{
					Type: "platform",
					Values: []backend.ValueCount{
						{
							Value: "windows",
							Count: 1,
						},
						{
							Value: "linux",
							Count: 1,
						},
						{
							Value: "redhat",
							Count: 1,
						},
					},
				},
			},
		},
		{
			description: "No nodes",
			types:       []string{"platform"},
			nodes:       []iBackend.Node{},
			expectedResponse: []backend.TypeCount{
				{
					Type:   "platform",
					Values: []backend.ValueCount{},
				},
			},
		},
		{
			description: "Start Date filter",
			types:       []string{"platform"},
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
			expectedResponse: []backend.TypeCount{
				{
					Type: "platform",
					Values: []backend.ValueCount{
						{
							Value: "windows",
							Count: 1,
						},
					},
				},
			},
		},
		{
			description: "End Date filter",
			types:       []string{"platform"},
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
			expectedResponse: []backend.TypeCount{
				{
					Type: "platform",
					Values: []backend.ValueCount{
						{
							Value: "redhat",
							Count: 1,
						},
						{
							Value: "ubuntu",
							Count: 1,
						},
					},
				},
			},
		},
		{
			description: "Start and End Date filter",
			types:       []string{"platform"},
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
			expectedResponse: []backend.TypeCount{
				{
					Type: "platform",
					Values: []backend.ValueCount{
						{
							Value: "redhat",
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

			actualResponse, err := cfgmgmt.GetNodeMetadataCounts(context.Background(),
				&request.NodeMetadataCounts{
					Type:   testCase.types,
					Filter: testCase.filter,
					Start:  testCase.start,
					End:    testCase.end,
				})
			require.NoError(t, err)

			require.Equal(t, len(testCase.expectedResponse), len(actualResponse.Types))
			for index := range actualResponse.Types {

				assert.Equal(t, testCase.expectedResponse[index].Type, actualResponse.Types[index].Type)
				assert.Equal(t, testCase.types[index], actualResponse.Types[index].Type)

				expectedValues := testCase.expectedResponse[index].Values
				actualValues := actualResponse.Types[index].Values
				require.Equal(t, len(expectedValues), len(actualValues),
					"field term lengths for %q do not match", testCase.types[index])

				for _, expectedValueCount := range expectedValues {
					actualCount, found := find(expectedValueCount.Value, actualValues)
					require.Truef(t, found, "term %q not found", expectedValueCount.Value)
					assert.Equal(t, expectedValueCount.Count, actualCount)
				}
			}
		})
	}
}

func TestGetNodeMetadataCountsError(t *testing.T) {
	cases := []struct {
		description string
		types       []string
		start       string
		end         string
	}{
		{
			description: "Start date is after End Date filter",
			types:       []string{"platform"},
			start:       time.Now().Add(-time.Hour * 24).Format(time.RFC3339),
			end:         time.Now().Add(-time.Hour * 24 * 5).Format(time.RFC3339),
		},
	}

	for _, testCase := range cases {
		t.Run(testCase.description, func(t *testing.T) {

			_, err := cfgmgmt.GetNodeMetadataCounts(context.Background(),
				&request.NodeMetadataCounts{
					Type:  testCase.types,
					Start: testCase.start,
					End:   testCase.end,
				})
			require.Error(t, err)
		})
	}
}

func find(needle string, haystack []*response.ValueCount) (int, bool) {
	for _, valueCount := range haystack {
		if valueCount.Value == needle {
			return int(valueCount.Count), true
		}
	}
	return 0, false
}
