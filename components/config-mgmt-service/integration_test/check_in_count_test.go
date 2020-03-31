package integration_test

import (
	"strconv"
	"testing"
	"time"

	"github.com/chef/automate/components/config-mgmt-service/backend"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCheckinCount(t *testing.T) {

	cases := []struct {
		now         time.Time
		description string
		daysAgo     int
		filter      map[string][]string
		nodeSets    []struct {
			node iBackend.Node
			runs []iBackend.Run
		}
		expectedResponse []backend.CountPeroid
	}{
		{
			description: "Zero nodes three days window",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     3,
			expectedResponse: []backend.CountPeroid{
				{
					Count: 0,
					Start: parseTime(t, "2020-03-12T13:00:00Z"),
					End:   parseTime(t, "2020-03-13T12:59:59Z"),
				},
				{
					Count: 0,
					Start: parseTime(t, "2020-03-13T13:00:00Z"),
					End:   parseTime(t, "2020-03-14T12:59:59Z"),
				},
				{
					Count: 0,
					Start: parseTime(t, "2020-03-14T13:00:00Z"),
					End:   parseTime(t, "2020-03-15T12:59:59Z"),
				},
			},
			nodeSets: []struct {
				node iBackend.Node
				runs []iBackend.Run
			}{},
		},
		{
			description: "One node checks-in all three days",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     3,
			expectedResponse: []backend.CountPeroid{
				{
					Count: 1,
					Start: parseTime(t, "2020-03-12T13:00:00Z"),
					End:   parseTime(t, "2020-03-13T12:59:59Z"),
				},
				{
					Count: 1,
					Start: parseTime(t, "2020-03-13T13:00:00Z"),
					End:   parseTime(t, "2020-03-14T12:59:59Z"),
				},
				{
					Count: 1,
					Start: parseTime(t, "2020-03-14T13:00:00Z"),
					End:   parseTime(t, "2020-03-15T12:59:59Z"),
				},
			},
			nodeSets: []struct {
				node iBackend.Node
				runs []iBackend.Run
			}{
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-12T13:01:00Z"),
							EndTime:   parseTime(t, "2020-03-12T13:02:59Z"),
						},
						{
							StartTime: parseTime(t, "2020-03-14T12:01:00Z"),
							EndTime:   parseTime(t, "2020-03-14T12:02:59Z"),
						},
						{
							StartTime: parseTime(t, "2020-03-14T13:05:00Z"),
							EndTime:   parseTime(t, "2020-03-14T13:06:59Z"),
						},
					},
				},
			},
		},
		{
			description: "Empty last bucket",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     3,
			expectedResponse: []backend.CountPeroid{
				{
					Count: 1,
					Start: parseTime(t, "2020-03-12T13:00:00Z"),
					End:   parseTime(t, "2020-03-13T12:59:59Z"),
				},
				{
					Count: 1,
					Start: parseTime(t, "2020-03-13T13:00:00Z"),
					End:   parseTime(t, "2020-03-14T12:59:59Z"),
				},
				{
					Count: 0,
					Start: parseTime(t, "2020-03-14T13:00:00Z"),
					End:   parseTime(t, "2020-03-15T12:59:59Z"),
				},
			},
			nodeSets: []struct {
				node iBackend.Node
				runs []iBackend.Run
			}{
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-12T13:01:00Z"),
							EndTime:   parseTime(t, "2020-03-12T13:02:59Z"),
						},
						{
							StartTime: parseTime(t, "2020-03-14T12:01:00Z"),
							EndTime:   parseTime(t, "2020-03-14T12:02:59Z"),
						},
					},
				},
			},
		},
		{
			description: "Empty start bucket",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     3,
			expectedResponse: []backend.CountPeroid{
				{
					Count: 0,
					Start: parseTime(t, "2020-03-12T13:00:00Z"),
					End:   parseTime(t, "2020-03-13T12:59:59Z"),
				},
				{
					Count: 1,
					Start: parseTime(t, "2020-03-13T13:00:00Z"),
					End:   parseTime(t, "2020-03-14T12:59:59Z"),
				},
				{
					Count: 1,
					Start: parseTime(t, "2020-03-14T13:00:00Z"),
					End:   parseTime(t, "2020-03-15T12:59:59Z"),
				},
			},
			nodeSets: []struct {
				node iBackend.Node
				runs []iBackend.Run
			}{
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-14T12:01:00Z"),
							EndTime:   parseTime(t, "2020-03-14T12:02:59Z"),
						},
						{
							StartTime: parseTime(t, "2020-03-14T13:05:00Z"),
							EndTime:   parseTime(t, "2020-03-14T13:06:59Z"),
						},
					},
				},
			},
		},
		{
			description: "Empty center bucket",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     3,
			expectedResponse: []backend.CountPeroid{
				{
					Count: 1,
					Start: parseTime(t, "2020-03-12T13:00:00Z"),
					End:   parseTime(t, "2020-03-13T12:59:59Z"),
				},
				{
					Count: 0,
					Start: parseTime(t, "2020-03-13T13:00:00Z"),
					End:   parseTime(t, "2020-03-14T12:59:59Z"),
				},
				{
					Count: 1,
					Start: parseTime(t, "2020-03-14T13:00:00Z"),
					End:   parseTime(t, "2020-03-15T12:59:59Z"),
				},
			},
			nodeSets: []struct {
				node iBackend.Node
				runs []iBackend.Run
			}{
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-12T13:01:00Z"),
							EndTime:   parseTime(t, "2020-03-12T13:02:59Z"),
						},
						{
							StartTime: parseTime(t, "2020-03-14T13:05:00Z"),
							EndTime:   parseTime(t, "2020-03-14T13:06:59Z"),
						},
					},
				},
			},
		},
		{
			description: "One node checks-in twice in one day",
			now:         parseTime(t, "2020-03-15T01:14:00Z"),
			daysAgo:     1,
			expectedResponse: []backend.CountPeroid{
				{
					Count: 1,
					Start: parseTime(t, "2020-03-14T02:00:00Z"),
					End:   parseTime(t, "2020-03-15T01:59:59Z"),
				},
			},
			nodeSets: []struct {
				node iBackend.Node
				runs []iBackend.Run
			}{
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-14T18:01:00Z"),
							EndTime:   parseTime(t, "2020-03-14T18:02:59Z"),
						},
						{
							StartTime: parseTime(t, "2020-03-14T20:01:00Z"),
							EndTime:   parseTime(t, "2020-03-14T20:02:59Z"),
						},
					},
				},
			},
		},
		{
			description: "Two nodes check-in on the same day",
			now:         parseTime(t, "2020-03-15T11:59:00Z"),
			daysAgo:     1,
			expectedResponse: []backend.CountPeroid{
				{
					Count: 2,
					Start: parseTime(t, "2020-03-14T12:00:00Z"),
					End:   parseTime(t, "2020-03-15T11:59:59Z"),
				},
			},
			nodeSets: []struct {
				node iBackend.Node
				runs []iBackend.Run
			}{
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-14T20:01:00Z"),
							EndTime:   parseTime(t, "2020-03-14T20:02:59Z"),
						},
					},
				},
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-10T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-14T23:01:00Z"),
							EndTime:   parseTime(t, "2020-03-14T23:02:59Z"),
						},
					},
				},
			},
		},
		{
			description: "3 days over daylight savings hour forword",
			now:         parseTime(t, "2020-03-09T12:34:00Z"),
			daysAgo:     3,
			expectedResponse: []backend.CountPeroid{
				{
					Count: 1,
					Start: parseTime(t, "2020-03-06T13:00:00Z"),
					End:   parseTime(t, "2020-03-07T12:59:59Z"),
				},
				{
					Count: 1,
					Start: parseTime(t, "2020-03-07T13:00:00Z"),
					End:   parseTime(t, "2020-03-08T12:59:59Z"),
				},
				{
					Count: 1,
					Start: parseTime(t, "2020-03-08T13:00:00Z"),
					End:   parseTime(t, "2020-03-09T12:59:59Z"),
				},
			},
			nodeSets: []struct {
				node iBackend.Node
				runs []iBackend.Run
			}{
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-03T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-06T13:01:00Z"),
							EndTime:   parseTime(t, "2020-03-06T13:02:59Z"),
						},
						{
							StartTime: parseTime(t, "2020-03-08T12:01:00Z"),
							EndTime:   parseTime(t, "2020-03-08T12:02:59Z"),
						},
						{
							StartTime: parseTime(t, "2020-03-08T13:05:00Z"),
							EndTime:   parseTime(t, "2020-03-08T13:06:59Z"),
						},
					},
				},
			},
		},
		{
			description: "3 days over daylight savings hour back",
			now:         parseTime(t, "2019-11-04T12:34:00Z"),
			daysAgo:     3,
			expectedResponse: []backend.CountPeroid{
				{
					Count: 1,
					Start: parseTime(t, "2019-11-01T13:00:00Z"),
					End:   parseTime(t, "2019-11-02T12:59:59Z"),
				},
				{
					Count: 1,
					Start: parseTime(t, "2019-11-02T13:00:00Z"),
					End:   parseTime(t, "2019-11-03T12:59:59Z"),
				},
				{
					Count: 1,
					Start: parseTime(t, "2019-11-03T13:00:00Z"),
					End:   parseTime(t, "2019-11-04T12:59:59Z"),
				},
			},
			nodeSets: []struct {
				node iBackend.Node
				runs []iBackend.Run
			}{
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2019-11-01T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2019-11-01T13:01:00Z"),
							EndTime:   parseTime(t, "2019-11-01T13:02:59Z"),
						},
						{
							StartTime: parseTime(t, "2019-11-03T12:01:00Z"),
							EndTime:   parseTime(t, "2019-11-03T12:02:59Z"),
						},
						{
							StartTime: parseTime(t, "2019-11-03T13:05:00Z"),
							EndTime:   parseTime(t, "2019-11-03T13:06:59Z"),
						},
					},
				},
			},
		},
		{
			description: "filtering environment",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     3,
			filter: map[string][]string{
				"environment": {"forest"},
			},
			expectedResponse: []backend.CountPeroid{
				{
					Count: 1,
					Start: parseTime(t, "2020-03-12T13:00:00Z"),
					End:   parseTime(t, "2020-03-13T12:59:59Z"),
				},
				{
					Count: 0,
					Start: parseTime(t, "2020-03-13T13:00:00Z"),
					End:   parseTime(t, "2020-03-14T12:59:59Z"),
				},
				{
					Count: 1,
					Start: parseTime(t, "2020-03-14T13:00:00Z"),
					End:   parseTime(t, "2020-03-15T12:59:59Z"),
				},
			},
			nodeSets: []struct {
				node iBackend.Node
				runs []iBackend.Run
			}{
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-01T16:02:59Z"),
						NodeInfo: iBackend.NodeInfo{
							Environment: "forest",
						},
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-12T13:01:00Z"),
							EndTime:   parseTime(t, "2020-03-12T13:02:59Z"),
							NodeInfo: iBackend.NodeInfo{
								Environment: "forest",
							},
						},
						{
							StartTime: parseTime(t, "2020-03-14T12:01:00Z"),
							EndTime:   parseTime(t, "2020-03-14T12:02:59Z"),
							NodeInfo: iBackend.NodeInfo{
								Environment: "desert",
							},
						},
						{
							StartTime: parseTime(t, "2020-03-14T13:05:00Z"),
							EndTime:   parseTime(t, "2020-03-14T13:06:59Z"),
							NodeInfo: iBackend.NodeInfo{
								Environment: "forest",
							},
						},
					},
				},
			},
		},
		{
			description: "Over 10 nodes in one period",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     1,
			expectedResponse: []backend.CountPeroid{
				{
					Count: 11,
					Start: parseTime(t, "2020-03-14T13:00:00Z"),
					End:   parseTime(t, "2020-03-15T12:59:59Z"),
				},
			},
			nodeSets: []struct {
				node iBackend.Node
				runs []iBackend.Run
			}{
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-01T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-14T13:05:00Z"),
							EndTime:   parseTime(t, "2020-03-14T13:06:59Z"),
						},
					},
				},
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-02T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-15T01:05:00Z"),
							EndTime:   parseTime(t, "2020-03-15T01:06:59Z"),
						},
					},
				},
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-02T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-15T02:05:00Z"),
							EndTime:   parseTime(t, "2020-03-15T02:06:59Z"),
						},
					},
				},

				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-02T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-15T02:15:00Z"),
							EndTime:   parseTime(t, "2020-03-15T02:16:59Z"),
						},
					},
				},
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-02T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-15T02:25:00Z"),
							EndTime:   parseTime(t, "2020-03-15T02:26:59Z"),
						},
					},
				},
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-02T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-15T02:35:00Z"),
							EndTime:   parseTime(t, "2020-03-15T02:36:59Z"),
						},
					},
				},
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-02T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-15T02:45:00Z"),
							EndTime:   parseTime(t, "2020-03-15T02:46:59Z"),
						},
					},
				},
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-02T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-15T02:55:00Z"),
							EndTime:   parseTime(t, "2020-03-15T02:56:59Z"),
						},
					},
				},
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-02T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-15T03:05:00Z"),
							EndTime:   parseTime(t, "2020-03-15T03:06:59Z"),
						},
					},
				},
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-02T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-15T03:15:00Z"),
							EndTime:   parseTime(t, "2020-03-15T03:16:59Z"),
						},
					},
				},
				{
					node: iBackend.Node{
						Checkin: parseTime(t, "2020-03-02T16:02:59Z"),
					},
					runs: []iBackend.Run{
						{
							StartTime: parseTime(t, "2020-03-15T03:25:00Z"),
							EndTime:   parseTime(t, "2020-03-15T03:26:59Z"),
						},
					},
				},
			},
		},
	}

	for _, testCase := range cases {
		t.Run(testCase.description, func(t *testing.T) {
			nodes := make([]iBackend.Node, len(testCase.nodeSets))
			runs := make([]iBackend.Run, 0)
			// Adding required node data
			for index := range testCase.nodeSets {
				nodeID := newUUID()
				testCase.nodeSets[index].node.Exists = true
				testCase.nodeSets[index].node.NodeInfo.EntityUuid = nodeID
				testCase.nodeSets[index].node.NodeName = strconv.Itoa(index)
				nodes = append(nodes, testCase.nodeSets[index].node)
				for runIndex := range testCase.nodeSets[index].runs {
					runID := newUUID()
					testCase.nodeSets[index].runs[runIndex].EntityUuid = nodeID
					testCase.nodeSets[index].runs[runIndex].RunID = runID
					runs = append(runs, testCase.nodeSets[index].runs[runIndex])
				}
			}

			suite.IngestNodes(nodes)
			suite.IngestRuns(runs)
			defer suite.DeleteAllDocuments()

			endTime := time.Date(testCase.now.Year(), testCase.now.Month(), testCase.now.Day(),
				testCase.now.Hour(), 0, 0, 0, time.UTC).Add(time.Hour)

			startTime := endTime.Add(-time.Hour * 24 * time.Duration(testCase.daysAgo))

			actualResponse, err := esBackend.GetCheckinCountsTimeSeries(startTime,
				endTime.Add(-time.Millisecond), testCase.filter)
			require.NoError(t, err)

			assert.Equal(t, len(testCase.expectedResponse), len(actualResponse))
			for index := range actualResponse {
				assert.Equal(t, testCase.expectedResponse[index].Start.Format(time.RFC3339),
					actualResponse[index].Start.Format(time.RFC3339))
				assert.Equal(t, testCase.expectedResponse[index].End.Format(time.RFC3339),
					actualResponse[index].End.Format(time.RFC3339))
				assert.Equal(t, testCase.expectedResponse[index].Count,
					actualResponse[index].Count)
			}
		})
	}
}

func TestTimeSeriesDeletedNodes(t *testing.T) {
	cases := []struct {
		description      string
		filter           map[string][]string
		nodes            []iBackend.Node
		datetime         time.Time
		daysAgo          int
		expectedResponse []backend.CountPeroid
	}{
		{
			description: "Three daily buckets with one node deleted each day",
			datetime:    parseTime(t, "2020-03-15T20:02:59Z"),
			daysAgo:     3,
			nodes: []iBackend.Node{
				{
					Exists: false,
					NodeInfo: iBackend.NodeInfo{
						Timestamp: parseTime(t, "2020-03-15T16:02:59Z"),
					},
				},
				{
					Exists: false,
					NodeInfo: iBackend.NodeInfo{
						Timestamp: parseTime(t, "2020-03-14T16:02:59Z"),
					},
				},
				{
					Exists: false,
					NodeInfo: iBackend.NodeInfo{
						Timestamp: parseTime(t, "2020-03-13T16:02:59Z"),
					},
				},
			},
			expectedResponse: []backend.CountPeroid{
				{
					Start: parseTime(t, "2020-03-12T21:00:00Z"),
					End:   parseTime(t, "2020-03-13T20:59:59Z"),
					Count: 1,
				},
				{
					Start: parseTime(t, "2020-03-13T21:00:00Z"),
					End:   parseTime(t, "2020-03-14T20:59:59Z"),
					Count: 2,
				},
				{
					Start: parseTime(t, "2020-03-14T21:00:00Z"),
					End:   parseTime(t, "2020-03-15T20:59:59Z"),
					Count: 3,
				},
			},
		},
		{
			description: "All but one node is deleted",
			datetime:    parseTime(t, "2020-03-15T20:02:59Z"),
			daysAgo:     3,
			nodes: []iBackend.Node{
				{
					Exists: false,
					NodeInfo: iBackend.NodeInfo{
						Timestamp: parseTime(t, "2020-03-15T16:02:59Z"),
					},
				},
				{
					Exists: true,
					NodeInfo: iBackend.NodeInfo{
						Timestamp: parseTime(t, "2020-03-14T16:02:59Z"),
					},
				},
				{
					Exists: false,
					NodeInfo: iBackend.NodeInfo{
						Timestamp: parseTime(t, "2020-03-13T16:02:59Z"),
					},
				},
			},
			expectedResponse: []backend.CountPeroid{
				{
					Start: parseTime(t, "2020-03-12T21:00:00Z"),
					End:   parseTime(t, "2020-03-13T20:59:59Z"),
					Count: 1,
				},
				{
					Start: parseTime(t, "2020-03-13T21:00:00Z"),
					End:   parseTime(t, "2020-03-14T20:59:59Z"),
					Count: 1,
				},
				{
					Start: parseTime(t, "2020-03-14T21:00:00Z"),
					End:   parseTime(t, "2020-03-15T20:59:59Z"),
					Count: 2,
				},
			},
		},
		{
			description: "All nodes deleted in last 24 hours",
			datetime:    parseTime(t, "2020-03-15T20:02:59Z"),
			daysAgo:     3,
			nodes: []iBackend.Node{
				{
					Exists: false,
					NodeInfo: iBackend.NodeInfo{
						Timestamp: parseTime(t, "2020-03-15T16:02:59Z"),
					},
				},
				{
					Exists: false,
					NodeInfo: iBackend.NodeInfo{
						Timestamp: parseTime(t, "2020-03-15T16:12:59Z"),
					},
				},
				{
					Exists: false,
					NodeInfo: iBackend.NodeInfo{
						Timestamp: parseTime(t, "2020-03-15T16:22:59Z"),
					},
				},
			},
			expectedResponse: []backend.CountPeroid{
				{
					Start: parseTime(t, "2020-03-12T21:00:00Z"),
					End:   parseTime(t, "2020-03-13T20:59:59Z"),
					Count: 0,
				},
				{
					Start: parseTime(t, "2020-03-13T21:00:00Z"),
					End:   parseTime(t, "2020-03-14T20:59:59Z"),
					Count: 0,
				},
				{
					Start: parseTime(t, "2020-03-14T21:00:00Z"),
					End:   parseTime(t, "2020-03-15T20:59:59Z"),
					Count: 3,
				},
			},
		},
		{
			description: "All nodes deleted before time series",
			datetime:    parseTime(t, "2020-03-15T20:02:59Z"),
			daysAgo:     3,
			nodes: []iBackend.Node{
				{
					Exists: false,
					NodeInfo: iBackend.NodeInfo{
						Timestamp: parseTime(t, "2020-01-15T16:02:59Z"),
					},
				},
				{
					Exists: false,
					NodeInfo: iBackend.NodeInfo{
						Timestamp: parseTime(t, "2020-01-15T16:12:59Z"),
					},
				},
				{
					Exists: false,
					NodeInfo: iBackend.NodeInfo{
						Timestamp: parseTime(t, "2020-01-15T16:22:59Z"),
					},
				},
			},
			expectedResponse: []backend.CountPeroid{
				{
					Start: parseTime(t, "2020-03-12T21:00:00Z"),
					End:   parseTime(t, "2020-03-13T20:59:59Z"),
					Count: 3,
				},
				{
					Start: parseTime(t, "2020-03-13T21:00:00Z"),
					End:   parseTime(t, "2020-03-14T20:59:59Z"),
					Count: 3,
				},
				{
					Start: parseTime(t, "2020-03-14T21:00:00Z"),
					End:   parseTime(t, "2020-03-15T20:59:59Z"),
					Count: 3,
				},
			},
		},
	}

	for _, testCase := range cases {
		t.Run(testCase.description, func(t *testing.T) {
			// Adding required nodes
			for index := range testCase.nodes {
				nodeID := newUUID()
				testCase.nodes[index].NodeInfo.EntityUuid = nodeID
				testCase.nodes[index].NodeName = strconv.Itoa(index)
			}

			suite.IngestNodes(testCase.nodes)
			defer suite.DeleteAllDocuments()

			endTime := time.Date(testCase.datetime.Year(), testCase.datetime.Month(), testCase.datetime.Day(),
				testCase.datetime.Hour(), 0, 0, 0, time.UTC).Add(time.Hour)

			startTime := endTime.Add(-time.Hour * 24 * time.Duration(testCase.daysAgo))

			if testCase.filter == nil {
				testCase.filter = map[string][]string{}
			}
			actualResponse, err := esBackend.GetDeletedCountsTimeSeries(startTime,
				endTime.Add(-time.Millisecond), testCase.filter)
			require.NoError(t, err)

			assert.Equal(t, len(testCase.expectedResponse), len(actualResponse))
			for index := range actualResponse {
				assert.Equal(t, testCase.expectedResponse[index].Start.Format(time.RFC3339),
					actualResponse[index].Start.Format(time.RFC3339))
				assert.Equal(t, testCase.expectedResponse[index].End.Format(time.RFC3339),
					actualResponse[index].End.Format(time.RFC3339))
				assert.Equal(t, testCase.expectedResponse[index].Count,
					actualResponse[index].Count)
			}
		})
	}
}

func TestTimeSeriesCreatedNodes(t *testing.T) {
	cases := []struct {
		description      string
		filter           map[string][]string
		nodes            []iBackend.UpsertNode
		datetime         time.Time
		daysAgo          int
		expectedResponse []backend.CountPeroid
	}{
		{
			description: "Three daily buckets with one node created each day",
			datetime:    parseTime(t, "2020-03-15T20:02:59Z"),
			daysAgo:     3,
			nodes: []iBackend.UpsertNode{
				{
					Created: parseTime(t, "2020-03-15T16:02:59Z"),
				},
				{
					Created: parseTime(t, "2020-03-14T16:02:59Z"),
				},
				{
					Created: parseTime(t, "2020-03-13T16:02:59Z"),
				},
			},
			expectedResponse: []backend.CountPeroid{
				{
					Start: parseTime(t, "2020-03-12T21:00:00Z"),
					End:   parseTime(t, "2020-03-13T20:59:59Z"),
					Count: 1,
				},
				{
					Start: parseTime(t, "2020-03-13T21:00:00Z"),
					End:   parseTime(t, "2020-03-14T20:59:59Z"),
					Count: 2,
				},
				{
					Start: parseTime(t, "2020-03-14T21:00:00Z"),
					End:   parseTime(t, "2020-03-15T20:59:59Z"),
					Count: 3,
				},
			},
		},
		{
			description: "All nodes created in last 24 hours",
			datetime:    parseTime(t, "2020-03-15T20:02:59Z"),
			daysAgo:     3,
			nodes: []iBackend.UpsertNode{
				{
					Created: parseTime(t, "2020-03-15T16:02:59Z"),
				},
				{
					Created: parseTime(t, "2020-03-15T16:12:59Z"),
				},
				{
					Created: parseTime(t, "2020-03-15T16:22:59Z"),
				},
			},
			expectedResponse: []backend.CountPeroid{
				{
					Start: parseTime(t, "2020-03-12T21:00:00Z"),
					End:   parseTime(t, "2020-03-13T20:59:59Z"),
					Count: 0,
				},
				{
					Start: parseTime(t, "2020-03-13T21:00:00Z"),
					End:   parseTime(t, "2020-03-14T20:59:59Z"),
					Count: 0,
				},
				{
					Start: parseTime(t, "2020-03-14T21:00:00Z"),
					End:   parseTime(t, "2020-03-15T20:59:59Z"),
					Count: 3,
				},
			},
		},
		{
			description: "All nodes created before time series",
			datetime:    parseTime(t, "2020-03-15T20:02:59Z"),
			daysAgo:     3,
			nodes: []iBackend.UpsertNode{
				{
					Created: parseTime(t, "2020-01-15T16:02:59Z"),
				},
				{
					Created: parseTime(t, "2020-01-15T16:12:59Z"),
				},
				{
					Created: parseTime(t, "2020-01-15T16:22:59Z"),
				},
			},
			expectedResponse: []backend.CountPeroid{
				{
					Start: parseTime(t, "2020-03-12T21:00:00Z"),
					End:   parseTime(t, "2020-03-13T20:59:59Z"),
					Count: 3,
				},
				{
					Start: parseTime(t, "2020-03-13T21:00:00Z"),
					End:   parseTime(t, "2020-03-14T20:59:59Z"),
					Count: 3,
				},
				{
					Start: parseTime(t, "2020-03-14T21:00:00Z"),
					End:   parseTime(t, "2020-03-15T20:59:59Z"),
					Count: 3,
				},
			},
		},
	}

	for _, testCase := range cases {
		t.Run(testCase.description, func(t *testing.T) {
			// Adding required nodes
			for index := range testCase.nodes {
				nodeID := newUUID()
				testCase.nodes[index].NodeInfo.EntityUuid = nodeID
				testCase.nodes[index].NodeName = strconv.Itoa(index)
			}

			suite.IngestInitialNodes(testCase.nodes)
			defer suite.DeleteAllDocuments()

			endTime := time.Date(testCase.datetime.Year(), testCase.datetime.Month(), testCase.datetime.Day(),
				testCase.datetime.Hour(), 0, 0, 0, time.UTC).Add(time.Hour)

			startTime := endTime.Add(-time.Hour * 24 * time.Duration(testCase.daysAgo))

			if testCase.filter == nil {
				testCase.filter = map[string][]string{}
			}
			actualResponse, err := esBackend.GetCreateCountsTimeSeries(startTime,
				endTime.Add(-time.Millisecond), testCase.filter)
			require.NoError(t, err)

			assert.Equal(t, len(testCase.expectedResponse), len(actualResponse))
			for index := range actualResponse {
				assert.Equal(t, testCase.expectedResponse[index].Start.Format(time.RFC3339),
					actualResponse[index].Start.Format(time.RFC3339))
				assert.Equal(t, testCase.expectedResponse[index].End.Format(time.RFC3339),
					actualResponse[index].End.Format(time.RFC3339))
				assert.Equal(t, testCase.expectedResponse[index].Count,
					actualResponse[index].Count)
			}
		})
	}
}

func parseTime(t *testing.T, timeString string) time.Time {
	parsedTime, err := time.Parse(time.RFC3339, timeString)
	require.NoError(t, err)

	return parsedTime
}
