package integration_test

import (
	"testing"
	"time"

	"github.com/chef/automate/components/config-mgmt-service/backend"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/stretchr/testify/require"
)

func TestGetNodeDailyStatusTimeSeries(t *testing.T) {

	cases := []struct {
		now              time.Time
		description      string
		daysAgo          int
		node             iBackend.Node
		runs             []iBackend.Run
		expectedResponse []backend.CountPeroid
	}{
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
			node: iBackend.Node{
				Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
			},
			runs: []iBackend.Run{
				{
					StartTime: parseTime(t, "2020-03-12T13:01:00Z"),
					EndTime:   parseTime(t, "2020-03-12T13:02:59Z"),
					RunID:     "1",
					NodeInfo: iBackend.NodeInfo{
						Status: "failure",
					},
				},
				{
					StartTime: parseTime(t, "2020-03-12T14:01:00Z"),
					EndTime:   parseTime(t, "2020-03-12T14:02:59Z"),
					RunID:     "2",
					NodeInfo: iBackend.NodeInfo{
						Status: "failure",
					},
				},
				{
					StartTime: parseTime(t, "2020-03-14T12:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T12:02:59Z"),
					RunID:     "4",
					NodeInfo: iBackend.NodeInfo{
						Status: "successful",
					},
				},
				{
					StartTime: parseTime(t, "2020-03-14T11:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T11:02:59Z"),
					RunID:     "3",
					NodeInfo: iBackend.NodeInfo{
						Status: "failure",
					},
				},
				{
					StartTime: parseTime(t, "2020-03-14T14:05:00Z"),
					EndTime:   parseTime(t, "2020-03-14T14:06:59Z"),
					RunID:     "6",
					NodeInfo: iBackend.NodeInfo{
						Status: "successful",
					},
				},
				{
					StartTime: parseTime(t, "2020-03-14T13:05:00Z"),
					EndTime:   parseTime(t, "2020-03-14T13:06:59Z"),
					RunID:     "5",
					NodeInfo: iBackend.NodeInfo{
						Status: "failure",
					},
				},
			},
		},
	}

	for _, testCase := range cases {
		t.Run(testCase.description, func(t *testing.T) {

			nodeID := newUUID()
			testCase.node.Exists = true
			testCase.node.NodeInfo.EntityUuid = nodeID
			testCase.node.NodeName = "nodeDailyStatusTimeSeries"
			for runIndex := range testCase.runs {
				// runID := newUUID()
				testCase.runs[runIndex].EntityUuid = nodeID
				// testCase.runs[runIndex].RunID = runID
			}

			suite.IngestNodes([]iBackend.Node{testCase.node})
			suite.IngestRuns(testCase.runs)
			defer suite.DeleteAllDocuments()

			endTime := time.Date(testCase.now.Year(), testCase.now.Month(), testCase.now.Day(),
				testCase.now.Hour(), 0, 0, 0, time.UTC).Add(time.Hour)

			startTime := endTime.Add(-time.Hour * 24 * time.Duration(testCase.daysAgo))

			_, err := esBackend.GetNodeDailyStatusTimeSeries(nodeID, startTime,
				endTime.Add(-time.Millisecond))
			require.NoError(t, err)

			t.Fail()
		})
	}
}
