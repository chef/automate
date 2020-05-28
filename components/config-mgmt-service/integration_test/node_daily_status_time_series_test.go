package integration_test

import (
	"context"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/chef/automate/components/config-mgmt-service/backend"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestGetNodeDailyStatusTimeSeriesBackend(t *testing.T) {

	cases := []struct {
		now              time.Time
		description      string
		daysAgo          int
		node             iBackend.Node
		runs             []iBackend.Run
		expectedResponse []backend.RunDurationStatus
	}{
		{
			description: "typical request",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     3,
			expectedResponse: []backend.RunDurationStatus{
				{
					Status: "failure",
					RunID:  "2",
					Start:  parseTime(t, "2020-03-12T13:00:00Z"),
					End:    parseTime(t, "2020-03-13T12:59:59.999Z"),
				},
				{
					Status: "failure",
					RunID:  "3",
					Start:  parseTime(t, "2020-03-13T13:00:00Z"),
					End:    parseTime(t, "2020-03-14T12:59:59.999Z"),
				},
				{
					Status: "failure",
					RunID:  "5",
					Start:  parseTime(t, "2020-03-14T13:00:00Z"),
					End:    parseTime(t, "2020-03-15T12:59:59.999Z"),
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
						Status: "success",
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
						Status: "success",
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
		{
			description: "Missing duration",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     1,
			expectedResponse: []backend.RunDurationStatus{
				{
					Status: "missing",
					RunID:  "",
					Start:  parseTime(t, "2020-03-14T13:00:00Z"),
					End:    parseTime(t, "2020-03-15T12:59:59.999Z"),
				},
			},
			node: iBackend.Node{
				Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
			},
			runs: []iBackend.Run{
				{ // not within the last 24 hours
					StartTime: parseTime(t, "2020-03-12T13:01:00Z"),
					EndTime:   parseTime(t, "2020-03-12T13:02:59Z"),
					RunID:     "1",
					NodeInfo: iBackend.NodeInfo{
						Status: "failure",
					},
				},
			},
		},
		{
			description: "Request 3 days but no node runs exist",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     3,
			expectedResponse: []backend.RunDurationStatus{
				{
					Status: "missing",
					RunID:  "",
					Start:  parseTime(t, "2020-03-12T13:00:00Z"),
					End:    parseTime(t, "2020-03-13T12:59:59.999Z"),
				},
				{
					Status: "missing",
					RunID:  "",
					Start:  parseTime(t, "2020-03-13T13:00:00Z"),
					End:    parseTime(t, "2020-03-14T12:59:59.999Z"),
				},
				{
					Status: "missing",
					RunID:  "",
					Start:  parseTime(t, "2020-03-14T13:00:00Z"),
					End:    parseTime(t, "2020-03-15T12:59:59.999Z"),
				},
			},
			runs: []iBackend.Run{},
		},
		{
			description: "Missing middle duration of non-missing durations",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     3,
			expectedResponse: []backend.RunDurationStatus{
				{
					Status: "failure",
					RunID:  "1",
					Start:  parseTime(t, "2020-03-12T13:00:00Z"),
					End:    parseTime(t, "2020-03-13T12:59:59.999Z"),
				},
				{
					Status: "missing",
					RunID:  "",
					Start:  parseTime(t, "2020-03-13T13:00:00Z"),
					End:    parseTime(t, "2020-03-14T12:59:59.999Z"),
				},
				{
					Status: "failure",
					RunID:  "5",
					Start:  parseTime(t, "2020-03-14T13:00:00Z"),
					End:    parseTime(t, "2020-03-15T12:59:59.999Z"),
				},
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
					StartTime: parseTime(t, "2020-03-14T15:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T15:02:59Z"),
					RunID:     "5",
					NodeInfo: iBackend.NodeInfo{
						Status: "failure",
					},
				},
			},
		},
		{
			description: "Choose the newest successful run with no failure runs",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     1,
			expectedResponse: []backend.RunDurationStatus{
				{
					Status: "success",
					RunID:  "88",
					Start:  parseTime(t, "2020-03-14T13:00:00Z"),
					End:    parseTime(t, "2020-03-15T12:59:59.999Z"),
				},
			},
			node: iBackend.Node{
				Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
			},
			runs: []iBackend.Run{
				{
					StartTime: parseTime(t, "2020-03-14T13:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T13:02:59Z"),
					RunID:     "77",
					NodeInfo: iBackend.NodeInfo{
						Status: "success",
					},
				},
				{
					StartTime: parseTime(t, "2020-03-14T17:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T17:02:59Z"),
					RunID:     "4898",
					NodeInfo: iBackend.NodeInfo{
						Status: "success",
					},
				},
				{ // chosen
					StartTime: parseTime(t, "2020-03-14T20:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T20:02:59Z"),
					RunID:     "88",
					NodeInfo: iBackend.NodeInfo{
						Status: "success",
					},
				},
			},
		},
		{
			description: "Choose the older failure run with multiple newer successful runs",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     1,
			expectedResponse: []backend.RunDurationStatus{
				{
					Status: "failure",
					RunID:  "77",
					Start:  parseTime(t, "2020-03-14T13:00:00Z"),
					End:    parseTime(t, "2020-03-15T12:59:59.999Z"),
				},
			},
			node: iBackend.Node{
				Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
			},
			runs: []iBackend.Run{
				{ // chosen
					StartTime: parseTime(t, "2020-03-14T13:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T13:02:59Z"),
					RunID:     "77",
					NodeInfo: iBackend.NodeInfo{
						Status: "failure",
					},
				},
				{
					StartTime: parseTime(t, "2020-03-14T17:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T17:02:59Z"),
					RunID:     "4898",
					NodeInfo: iBackend.NodeInfo{
						Status: "success",
					},
				},
				{
					StartTime: parseTime(t, "2020-03-14T20:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T20:02:59Z"),
					RunID:     "88",
					NodeInfo: iBackend.NodeInfo{
						Status: "success",
					},
				},
			},
		},
		{
			description: "Choose the failure run sandwiched between multiple successful runs",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     1,
			expectedResponse: []backend.RunDurationStatus{
				{
					Status: "failure",
					RunID:  "77",
					Start:  parseTime(t, "2020-03-14T13:00:00Z"),
					End:    parseTime(t, "2020-03-15T12:59:59.999Z"),
				},
			},
			node: iBackend.Node{
				Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
			},
			runs: []iBackend.Run{
				{ // chosen
					StartTime: parseTime(t, "2020-03-14T14:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T14:02:59Z"),
					RunID:     "77",
					NodeInfo: iBackend.NodeInfo{
						Status: "failure",
					},
				},
				{
					StartTime: parseTime(t, "2020-03-14T17:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T17:02:59Z"),
					RunID:     "4898",
					NodeInfo: iBackend.NodeInfo{
						Status: "success",
					},
				},
				{
					StartTime: parseTime(t, "2020-03-14T20:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T20:02:59Z"),
					RunID:     "88",
					NodeInfo: iBackend.NodeInfo{
						Status: "success",
					},
				},
				{ // oldest
					StartTime: parseTime(t, "2020-03-14T13:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T13:02:59Z"),
					RunID:     "12345",
					NodeInfo: iBackend.NodeInfo{
						Status: "success",
					},
				},
			},
		},
		{
			description: "Choose newest successful run with no failure runs",
			now:         parseTime(t, "2020-03-15T12:34:00Z"),
			daysAgo:     1,
			expectedResponse: []backend.RunDurationStatus{
				{
					Status: "success",
					RunID:  "88",
					Start:  parseTime(t, "2020-03-14T13:00:00Z"),
					End:    parseTime(t, "2020-03-15T12:59:59.999Z"),
				},
			},
			node: iBackend.Node{
				Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
			},
			runs: []iBackend.Run{
				{
					StartTime: parseTime(t, "2020-03-14T13:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T13:02:59Z"),
					RunID:     "77",
					NodeInfo: iBackend.NodeInfo{
						Status: "success",
					},
				},
				{
					StartTime: parseTime(t, "2020-03-14T17:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T17:02:59Z"),
					RunID:     "4898",
					NodeInfo: iBackend.NodeInfo{
						Status: "success",
					},
				},
				{ // chosen
					StartTime: parseTime(t, "2020-03-14T20:01:00Z"),
					EndTime:   parseTime(t, "2020-03-14T20:02:59Z"),
					RunID:     "88",
					NodeInfo: iBackend.NodeInfo{
						Status: "success",
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
				testCase.runs[runIndex].EntityUuid = nodeID
			}

			suite.IngestNodes([]iBackend.Node{testCase.node})
			suite.IngestRuns(testCase.runs)
			defer suite.DeleteAllDocuments()

			endTime := time.Date(testCase.now.Year(), testCase.now.Month(), testCase.now.Day(),
				testCase.now.Hour(), 0, 0, 0, time.UTC).Add(time.Hour)

			startTime := endTime.Add(-time.Hour * 24 * time.Duration(testCase.daysAgo))

			actualResponse, err := esBackend.GetNodeRunsDailyStatusTimeSeries(nodeID, startTime,
				endTime.Add(-time.Millisecond))
			require.NoError(t, err)

			assert.Equal(t, len(testCase.expectedResponse), len(actualResponse))
			for index, expected := range testCase.expectedResponse {
				actualRunDurationStatus := actualResponse[index]

				assert.Equal(t, expected.Start, actualRunDurationStatus.Start)
				assert.Equal(t, expected.End, actualRunDurationStatus.End)
				assert.Equal(t, expected.RunID, actualRunDurationStatus.RunID)
				assert.Equal(t, expected.Status, actualRunDurationStatus.Status)
			}
		})
	}
}

func TestGetNodeDailyStatusTimeSeriesRPC(t *testing.T) {

	now := time.Now()
	cases := []struct {
		description      string
		request          *request.NodeRunsDailyStatusTimeSeries
		node             iBackend.Node
		runs             []iBackend.Run
		expectedResponse *response.NodeRunsDailyStatusTimeSeries
		expectedFailure  bool
	}{
		{
			description:     "Fails: Missing node ID",
			expectedFailure: true,
			request: &request.NodeRunsDailyStatusTimeSeries{
				NodeId:  "",
				DaysAgo: 1,
			},
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: "123456",
				},
				Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
			},
		},
		{
			description:     "Fails: Missing node ID",
			expectedFailure: true,
			request: &request.NodeRunsDailyStatusTimeSeries{
				NodeId:  "123456",
				DaysAgo: -1,
			},
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: "123456",
				},
				Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
			},
		},
		{
			description: "Return one 24-hour duration",
			request: &request.NodeRunsDailyStatusTimeSeries{
				NodeId:  "123456",
				DaysAgo: 1,
			},
			expectedResponse: &response.NodeRunsDailyStatusTimeSeries{
				Durations: []*response.RunDurationStatus{
					{
						Status: "missing",
						RunId:  "",
						Start: time.Date(now.Year(), now.Month(), now.Day(),
							now.Hour(), 0, 0, 0, time.UTC).Add(time.Hour).Add(-time.Hour * 24).Format(time.RFC3339),
						End: time.Date(now.Year(), now.Month(), now.Day(),
							now.Hour(), 0, 0, 0, time.UTC).Add(time.Hour).Add(-time.Millisecond).Format(time.RFC3339),
					},
				},
			},
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: "123456",
				},
				Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
			},
		},
		{
			description: "Return three 24-hour durations",
			request: &request.NodeRunsDailyStatusTimeSeries{
				NodeId:  "123456",
				DaysAgo: 3,
			},
			expectedResponse: &response.NodeRunsDailyStatusTimeSeries{
				Durations: []*response.RunDurationStatus{
					{
						Status: "missing",
						RunId:  "",
						Start: time.Date(now.Year(), now.Month(), now.Day(),
							now.Hour(), 0, 0, 0, time.UTC).Add(time.Hour).Add(-time.Hour * 24 * 3).Format(time.RFC3339),
						End: time.Date(now.Year(), now.Month(), now.Day(),
							now.Hour(), 0, 0, 0, time.UTC).Add(time.Hour).Add(-time.Hour * 24 * 2).Add(-time.Millisecond).Format(time.RFC3339),
					},
					{
						Status: "missing",
						RunId:  "",
						Start: time.Date(now.Year(), now.Month(), now.Day(),
							now.Hour(), 0, 0, 0, time.UTC).Add(time.Hour).Add(-time.Hour * 24 * 2).Format(time.RFC3339),
						End: time.Date(now.Year(), now.Month(), now.Day(),
							now.Hour(), 0, 0, 0, time.UTC).Add(time.Hour).Add(-time.Hour * 24 * 1).Add(-time.Millisecond).Format(time.RFC3339),
					},
					{
						Status: "missing",
						RunId:  "",
						Start: time.Date(now.Year(), now.Month(), now.Day(),
							now.Hour(), 0, 0, 0, time.UTC).Add(time.Hour).Add(-time.Hour * 24).Format(time.RFC3339),
						End: time.Date(now.Year(), now.Month(), now.Day(),
							now.Hour(), 0, 0, 0, time.UTC).Add(time.Hour).Add(-time.Millisecond).Format(time.RFC3339),
					},
				},
			},
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: "123456",
				},
				Checkin: parseTime(t, "2020-03-12T16:02:59Z"),
			},
		},
	}

	for _, testCase := range cases {
		t.Run(testCase.description, func(t *testing.T) {

			testCase.node.Exists = true
			testCase.node.NodeName = "nodeDailyStatusTimeSeries"
			for runIndex := range testCase.runs {
				testCase.runs[runIndex].EntityUuid = testCase.node.NodeInfo.EntityUuid
			}

			suite.IngestNodes([]iBackend.Node{testCase.node})
			suite.IngestRuns(testCase.runs)
			defer suite.DeleteAllDocuments()

			actualResponse, err := cfgmgmt.GetNodeRunsDailyStatusTimeSeries(context.Background(),
				testCase.request)
			if testCase.expectedFailure {
				assert.Error(t, err)
				return
			}
			require.NoError(t, err)

			require.Equal(t, len(testCase.expectedResponse.Durations), len(actualResponse.Durations))
			for index, expectedDuration := range testCase.expectedResponse.Durations {
				actualRunDurationStatus := actualResponse.Durations[index]

				assert.Equal(t, expectedDuration.Start, actualRunDurationStatus.Start)
				assert.Equal(t, expectedDuration.End, actualRunDurationStatus.End)
				assert.Equal(t, expectedDuration.RunId, actualRunDurationStatus.RunId)
				assert.Equal(t, expectedDuration.Status, actualRunDurationStatus.Status)
			}
		})
	}
}
