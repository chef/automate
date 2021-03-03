package integration_test

import (
	"context"
	"strconv"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestMissingNodeRangeCounts(t *testing.T) {
	cases := []struct {
		description      string
		nodes            []iBackend.Node
		durations        []string
		expectedResponse []*response.CountedDuration
	}{
		{
			description: "normal case",
			nodes: []iBackend.Node{ // need 12 of these
				{
					// checked in within the three days. Not counted in any bucket
					Checkin: time.Now().Add(time.Hour * -28),
				},
				{
					// checked in within 1 week but not three days. Counted in the 3d bucket only
					Checkin: time.Now().AddDate(0, 0, -3).Add(time.Hour * -2),
				},
				{
					// checked in within 1 week but not three days. Counted in the 3d bucket only
					Checkin: time.Now().AddDate(0, 0, -4),
				},
				{
					// checked in within 1 week but not three days. Counted in the 3d bucket only
					Checkin: time.Now().AddDate(0, 0, -5),
				},
				{
					// checked in within 2 weeks but not 1 week. Counted in the 3d, and 1w buckets only
					Checkin: time.Now().AddDate(0, 0, -8),
				},
				{
					// checked in within 2 weeks but not 1 week. Counted in the 3d, and 1w buckets only
					Checkin: time.Now().AddDate(0, 0, -9),
				},
				{
					// checked in within 1 month but not 2 week. Counted in the 3d, 1w, and 2w buckets only
					Checkin: time.Now().AddDate(0, 0, -20),
				},
				{
					// checked in within 1 month but not 2 week. Counted in the 3d, 1w, and 2w buckets only
					Checkin: time.Now().AddDate(0, 0, -21),
				},
				{
					// checked in within 1 month but not 2 week. Counted in the 3d, 1w, and 2w buckets only
					Checkin: time.Now().AddDate(0, 0, -25),
				},
				{
					// checked in within 1 month but not 2 week. Counted in the 3d, 1w, and 2w buckets only
					// Keep this under the number of days of ANY month, i.e. strictly less than 28 days!
					Checkin: time.Now().AddDate(0, 0, -27),
				},
				{
					// checked in within 3 month but not 1 month. Counted in the 3d, 1w, 2w, 1m buckets only
					Checkin: time.Now().AddDate(0, 0, -40),
				},
				{
					// has not checked in within 3 month. Counted in all buckets
					Checkin: time.Now().AddDate(0, 0, -100),
				},
			},
			durations: []string{"3d", "1w", "2w", "1M", "3M"},
			expectedResponse: []*response.CountedDuration{
				{
					Duration: "3d",
					Count:    11,
				},
				{
					Duration: "1w",
					Count:    8,
				},
				{
					Duration: "2w",
					Count:    6,
				},
				{
					Duration: "1M",
					Count:    2,
				},
				{
					Duration: "3M",
					Count:    1,
				},
			},
		},
		{
			description: "days: no nodes are missing after 3 days",
			nodes: []iBackend.Node{
				{
					Checkin: time.Now().Add(time.Hour*-24*3 + time.Hour*2),
				},
			},
			durations: []string{"3d"},
			expectedResponse: []*response.CountedDuration{
				{
					Duration: "3d",
					Count:    0,
				},
			},
		},
		{
			description: "days: one node is missing after 2 days",
			nodes: []iBackend.Node{
				{
					Checkin: time.Now().Add(time.Hour*-24*3 + time.Hour*-2),
				},
			},
			durations: []string{"3d"},
			expectedResponse: []*response.CountedDuration{
				{
					Duration: "3d",
					Count:    1,
				},
			},
		},
		{
			description: "weeks: no nodes are missing after 2 weeks",
			nodes: []iBackend.Node{
				{
					Checkin: time.Now().AddDate(0, 0, -14).Add(time.Hour * 2),
				},
			},
			durations: []string{"2w"},
			expectedResponse: []*response.CountedDuration{
				{
					Duration: "2w",
					Count:    0,
				},
			},
		},
		{
			description: "weeks: one node is missing after 2 weeks",
			nodes: []iBackend.Node{
				{
					Checkin: time.Now().AddDate(0, 0, -14).Add(time.Hour * -2),
				},
			},
			durations: []string{"2w"},
			expectedResponse: []*response.CountedDuration{
				{
					Duration: "2w",
					Count:    1,
				},
			},
		},
		{
			description: "months: no nodes are missing after 2 months",
			nodes: []iBackend.Node{
				{
					Checkin: time.Now().AddDate(0, -2, 3),
				},
			},
			durations: []string{"2M"},
			expectedResponse: []*response.CountedDuration{
				{
					Duration: "2M",
					Count:    0,
				},
			},
		},
		{
			description: "months: one node is missing after 2 months",
			nodes: []iBackend.Node{
				{
					Checkin: time.Now().AddDate(0, -2, -3),
				},
			},
			durations: []string{"2M"},
			expectedResponse: []*response.CountedDuration{
				{
					Duration: "2M",
					Count:    1,
				},
			},
		},
		{
			description: "returned order same as requested order ascending",
			nodes: []iBackend.Node{
				{
					Checkin: time.Now().AddDate(0, -4, 0),
				},
			},
			durations: []string{"3d", "1w", "2w", "1M", "3M"},
			expectedResponse: []*response.CountedDuration{
				{
					Duration: "3d",
					Count:    1,
				},
				{
					Duration: "1w",
					Count:    1,
				},
				{
					Duration: "2w",
					Count:    1,
				},
				{
					Duration: "1M",
					Count:    1,
				},
				{
					Duration: "3M",
					Count:    1,
				},
			},
		},
		{
			description: "returned order same as requested order descending",
			nodes: []iBackend.Node{
				{
					Checkin: time.Now().AddDate(0, -4, 0),
				},
			},
			durations: []string{"3M", "1M", "2w", "1w", "3d"},
			expectedResponse: []*response.CountedDuration{
				{
					Duration: "3M",
					Count:    1,
				},
				{
					Duration: "1M",
					Count:    1,
				},
				{
					Duration: "2w",
					Count:    1,
				},
				{
					Duration: "1w",
					Count:    1,
				},
				{
					Duration: "3d",
					Count:    1,
				},
			},
		},
		{
			description: "returned order same as requested order random",
			nodes: []iBackend.Node{
				{
					Checkin: time.Now().AddDate(0, -4, 0),
				},
			},
			durations: []string{"3M", "3d", "2w", "1w", "1M"},
			expectedResponse: []*response.CountedDuration{
				{
					Duration: "3M",
					Count:    1,
				},
				{
					Duration: "3d",
					Count:    1,
				},
				{
					Duration: "2w",
					Count:    1,
				},
				{
					Duration: "1w",
					Count:    1,
				},
				{
					Duration: "1M",
					Count:    1,
				},
			},
		},
		{
			description: "All nodes",
			nodes: []iBackend.Node{
				{
					Checkin: time.Now(),
				},
				{
					Checkin: time.Now().AddDate(0, -4, 0),
				},
			},
			durations: []string{"0h"},
			expectedResponse: []*response.CountedDuration{
				{
					Duration: "0h",
					Count:    2,
				},
			},
		},
		{
			description: "Exclude deleted nodes",
			nodes: []iBackend.Node{
				{
					Checkin: time.Now(),
					Exists:  false,
				},
				{
					Checkin: time.Now().AddDate(0, -4, 0),
				},
			},
			durations: []string{"0h"},
			expectedResponse: []*response.CountedDuration{
				{
					Duration: "0h",
					Count:    2,
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

			actualResponse, err := cfgmgmt.GetMissingNodeDurationCounts(context.Background(),
				&request.MissingNodeDurationCounts{Durations: testCase.durations})
			require.NoError(t, err)

			assert.Equal(t, len(testCase.expectedResponse), len(actualResponse.CountedDurations))
			for index, expected := range testCase.expectedResponse {
				actual := actualResponse.CountedDurations[index]

				assert.Equal(t, expected.Duration, actual.Duration)
				assert.Equal(t, expected.Count, actual.Count)
			}
		})
	}
}

func TestMissingNodeRangeCountsExcludeDeletedNodes(t *testing.T) {

	// Get all nodes. There should only be one
	expectedResponse := []*response.CountedDuration{
		{
			Duration: "0h",
			Count:    1,
		},
	}

	// Add two nodes one deleted and one not
	nodes := []iBackend.Node{
		{
			Checkin: time.Now(),
			Exists:  false,
		},
		{
			Checkin: time.Now().AddDate(0, -4, 0),
			Exists:  true,
		},
	}

	for index := range nodes {
		nodeID := newUUID()
		nodes[index].NodeInfo.EntityUuid = nodeID
		nodes[index].NodeName = strconv.Itoa(index)
	}

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	actualResponse, err := cfgmgmt.GetMissingNodeDurationCounts(context.Background(),
		&request.MissingNodeDurationCounts{Durations: []string{"0h"}})
	require.NoError(t, err)

	assert.Equal(t, len(expectedResponse), len(actualResponse.CountedDurations))
	for index, expected := range expectedResponse {
		actual := actualResponse.CountedDurations[index]

		assert.Equal(t, expected.Duration, actual.Duration)
		assert.Equal(t, expected.Count, actual.Count)
	}
}
