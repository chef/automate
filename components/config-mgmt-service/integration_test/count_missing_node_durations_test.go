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
		filter           map[string][]string
		nodes            []iBackend.Node
		durations        []string
		expectedResponse []response.CountedDuration
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
					Checkin: time.Now().AddDate(0, 0, -28),
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
			durations: []string{"3d", "1w", "2w", "1m", "3m"},
			expectedResponse: []response.CountedDuration{
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
					Duration: "1m",
					Count:    2,
				},
				{
					Duration: "3m",
					Count:    1,
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
			actualResponse, err := cfgmgmt.MissingNodeDurationCounts(context.Background(),
				&request.MissingNodeDurationCounts{Durations: testCase.durations})
			require.NoError(t, err)

			assert.Equal(t, len(testCase.expectedResponse), len(actualResponse.CountedDurations))
			for index := range actualResponse.CountedDurations {
				expected := testCase.expectedResponse[index]
				actual := actualResponse.CountedDurations[index]
				assert.Equal(t, expected.Duration, actual.Duration)
				assert.Equal(t, expected.Count, actual.Count)
			}
		})
	}
}
