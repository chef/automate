package integration_test

import (
	"testing"
	"time"

	"context"
	"fmt"

	dls "github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	log "github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type purgeTestCase struct {
	description       string
	request           *dls.TriggerPurgeRequest
	expectedResponse  *dls.TriggerPurgeResponse
	expectedFeedRes   *dls.PurgeResponse
	expectedRemaining int64
}

// The data lifecycle is set to only the keep the last 7 days of events
// Create 1000 events one for every 24 hours going back from now
// triggering the data lifecycle should remove 993 and leave the most recent 7 events
func TestFeedPurge(t *testing.T) {
	var (
		totalEntries = 1000
		startDate    = time.Now().UTC()
		timeDiff     = int(time.Hour) * -24
		entries      = createEntries(startDate, totalEntries, timeDiff)
		statusMap    = make(map[string]*dls.PurgeStatus)
		resMap       = make(map[string]*dls.PurgeResponse)
		purgeReqId   = newUUID()
	)

	purgeReq := dls.TriggerPurgeRequest{Id: purgeReqId, ServiceName: "event-feed-service"}

	purgeStatusOK := dls.PurgeStatus{
		Status: dls.PurgeStatus_SUCCESS,
		Msg:    "",
	}
	statusMap[persistence.IndexNameFeeds] = &purgeStatusOK
	purgeRes := dls.PurgeResponse{
		Id:              purgeReqId,
		ComponentStatus: statusMap,
	}

	resMap["event-feed-service"] = &purgeRes
	triggerPurgeRes := dls.TriggerPurgeResponse{Responses: resMap}

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	cases := []purgeTestCase{
		{
			description:       "purge should delete 993 docs and leave 7 docs",
			request:           &purgeReq,
			expectedResponse:  &triggerPurgeRes,
			expectedFeedRes:   &purgeRes,
			expectedRemaining: 7,
		},
	}

	// Run all the cases!
	runPurgeTestCases(t, cases)
}

// Runing the data lifecycle when there are zero events
func TestFeedPurgeNoDocsInIndex(t *testing.T) {
	var (
		statusMap  = make(map[string]*dls.PurgeStatus)
		resMap     = make(map[string]*dls.PurgeResponse)
		purgeReqId = newUUID()
	)

	purgeReq := dls.TriggerPurgeRequest{Id: purgeReqId, ServiceName: "event-feed-service"}

	purgeStatusOK := dls.PurgeStatus{
		Status: dls.PurgeStatus_SUCCESS,
		Msg:    "",
	}
	statusMap[persistence.IndexNameFeeds] = &purgeStatusOK
	purgeRes := dls.PurgeResponse{
		Id:              purgeReqId,
		ComponentStatus: statusMap,
	}

	resMap["event-feed-service"] = &purgeRes
	triggerPurgeRes := dls.TriggerPurgeResponse{Responses: resMap}

	cases := []purgeTestCase{
		{
			description:       "no docs in index... purge should delete 0 docs",
			request:           &purgeReq,
			expectedResponse:  &triggerPurgeRes,
			expectedFeedRes:   &purgeRes,
			expectedRemaining: 0,
		},
	}

	// Run all the cases!
	runPurgeTestCases(t, cases)
}

// The data lifecycle is set to only the keep the last 7 days of events
// Create 1000 events within the last 7 days
// triggering the data lifecycle should not remove any events
func TestFeedPurgeNoDocsToDelete(t *testing.T) {
	var (
		totalEntries = 1000
		startDate    = time.Now().UTC()
		timeDiff     = 0
		entries      = createEntries(startDate, totalEntries, timeDiff)
		statusMap    = make(map[string]*dls.PurgeStatus)
		resMap       = make(map[string]*dls.PurgeResponse)
		purgeReqId   = newUUID()
	)

	purgeReq := dls.TriggerPurgeRequest{Id: purgeReqId, ServiceName: "event-feed-service"}

	purgeStatusOK := dls.PurgeStatus{
		Status: dls.PurgeStatus_SUCCESS,
		Msg:    "",
	}
	statusMap[persistence.IndexNameFeeds] = &purgeStatusOK
	purgeRes := dls.PurgeResponse{
		Id:              purgeReqId,
		ComponentStatus: statusMap,
	}

	resMap["event-feed-service"] = &purgeRes
	triggerPurgeRes := dls.TriggerPurgeResponse{Responses: resMap}

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	cases := []purgeTestCase{
		{
			description:       "purge should delete 0 docs and leave 1000 docs",
			request:           &purgeReq,
			expectedResponse:  &triggerPurgeRes,
			expectedFeedRes:   &purgeRes,
			expectedRemaining: 1000,
		},
	}

	// Run all the cases!
	runPurgeTestCases(t, cases)
}

func runPurgeTestCases(t *testing.T, cases []purgeTestCase) {
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := dataLifecycleClient.TriggerPurge(context.Background(), test.request)
				testSuite.RefreshIndices(persistence.IndexNameFeeds)
				require.NoError(t, err)

				// get the feed purge response; there should be only one
				var count int
				var feedRes *dls.PurgeResponse

				for _, r := range res.Responses {
					if r.Id == test.request.Id {
						count++
						feedRes = r
					}
				}
				require.Equal(t, 1, count)
				require.Equal(t, test.expectedFeedRes.Id, feedRes.Id)
				log.Infof("Purge response: %+v", feedRes)
				assert.Equal(t, test.expectedFeedRes.ComponentStatus[persistence.IndexNameFeeds].Status, feedRes.ComponentStatus[persistence.IndexNameFeeds].Status)

				counts, err := testSuite.feedServer.GetFeedSummary(context.Background(), &event_feed.FeedSummaryRequest{CountCategory: "event-type"})
				require.NoError(t, err)

				assert.Equal(t, test.expectedRemaining, counts.TotalEntries,
					"Expected number of post-purge documents remaining does not match results %d != %d", test.expectedRemaining, counts.TotalEntries)
			})
	}
}
