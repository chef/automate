package integration_test

import (
	"testing"
	"time"

	"context"
	"fmt"

	"os"

	dls "github.com/chef/automate/api/interservice/data_lifecycle"
	automate_feed "github.com/chef/automate/components/compliance-service/api/automate-feed"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
)

type purgeTestCase struct {
	description       string
	request           *dls.TriggerPurgeRequest
	expectedResponse  *dls.TriggerPurgeResponse
	expectedFeedRes   *dls.PurgeResponse
	expectedRemaining int64
}

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

	purgeReq := dls.TriggerPurgeRequest{Id: purgeReqId, ServiceName: "compliance-service"}

	purgeStatusOK := dls.PurgeStatus{
		Status: dls.PurgeStatus_SUCCESS,
		Msg:    "",
	}
	statusMap["comp-2-feeds"] = &purgeStatusOK
	purgeRes := dls.PurgeResponse{
		Id:              purgeReqId,
		ComponentStatus: statusMap,
	}

	resMap["compliance-service"] = &purgeRes
	triggerPurgeRes := dls.TriggerPurgeResponse{Responses: resMap}

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(mappings.IndexNameFeeds)
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

func TestFeedPurgeNoDocsInIndex(t *testing.T) {
	var (
		statusMap  = make(map[string]*dls.PurgeStatus)
		resMap     = make(map[string]*dls.PurgeResponse)
		purgeReqId = newUUID()
	)

	purgeReq := dls.TriggerPurgeRequest{Id: purgeReqId, ServiceName: "compliance-service"}

	purgeStatusOK := dls.PurgeStatus{
		Status: dls.PurgeStatus_SUCCESS,
		Msg:    "",
	}
	statusMap["comp-2-feeds"] = &purgeStatusOK
	purgeRes := dls.PurgeResponse{
		Id:              purgeReqId,
		ComponentStatus: statusMap,
	}

	resMap["compliance-service"] = &purgeRes
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

	purgeReq := dls.TriggerPurgeRequest{Id: purgeReqId, ServiceName: "compliance-service"}

	purgeStatusOK := dls.PurgeStatus{
		Status: dls.PurgeStatus_SUCCESS,
		Msg:    "",
	}
	statusMap["comp-2-feeds"] = &purgeStatusOK
	purgeRes := dls.PurgeResponse{
		Id:              purgeReqId,
		ComponentStatus: statusMap,
	}

	resMap["compliance-service"] = &purgeRes
	triggerPurgeRes := dls.TriggerPurgeResponse{Responses: resMap}

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(mappings.IndexNameFeeds)
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
				res, err := purgeClient.TriggerPurge(context.Background(), test.request)
				testSuite.RefreshIndices(mappings.IndexNameFeeds)

				if assert.Nil(t, err) {
					// get the feed purge response; there should be only one
					var count int
					var feedRes *dls.PurgeResponse

					for _, r := range res.Responses {
						if r.Id == test.request.Id {
							count++
							feedRes = r
						}
					}
					assert.Equal(t, 1, count)
					assert.Equal(t, test.expectedFeedRes.Id, feedRes.Id)
					logrus.Infof("Purge response: %+v", feedRes)
					assert.Equal(t, test.expectedFeedRes.ComponentStatus["comp-2-feeds"].Status, feedRes.ComponentStatus["comp-2-feeds"].Status)

					counts, err := feedService.GetFeedSummary(context.Background(), &automate_feed.FeedSummaryRequest{CountCategory: "entity_type"})
					if err != nil {
						logrus.Error(err)
						os.Exit(1)
					}

					assert.Equal(t, test.expectedRemaining, counts.TotalEntries,
						"Expected number of post-purge documents remaining does not match results %d != %d", test.expectedRemaining, counts.TotalEntries)
				}
			})
	}
}
