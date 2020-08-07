package eventfeed_test

import (
	"context"
	"testing"

	cmsReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cmsRes "github.com/chef/automate/api/interservice/cfgmgmt/response"
	cmsService "github.com/chef/automate/api/interservice/cfgmgmt/service"
	event_feed_api "github.com/chef/automate/api/interservice/event_feed"
	agReq "github.com/chef/automate/components/automate-gateway/api/event_feed/request"
	agRes "github.com/chef/automate/components/automate-gateway/api/event_feed/response"
	subject "github.com/chef/automate/components/automate-gateway/eventfeed"
	"github.com/golang/mock/gomock"
	"github.com/stretchr/testify/assert"
)

func TestEventTaskCountsAllEmpty(t *testing.T) {
	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventTaskCounts(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventCountsFilter) (*cmsRes.EventCounts, error) {
		return &cmsRes.EventCounts{}, nil
	})

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedSummary(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *event_feed_api.FeedSummaryRequest) (*event_feed_api.FeedSummaryResponse, error) {
		return &event_feed_api.FeedSummaryResponse{}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventCounts, err := eventFeedAggregate.CollectEventTaskCounts(
		context.Background(),
		&agReq.EventCountsFilter{},
	)

	assert.Nil(t, err)
	assert.Equal(t, int64(0), eventCounts.Total, "Total number of counts should be zero")
}

func TestEventTaskCountsBothValues(t *testing.T) {
	ctrl := gomock.NewController(t)

	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetEventTaskCounts(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventCountsFilter) (*cmsRes.EventCounts, error) {
		return &cmsRes.EventCounts{
			Total: 6,
			Counts: []*cmsRes.EventCount{
				{
					Name:  "create",
					Count: 3,
				},
				{
					Name:  "update",
					Count: 2,
				},
				{
					Name:  "delete",
					Count: 1,
				},
			},
		}, nil
	})

	// TODO @afiune Ask compliance to integrate here with their service/rpc call
	//mockComplianceEventsClient := mock_compliance_events.NewMockEventsServiceClient(ctrl)
	//mockComplianceEventsClient.EXPECT().GetEventTaskCounts(
	//context.Background(),
	//gomock.Any(),
	//).DoAndReturn(func(c context.Context, action *cmplEvents.EventTaskCountsFilterRequest) (*cmplEvents.EventCountsResponse, error) {
	//return &cmplEvents.EventCountsResponse{
	//Total: 5,
	//Counts: []*cmplEvents.EventCountResponse{
	//&cmplEvents.EventCountResponse{
	//Name:  "create",
	//Count: 3,
	//},
	//&cmplEvents.EventCountResponse{
	//Name:  "delete",
	//Count: 2,
	//},
	//},
	//}, nil
	//})

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedSummary(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *event_feed_api.FeedSummaryRequest) (*event_feed_api.FeedSummaryResponse, error) {
		return &event_feed_api.FeedSummaryResponse{}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventCounts, err := eventFeedAggregate.CollectEventTaskCounts(
		context.Background(),
		&agReq.EventCountsFilter{},
	)

	assert.Nil(t, err)
	assert.Equal(t, int64(6), eventCounts.Total, "Total number of counts should be six")

	expectedCounts := []*agRes.EventCount{
		{
			Name:  "create",
			Count: 3,
		},
		{
			Name:  "update",
			Count: 2,
		},
		{
			Name:  "delete",
			Count: 1,
		},
	}

	assert.ElementsMatch(t, expectedCounts, eventCounts.Counts)
}

func TestEventTaskCountsOneEmpty(t *testing.T) {
	ctrl := gomock.NewController(t)

	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetEventTaskCounts(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventCountsFilter) (*cmsRes.EventCounts, error) {
		return &cmsRes.EventCounts{
			Total: 6,
			Counts: []*cmsRes.EventCount{
				{
					Name:  "create",
					Count: 3,
				},
				{
					Name:  "update",
					Count: 2,
				},
			},
		}, nil
	})

	// TODO @afiune Ask compliance to integrate here with their service/rpc call
	//mockComplianceEventsClient := mock_compliance_events.NewMockEventsServiceClient(ctrl)
	//mockComplianceEventsClient.EXPECT().GetEventTaskCounts(
	//context.Background(),
	//gomock.Any(),
	//).DoAndReturn(func(c context.Context, action *cmplEvents.EventTaskCountsFilterRequest) (*cmplEvents.EventCountsResponse, error) {
	//return &cmplEvents.EventCountsResponse{}, nil
	//})

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedSummary(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *event_feed_api.FeedSummaryRequest) (*event_feed_api.FeedSummaryResponse, error) {
		return &event_feed_api.FeedSummaryResponse{}, nil
	})
	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventCounts, err := eventFeedAggregate.CollectEventTaskCounts(
		context.Background(),
		&agReq.EventCountsFilter{},
	)

	assert.Nil(t, err)
	assert.Equal(t, int64(6), eventCounts.Total, "Total number of counts should be five")

	expectedCounts := []*agRes.EventCount{
		{
			Name:  "create",
			Count: 3,
		},
		{
			Name:  "update",
			Count: 2,
		},
	}

	assert.ElementsMatch(t, expectedCounts, eventCounts.Counts)
}

// TODO @afiune Ask compliance to integrate here with their service/rpc call
//func TestEventTaskCountsConfigMgmtDown(t *testing.T) {
//}

// TODO @afiune Ask compliance to integrate here with their service/rpc call
//func TestEventTaskCountsComplianceDown(t *testing.T) {
//}

// TODO @afiune Ask compliance to integrate here with their service/rpc call
//func TestEventTaskCountsAllSubServicesDown(t *testing.T) {
//}
