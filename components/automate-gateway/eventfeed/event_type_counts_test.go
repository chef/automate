package eventfeed_test

import (
	"context"
	"testing"

	cmsReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cmsRes "github.com/chef/automate/api/interservice/cfgmgmt/response"
	cmsService "github.com/chef/automate/api/interservice/cfgmgmt/service"
	agReq "github.com/chef/automate/components/automate-gateway/api/event_feed/request"
	agRes "github.com/chef/automate/components/automate-gateway/api/event_feed/response"
	subject "github.com/chef/automate/components/automate-gateway/eventfeed"
	mock_automate_feed "github.com/chef/automate/components/automate-gateway/gateway_mocks/mock_feed"
	complFeed "github.com/chef/automate/components/compliance-service/api/automate-feed"
	"github.com/golang/mock/gomock"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

func TestEventTypeCountsAllEmpty(t *testing.T) {
	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventTypeCounts(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventCountsFilter) (*cmsRes.EventCounts, error) {
		return &cmsRes.EventCounts{}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedSummary(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedSummaryRequest) (*complFeed.FeedSummaryResponse, error) {
		return &complFeed.FeedSummaryResponse{}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventCounts, err := eventFeedAggregate.CollectEventTypeCounts(
		context.Background(),
		&agReq.EventCountsFilter{},
	)

	assert.Nil(t, err)
	assert.Equal(t, int64(0), eventCounts.Total, "Total number of counts should be zero")
}

func TestEventTypeCountsBothValues(t *testing.T) {
	ctrl := gomock.NewController(t)

	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetEventTypeCounts(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventCountsFilter) (*cmsRes.EventCounts, error) {
		return &cmsRes.EventCounts{
			Total: 5,
			Counts: []*cmsRes.EventCount{
				&cmsRes.EventCount{
					Name:  "node",
					Count: 3,
				},
				&cmsRes.EventCount{
					Name:  "cookbook",
					Count: 2,
				},
			},
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedSummary(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedSummaryRequest) (*complFeed.FeedSummaryResponse, error) {
		return &complFeed.FeedSummaryResponse{
			TotalEntries: 5,
			EntryCounts: []*complFeed.EntryCount{
				&complFeed.EntryCount{
					Category: "scanjobs",
					Count:    3,
				},
				&complFeed.EntryCount{
					Category: "profile",
					Count:    2,
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventCounts, err := eventFeedAggregate.CollectEventTypeCounts(
		context.Background(),
		&agReq.EventCountsFilter{},
	)

	assert.Nil(t, err)
	assert.Equal(t, int64(10), eventCounts.Total, "Total number of counts should be ten")

	expectedCounts := []*agRes.EventCount{
		&agRes.EventCount{
			Name:  "scanjobs",
			Count: 3,
		},
		&agRes.EventCount{
			Name:  "profile",
			Count: 2,
		},
		&agRes.EventCount{
			Name:  "node",
			Count: 3,
		},
		&agRes.EventCount{
			Name:  "cookbook",
			Count: 2,
		},
	}

	assert.ElementsMatch(t, expectedCounts, eventCounts.Counts)
}

func TestEventTypeCountsOneEmpty(t *testing.T) {
	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventTypeCounts(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventCountsFilter) (*cmsRes.EventCounts, error) {
		return &cmsRes.EventCounts{}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedSummary(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedSummaryRequest) (*complFeed.FeedSummaryResponse, error) {
		return &complFeed.FeedSummaryResponse{
			TotalEntries: 5,
			EntryCounts: []*complFeed.EntryCount{
				&complFeed.EntryCount{
					Category: "scanjobs",
					Count:    3,
				},
				&complFeed.EntryCount{
					Category: "profile",
					Count:    2,
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventCounts, err := eventFeedAggregate.CollectEventTypeCounts(
		context.Background(),
		&agReq.EventCountsFilter{},
	)

	assert.Nil(t, err)
	assert.Equal(t, int64(5), eventCounts.Total, "Total number of counts should be five")

	expectedCounts := []*agRes.EventCount{
		&agRes.EventCount{
			Name:  "scanjobs",
			Count: 3,
		},
		&agRes.EventCount{
			Name:  "profile",
			Count: 2,
		},
	}

	assert.ElementsMatch(t, expectedCounts, eventCounts.Counts)
}

func TestEventTypeCountsConfigMgmtDown(t *testing.T) {
	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventTypeCounts(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventCountsFilter) (*cmsRes.EventCounts, error) {
		return &cmsRes.EventCounts{}, status.Error(codes.Unavailable, "cfgmgmt service not running")
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedSummary(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedSummaryRequest) (*complFeed.FeedSummaryResponse, error) {
		return &complFeed.FeedSummaryResponse{
			TotalEntries: 5,
			EntryCounts: []*complFeed.EntryCount{
				&complFeed.EntryCount{
					Category: "scanjobs",
					Count:    3,
				},
				&complFeed.EntryCount{
					Category: "profile",
					Count:    2,
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventCounts, err := eventFeedAggregate.CollectEventTypeCounts(
		context.Background(),
		&agReq.EventCountsFilter{},
	)

	// Ignoring config-mgmt error
	if assert.Nil(t, err) {
		assert.Equal(t, int64(5), eventCounts.Total, "Total number of counts should be five")
	}
}

func TestEventTypeCountsComplianceDown(t *testing.T) {
	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventTypeCounts(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventCountsFilter) (*cmsRes.EventCounts, error) {
		return &cmsRes.EventCounts{
			Total: 5,
			Counts: []*cmsRes.EventCount{
				&cmsRes.EventCount{
					Name:  "node",
					Count: 3,
				},
				&cmsRes.EventCount{
					Name:  "cookbook",
					Count: 2,
				},
			},
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedSummary(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedSummaryRequest) (*complFeed.FeedSummaryResponse, error) {
		return &complFeed.FeedSummaryResponse{}, status.Error(codes.Unavailable, "compliance service not running")
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventCounts, err := eventFeedAggregate.CollectEventTypeCounts(
		context.Background(),
		&agReq.EventCountsFilter{},
	)

	// Ignoring compliance error
	if assert.Nil(t, err) {
		assert.Equal(t, int64(5), eventCounts.Total, "Total number of counts should be five")
	}
}

func TestEventTypeCountsAllSubServicesDown(t *testing.T) {
	ctrl := gomock.NewController(t)

	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetEventTypeCounts(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventCountsFilter) (*cmsRes.EventCounts, error) {
		return &cmsRes.EventCounts{}, status.Error(codes.Unavailable, "cfgmgmt service not running")
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedSummary(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedSummaryRequest) (*complFeed.FeedSummaryResponse, error) {
		return &complFeed.FeedSummaryResponse{}, status.Error(codes.Unavailable, "compliance service not running")
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventCounts, err := eventFeedAggregate.CollectEventTypeCounts(
		context.Background(),
		&agReq.EventCountsFilter{},
	)

	if assert.NotNil(t, err) {
		assert.Contains(t, err.Error(), "cfgmgmt service not running")
		assert.Contains(t, err.Error(), "compliance service not running")
		assert.Equal(t, int64(0), eventCounts.Total, "Total number of counts should be zero")
	}
}
