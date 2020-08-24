package eventfeed

import (
	"context"

	agReq "github.com/chef/automate/api/external/event_feed/request"
	agRes "github.com/chef/automate/api/external/event_feed/response"
	event_feed_api "github.com/chef/automate/api/interservice/event_feed"
)

// CollectEventTaskCounts - collect the event task counts from all the components
func (eventFeedAggregate *EventFeedAggregate) CollectEventTaskCounts(
	ctx context.Context, request *agReq.GetEventTaskCountsRequest) (*agRes.GetEventTaskCountsResponse, error) {
	eventFilter := &event_feed_api.FeedSummaryRequest{
		Filters:       request.GetFilter(),
		Start:         request.GetStart(),
		End:           request.GetEnd(),
		CountCategory: "task",
	}

	feedEntryCounts, err := eventFeedAggregate.feedServiceClient.GetFeedSummary(ctx, eventFilter)
	if err != nil {
		return &agRes.GetEventTaskCountsResponse{}, err
	}

	// convert csEventTypeCounts to agEventTypeCounts
	agEventCounts := make([]*agRes.EventCount, len(feedEntryCounts.EntryCounts))
	for index, eventCounts := range feedEntryCounts.EntryCounts {
		agEventCounts[index] = &agRes.EventCount{
			Name:  eventCounts.Category,
			Count: eventCounts.Count,
		}
	}

	return &agRes.GetEventTaskCountsResponse{
		Total:  feedEntryCounts.TotalEntries,
		Counts: agEventCounts,
	}, nil
}
