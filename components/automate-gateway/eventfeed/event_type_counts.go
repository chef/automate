package eventfeed

import (
	"context"
	"fmt"
	"strings"

	cmsReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cmsService "github.com/chef/automate/api/interservice/cfgmgmt/service"
	agReq "github.com/chef/automate/components/automate-gateway/api/event_feed/request"
	agRes "github.com/chef/automate/components/automate-gateway/api/event_feed/response"
	ccFeed "github.com/chef/automate/components/compliance-service/api/automate-feed"
)

// CollectEventTypeCounts - collect the event type counts from all the components
func (eventFeedAggregate *EventFeedAggregate) CollectEventTypeCounts(
	ctx context.Context, request *agReq.EventCountsFilter) (*agRes.EventCounts, error) {

	var (
		subscribers      = 2
		totalErrors      = make([]string, 0)
		totalEventCounts = &agRes.EventCounts{
			Counts: make([]*agRes.EventCount, 0),
			Total:  0,
		}
		collectComplianceFunc = func() (*agRes.EventCounts, error) {
			return collectFeedEventTypeCounts(ctx, eventFeedAggregate.feedServiceClient, request)
		}
		collectCfgmgmtFunc = func() (*agRes.EventCounts, error) {
			return collectConfigMgmtEventTypeCounts(ctx, eventFeedAggregate.cfgMgmtClient, request)
		}
	)

	c1, e1 := fanOutEventCounts("config-mgmt-service", collectCfgmgmtFunc)
	c2, e2 := fanOutEventCounts("compliance-service", collectComplianceFunc)

	for e := range fanErrIn(e1, e2) {
		if e != nil {
			totalErrors = append(totalErrors, e.Error())
		}
	}

	// If the number of errors is major or equal to the number of subscribers
	// it means that we don't have any data to display, lets return an error
	if len(totalErrors) >= subscribers {
		return totalEventCounts, fmt.Errorf(strings.Join(totalErrors, "\n"))
	}

	// TODO @afiune - In the near future we would like our API to handle partial
	// errors so that we don't ignore them here, but for now we will.

	for ec := range fanInEventCounts(c1, c2) {
		totalEventCounts.Total += ec.GetTotal()
		for _, cs := range ec.GetCounts() {
			matchEcIndex, found := findEventCountIndex(totalEventCounts.GetCounts(),
				func(eCount *agRes.EventCount) bool {
					return eCount.GetName() == cs.GetName()
				})

			if found {
				totalEventCounts.Counts[matchEcIndex].Count += cs.GetCount()
			} else {
				totalEventCounts.Counts = append(totalEventCounts.Counts, cs)
			}
		}
	}

	return totalEventCounts, nil
}

func collectConfigMgmtEventTypeCounts(ctx context.Context, cfgMgmtClient cmsService.CfgMgmtClient,
	request *agReq.EventCountsFilter) (*agRes.EventCounts, error) {
	eventFilter := &cmsReq.EventCountsFilter{
		Filter: request.GetFilter(),
		Start:  request.GetStart(),
		End:    request.GetEnd(),
	}

	csEventTypeCounts, err := cfgMgmtClient.GetEventTypeCounts(ctx, eventFilter)
	if err != nil {
		return &agRes.EventCounts{}, err
	}

	// convert csEventTypeCounts to agEventTypeCounts
	agEventCounts := make([]*agRes.EventCount, len(csEventTypeCounts.Counts))
	for index, csEventCounts := range csEventTypeCounts.Counts {
		agEventCounts[index] = &agRes.EventCount{
			Name:  csEventCounts.Name,
			Count: csEventCounts.Count,
		}
	}

	return &agRes.EventCounts{
		Total:  csEventTypeCounts.Total,
		Counts: agEventCounts,
	}, nil
}

func collectFeedEventTypeCounts(ctx context.Context,
	feedServiceClient ccFeed.FeedServiceClient,
	request *agReq.EventCountsFilter) (*agRes.EventCounts, error) {
	eventFilter := &ccFeed.FeedSummaryRequest{
		Filters:       request.GetFilter(),
		Start:         request.GetStart(),
		End:           request.GetEnd(),
		CountCategory: "entity_type",
	}

	feedEntryCounts, err := feedServiceClient.GetFeedSummary(ctx, eventFilter)
	if err != nil {
		return &agRes.EventCounts{}, err
	}

	// convert feedEntryCounts to agEventTypeCounts
	agEventCounts := make([]*agRes.EventCount, len(feedEntryCounts.EntryCounts))
	for index, eventCounts := range feedEntryCounts.EntryCounts {
		agEventCounts[index] = &agRes.EventCount{
			Name:  eventCounts.Category,
			Count: eventCounts.Count,
		}
	}

	return &agRes.EventCounts{
		Total:  feedEntryCounts.TotalEntries,
		Counts: agEventCounts,
	}, nil
}
