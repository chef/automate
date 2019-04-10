package eventfeed

import (
	"context"
	"fmt"
	"strings"
	"sync"

	cmsReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cmsService "github.com/chef/automate/api/interservice/cfgmgmt/service"
	agReq "github.com/chef/automate/components/automate-gateway/api/event_feed/request"
	agRes "github.com/chef/automate/components/automate-gateway/api/event_feed/response"
	ccFeed "github.com/chef/automate/components/compliance-service/api/automate-feed"
	log "github.com/sirupsen/logrus"
)

// A eventCountsFunc type returns a subset of event counts and an error
type eventCountsFunc func() (*agRes.EventCounts, error)

// CollectEventTaskCounts - collect the event task counts from all the components
func (eventFeedAggregate *EventFeedAggregate) CollectEventTaskCounts(
	ctx context.Context, request *agReq.EventCountsFilter) (*agRes.EventCounts, error) {
	var (
		subscribers      = 2
		totalErrors      = make([]string, 0)
		totalEventCounts = &agRes.EventCounts{
			Counts: make([]*agRes.EventCount, 0),
			Total:  0,
		}
		collectComplianceFunc = func() (*agRes.EventCounts, error) {
			return collectFeedTaskCounts(ctx, eventFeedAggregate.feedServiceClient, request)
		}
		collectCfgmgmtFunc = func() (*agRes.EventCounts, error) {
			return collectConfigMgmtEventTaskCounts(ctx, eventFeedAggregate.cfgMgmtClient, request)
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

func fanInEventCounts(cs ...<-chan *agRes.EventCounts) <-chan *agRes.EventCounts {
	var wg sync.WaitGroup
	out := make(chan *agRes.EventCounts)

	output := func(c <-chan *agRes.EventCounts) {
		for n := range c {
			out <- n
		}
		wg.Done()
	}
	wg.Add(len(cs))
	for _, c := range cs {
		go output(c)
	}

	go func() {
		wg.Wait()
		close(out)
	}()
	return out
}

func fanOutEventCounts(component string, fn eventCountsFunc) (<-chan *agRes.EventCounts, <-chan error) {
	errorChan := make(chan error)
	eCountsChann := make(chan *agRes.EventCounts)
	go func() {
		eCounts, err := fn()
		if err != nil {
			log.WithFields(log.Fields{
				"err":       err,
				"component": component,
			}).Error("collecting event task counts")
		}
		errorChan <- err
		close(errorChan)
		eCountsChann <- eCounts
		close(eCountsChann)
	}()
	return eCountsChann, errorChan
}

func findEventCountIndex(eventCountArray []*agRes.EventCount,
	f func(*agRes.EventCount) bool) (int, bool) {
	for i, eCount := range eventCountArray {
		if f(eCount) {
			return i, true
		}
	}
	return 0, false
}

func collectConfigMgmtEventTaskCounts(ctx context.Context, cfgMgmtClient cmsService.CfgMgmtClient,
	request *agReq.EventCountsFilter) (*agRes.EventCounts, error) {
	eventFilter := &cmsReq.EventCountsFilter{
		Filter: request.GetFilter(),
		Start:  request.GetStart(),
		End:    request.GetEnd(),
	}

	csEventTypeCounts, err := cfgMgmtClient.GetEventTaskCounts(ctx, eventFilter)
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

func collectFeedTaskCounts(ctx context.Context, feedServiceClient ccFeed.FeedServiceClient,
	request *agReq.EventCountsFilter) (*agRes.EventCounts, error) {
	eventFilter := &ccFeed.FeedSummaryRequest{
		Filters:       request.GetFilter(),
		Start:         request.GetStart(),
		End:           request.GetEnd(),
		CountCategory: "task",
	}

	feedEntryCounts, err := feedServiceClient.GetFeedSummary(ctx, eventFilter)
	if err != nil {
		return &agRes.EventCounts{}, err
	}

	// convert csEventTypeCounts to agEventTypeCounts
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
