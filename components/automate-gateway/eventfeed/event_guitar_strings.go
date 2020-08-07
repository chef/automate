package eventfeed

import (
	"context"
	"fmt"
	"math"
	"strings"
	"sync"
	"time"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	cmsReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cmsRes "github.com/chef/automate/api/interservice/cfgmgmt/response"
	cmsService "github.com/chef/automate/api/interservice/cfgmgmt/service"
	event_feed_api "github.com/chef/automate/api/interservice/event_feed"
	agReq "github.com/chef/automate/components/automate-gateway/api/event_feed/request"
	agRes "github.com/chef/automate/components/automate-gateway/api/event_feed/response"
	log "github.com/sirupsen/logrus"
)

// A stringsFunc type returns a subset of event strings and an error
type stringsFunc func() (*agRes.EventStrings, error)

// CollectEventGuitarStrings - collect the guitar strings from all the componets
//
// It is important to mention that we will be able to survive if one of the
// components return an error but the rest of them don't, the reason why is because
// the gateway is just collecting information from the downstream services and it
// should be able to handle errors.
func (eventFeedAggregate *EventFeedAggregate) CollectEventGuitarStrings(ctx context.Context,
	request *agReq.EventStrings) (*agRes.EventStrings, error) {

	var (
		totalErrors        = make([]string, 0)
		subscribers        = 2
		err                = validateEventStringsRequest(request)
		stringsCollections = make([][]*agRes.EventString, 0)
	)
	if err != nil {
		return &agRes.EventStrings{}, err
	}

	totalNumberOfBucketsAllowed, err := getTotalNumberOfBucketsAllowed(request)
	if err != nil {
		return &agRes.EventStrings{}, err
	}

	collectCfgmgmtStringsFunc := func() (*agRes.EventStrings, error) {
		return collectConfigMgmtEventGuitarStrings(ctx, eventFeedAggregate.cfgMgmtClient, request, totalNumberOfBucketsAllowed)
	}
	collectComplianceStringsFunc := func() (*agRes.EventStrings, error) {
		return collectComplianceEventGuitarStrings(ctx, eventFeedAggregate.feedServiceClient, request, totalNumberOfBucketsAllowed)
	}

	c1, e1 := fanOutEventStrings("config-mgmt-service", collectCfgmgmtStringsFunc)
	c2, e2 := fanOutEventStrings("compliance-service", collectComplianceStringsFunc)

	for e := range fanErrIn(e1, e2) {
		if e != nil {
			totalErrors = append(totalErrors, e.Error())
		}
	}

	// If the number of errors is major or equal to the number of subscribers
	// it means that we don't have any data to display, lets return an error
	if len(totalErrors) >= subscribers {
		emptyEventStrings, err := createEmptyEventStrings(request)
		if err != nil {
			totalErrors = append(totalErrors, err.Error())
		}
		return emptyEventStrings, fmt.Errorf(strings.Join(totalErrors, "\n"))
	}

	// TODO @afiune - In the near future we would like our API to handle partial
	// errors so that we don't ignore them here, but for now we will.

	for c := range fanInEventStrings(c1, c2) {
		if c == nil {
			continue
		}

		if c.Start != request.Start {
			return &agRes.EventStrings{}, status.Errorf(codes.Internal,
				"Returned start dates do not match. %s != %s", c.Start, request.Start)
		}
		if c.End != request.End {
			return &agRes.EventStrings{}, status.Errorf(codes.Internal,
				"Returned end dates do not match. %s != %s", c.End, request.End)
		}

		if c.HoursBetween != request.HoursBetween {
			return &agRes.EventStrings{}, status.Errorf(codes.Internal,
				"Returned hours between do not match. %d != %d", c.HoursBetween, request.HoursBetween)
		}

		stringsCollections = append(stringsCollections, c.Strings)
	}

	guitarStringCollection, err := mergeGuitarStringCollections(stringsCollections)
	if err != nil {
		return &agRes.EventStrings{}, err
	}

	return &agRes.EventStrings{
		Strings:      guitarStringCollection,
		Start:        request.Start,
		End:          request.End,
		HoursBetween: request.HoursBetween,
	}, nil
}

func fanOutEventStrings(component string, fn stringsFunc) (<-chan *agRes.EventStrings, <-chan error) {
	errorChan := make(chan error)
	stringsChann := make(chan *agRes.EventStrings)
	go func() {
		eStrings, err := fn()
		if err != nil {
			log.WithFields(log.Fields{
				"err":       err,
				"component": component,
			}).Error("collecting event strings")
		}
		errorChan <- err
		close(errorChan)
		stringsChann <- eStrings
		close(stringsChann)
	}()
	return stringsChann, errorChan
}

func fanInEventStrings(cs ...<-chan *agRes.EventStrings) <-chan *agRes.EventStrings {
	var wg sync.WaitGroup
	out := make(chan *agRes.EventStrings)

	output := func(c <-chan *agRes.EventStrings) {
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

func validateEventStringsRequest(request *agReq.EventStrings) error {
	// Validate TimeZone
	if request.Timezone == "" {
		return status.Error(codes.InvalidArgument, "A timezone must be provided")
	}

	_, err := time.LoadLocation(request.Timezone)
	if err != nil {
		return status.Error(codes.InvalidArgument, err.Error())
	}

	// Validate HoursBetween
	if request.HoursBetween <= 0 || request.HoursBetween > 24 {
		return status.Error(codes.InvalidArgument,
			"HoursBetween must be greater than 0 and less than or equal to 24")
	}

	if 24%request.HoursBetween != 0 {
		return status.Error(codes.InvalidArgument, "24 must be divisible by HoursBetween")
	}

	// Validate Date Range
	err = validateDateRange(request.Start, request.End)
	if err != nil {
		return err
	}

	return nil
}

func getTotalNumberOfBucketsAllowed(request *agReq.EventStrings) (int, error) {
	start := request.Start
	end := request.End
	bucketSizeInHours := int(request.HoursBetween)
	numberOfDaysBetween, err := getNumberOfDaysBetween(start, end, request.Timezone)
	if err != nil {
		return 0, err
	}

	return ((24 / bucketSizeInHours) * numberOfDaysBetween), nil
}

func getNumberOfDaysBetween(start, end, loc string) (int, error) {
	timezone, err := time.LoadLocation(loc)
	if err != nil {
		return 0, err
	}

	startTime, err := time.ParseInLocation("2006-01-02", start, timezone)
	if err != nil {
		return 0, err
	}

	endTime, err := time.ParseInLocation("2006-01-02", end, timezone)
	if err != nil {
		return 0, err
	}

	startUpdate := time.Date(startTime.Year(), startTime.Month(), startTime.Day(),
		0, 0, 0, 0, timezone)
	endUpdate := time.Date(endTime.Year(), endTime.Month(), endTime.Day(),
		0, 0, 0, 0, timezone).AddDate(0, 0, 1)
	return int(math.Round(endUpdate.Sub(startUpdate).Hours() / 24.0)), nil
}

// ValidateDateRange will validate that the provided start & end date are valid. That means they
// both have to have the right format and the start time must be less than or equal to the end time
//
// NOTE: If start or end are empty strings ("") that's consider an ok "empty" parameter
func validateDateRange(start string, end string) error {
	var (
		startTime time.Time
		endTime   time.Time
		err       error
	)

	if start != "" {
		startTime, err = createStartTime(start, time.UTC)
		if err != nil {
			return err
		}
	}

	if end != "" {
		endTime, err = createEndTime(end, time.UTC)
		if err != nil {
			return err
		}
	}

	// If both were provided, lets verify that the
	// end time is after the start time
	if end != "" && start != "" {
		if endTime.Equal(startTime) {
			return nil
		}
		if endTime.Before(startTime) {
			return status.Error(codes.InvalidArgument, "Invalid start time must be before end time")
		}
	}

	return nil
}

func createEmptyEventStrings(request *agReq.EventStrings) (*agRes.EventStrings, error) {
	loc, err := time.LoadLocation(request.Timezone)
	if err != nil {
		return &agRes.EventStrings{}, err
	}

	startTime, err := createStartTime(request.Start, loc)
	if err != nil {
		return &agRes.EventStrings{}, err
	}

	endTime, err := createEndTime(request.End, loc)
	if err != nil {
		return &agRes.EventStrings{}, err
	}

	itemCollection := make([]*agRes.EventCollection, 0)
	for startTime.Before(endTime) {
		startTime = startTime.Add(time.Duration(int64(time.Hour) * int64(request.HoursBetween)))
		itemCollection = append(itemCollection, new(agRes.EventCollection))
	}

	guitarStrings := make([]*agRes.EventString, 3)

	guitarStrings[0] = &agRes.EventString{
		EventAction: "create",
		Collection:  itemCollection,
	}

	guitarStrings[1] = &agRes.EventString{
		EventAction: "update",
		Collection:  itemCollection,
	}

	guitarStrings[2] = &agRes.EventString{
		EventAction: "delete",
		Collection:  itemCollection,
	}

	return &agRes.EventStrings{
		Strings:      guitarStrings,
		Start:        request.Start,
		End:          request.End,
		HoursBetween: request.HoursBetween,
	}, nil
}

func collectConfigMgmtEventGuitarStrings(ctx context.Context, cfgMgmtClient cmsService.CfgMgmtClient,
	request *agReq.EventStrings, totalNumberOfBucketsAllowed int) (*agRes.EventStrings, error) {
	eventStrings := &cmsReq.EventStrings{
		Start:        request.GetStart(),
		End:          request.GetEnd(),
		Timezone:     request.GetTimezone(),
		HoursBetween: request.GetHoursBetween(),
		Filter:       request.GetFilter(),
	}

	csEventStringBuckets, err := cfgMgmtClient.GetEventStringBuckets(ctx, eventStrings)
	if err != nil {
		return nil, err
	}

	// convert into gateway string buckets
	agEventStringBuckets := make([]*agRes.EventString, len(csEventStringBuckets.Strings))
	for index, csEventBuckets := range csEventStringBuckets.Strings {
		agEventStringBuckets[index] = &agRes.EventString{
			Collection:  configMgmtToGatewayEventCollection(csEventBuckets.Collection, totalNumberOfBucketsAllowed),
			EventAction: csEventBuckets.EventAction,
		}
	}

	return &agRes.EventStrings{
		Strings:      agEventStringBuckets,
		Start:        csEventStringBuckets.Start,
		End:          csEventStringBuckets.End,
		HoursBetween: csEventStringBuckets.HoursBetween,
	}, nil
}

// configMgmtToGatewayEventCollection converts a config-mgmt-service EventCollection message
// to a Gateway EventCollection message
func configMgmtToGatewayEventCollection(collection []*cmsRes.EventCollection, totalNumberOfBucketsAllowed int) []*agRes.EventCollection {
	newCollection := make([]*agRes.EventCollection, len(collection))
	for e, eCollection := range collection {
		newCollection[e] = new(agRes.EventCollection)
		if eCollection != nil && len(eCollection.EventsCount) > 0 {
			newCollection[e].EventsCount = make([]*agRes.EventCount, len(eCollection.EventsCount))
			for c, eCount := range eCollection.EventsCount {
				// Lets try this without assigning the casting to a variable
				newCollection[e].EventsCount[c] = (*agRes.EventCount)(eCount)
			}
		}
	}

	if len(collection) < totalNumberOfBucketsAllowed {
		for index := len(collection); index < totalNumberOfBucketsAllowed; index++ {
			newCollection = append(newCollection, new(agRes.EventCollection))
		}
	}

	return newCollection[:totalNumberOfBucketsAllowed]
}

// complianceToGatewayEventCollection converts a compliance-service (feed) timeline "Slots" message
// to a Gateway EventCollection message. A FeedTimeline consists of ActionLines. Each Actionline has
// Timeslots which contain EventCounts, each of which has a count category and count.
func complianceToGatewayEventCollection(slots []*event_feed_api.Timeslot, totalNumberOfBucketsAllowed int) []*agRes.EventCollection {
	newCollection := make([]*agRes.EventCollection, len(slots))
	for e, slot := range slots {
		newCollection[e] = new(agRes.EventCollection)
		if slot != nil && len(slot.Counts) > 0 {
			newCollection[e].EventsCount = make([]*agRes.EventCount, len(slot.Counts))
			for c, count := range slot.Counts {
				newCollection[e].EventsCount[c] = &agRes.EventCount{Name: count.Category, Count: count.Count}
			}
		}
	}

	if len(slots) < totalNumberOfBucketsAllowed {
		for index := len(slots); index < totalNumberOfBucketsAllowed; index++ {
			newCollection = append(newCollection, new(agRes.EventCollection))
		}
	}

	return newCollection[:totalNumberOfBucketsAllowed]
}

func collectComplianceEventGuitarStrings(ctx context.Context,
	feedServiceClient event_feed_api.EventFeedServiceClient,
	request *agReq.EventStrings, totalNumberOfBucketsAllowed int) (*agRes.EventStrings, error) {

	req := &event_feed_api.FeedTimelineRequest{
		Start:    request.GetStart(),
		End:      request.GetEnd(),
		Timezone: request.GetTimezone(),
		Interval: request.GetHoursBetween(),
		Filters:  request.GetFilter(),
	}

	feedTimeline, err := feedServiceClient.GetFeedTimeline(ctx, req)
	if err != nil {
		return nil, err
	}

	// convert into gateway string buckets
	agEventStringBuckets := make([]*agRes.EventString, len(feedTimeline.ActionLines))
	for index, line := range feedTimeline.ActionLines {
		agEventStringBuckets[index] = &agRes.EventString{
			Collection:  complianceToGatewayEventCollection(line.Slots, totalNumberOfBucketsAllowed),
			EventAction: line.Action,
		}
	}

	return &agRes.EventStrings{
		Strings:      agEventStringBuckets,
		Start:        feedTimeline.Start,
		End:          feedTimeline.End,
		HoursBetween: feedTimeline.Interval,
	}, nil
}

func mergeGuitarStringCollections(stringsCollections [][]*agRes.EventString) ([]*agRes.EventString, error) {
	var (
		first                       = true
		stringsLen                  int
		mergedEventStringCollection []*agRes.EventString
	)

	for _, s := range stringsCollections {
		if hasDuplicateStrings(s) {
			return []*agRes.EventString{}, status.Error(codes.Internal,
				"Returned guitar string collection has a duplicate")
		}

		if first {
			// Calculate the strings length to compare it with the rest of the collections
			stringsLen = len(s)
			// Save the first collection as the base to continue merging strings
			mergedEventStringCollection = s
			first = false
			continue
		}

		if len(s) != stringsLen {
			return []*agRes.EventString{}, status.Errorf(codes.Internal,
				"Returned number of guitar string are not equal. %d != %d", len(s), stringsLen)
		}

		for _, guitarString := range s {
			matchGsIndex, found := findGuitarStringIndex(mergedEventStringCollection,
				func(es *agRes.EventString) bool {
					return es.EventAction == guitarString.EventAction
				})

			if !found {
				return []*agRes.EventString{}, status.Errorf(codes.Internal,
					"Could not find a matching Guitar String for '%s'", guitarString.EventAction)
			}

			mergedEventString, err := mergeGuitarString(guitarString, mergedEventStringCollection[matchGsIndex])
			if err != nil {
				return []*agRes.EventString{}, err
			}

			mergedEventStringCollection[matchGsIndex] = mergedEventString
		}
	}

	return mergedEventStringCollection, nil
}

func mergeGuitarString(guitarString1, guitarString2 *agRes.EventString) (*agRes.EventString, error) {
	if len(guitarString1.Collection) != len(guitarString2.Collection) {
		return &agRes.EventString{}, status.Errorf(codes.Internal,
			"Returned matching guitar string differ in size. %d != %d",
			len(guitarString1.Collection), len(guitarString2.Collection))
	}

	mergedEventCollection := make([]*agRes.EventCollection, len(guitarString1.Collection))
	for index, eCollection1 := range guitarString1.Collection {
		eCollection2 := guitarString2.Collection[index]
		mergedEventCollection[index] = mergeGuitarStringEventCollection(eCollection1, eCollection2)
	}

	return &agRes.EventString{
		Collection:  mergedEventCollection,
		EventAction: guitarString1.EventAction,
	}, nil
}

// mergeGuitarStringEventCollection merges two EventCollection objects into one single collection
func mergeGuitarStringEventCollection(ec1, ec2 *agRes.EventCollection) *agRes.EventCollection {
	mergedEventCountCollection := make([]*agRes.EventCount, 0)
	for _, eCount := range ec1.EventsCount {
		// Search for a matching event
		matchingEventCount, found := findMatchingEventCount(eCount, ec2.EventsCount)

		var mergedEventCount *agRes.EventCount
		if found {
			mergedEventCount = &agRes.EventCount{
				Name:  eCount.Name,
				Count: eCount.Count + matchingEventCount.Count,
			}

		} else {
			mergedEventCount = &agRes.EventCount{
				Name:  eCount.Name,
				Count: eCount.Count,
			}
		}

		mergedEventCountCollection = append(mergedEventCountCollection, mergedEventCount)
	}

	missingEventCountCollection := findMissingEventCounts(ec2.EventsCount, ec1.EventsCount)

	mergedEventCountCollection = append(mergedEventCountCollection, missingEventCountCollection...)

	return &agRes.EventCollection{EventsCount: mergedEventCountCollection}
}

func findMissingEventCounts(eventCC1, eventCC2 []*agRes.EventCount) []*agRes.EventCount {
	missingEventCountCollection := make([]*agRes.EventCount, 0)
	for _, eventCount := range eventCC1 {
		_, found := findMatchingEventCount(eventCount, eventCC2)

		if !found {
			missingEventCountCollection = append(missingEventCountCollection, eventCount)
		}
	}
	return missingEventCountCollection
}

func findMatchingEventCount(eventCountToFind *agRes.EventCount, eventCountColl []*agRes.EventCount) (*agRes.EventCount, bool) {
	for _, eventCount := range eventCountColl {
		if eventCountToFind.Name == eventCount.Name {
			return eventCount, true
		}
	}
	return nil, false
}

func findGuitarStringIndex(guitarStringCollection []*agRes.EventString,
	f func(*agRes.EventString) bool) (int, bool) {
	for i, guitarString := range guitarStringCollection {
		if f(guitarString) {
			return i, true
		}
	}
	return 0, false
}

func hasDuplicateStrings(guitarStringCollection []*agRes.EventString) bool {
	nameMap := make(map[string]int)
	for _, guitarString := range guitarStringCollection {
		if _, ok := nameMap[guitarString.EventAction]; ok {
			return true
		}
		nameMap[guitarString.EventAction] = 1
	}

	return false
}

func createStartTime(startDate string, loc *time.Location) (time.Time, error) {
	startTime, err := time.ParseInLocation("2006-01-02", startDate, loc)
	if err != nil {
		return startTime, err
	}

	// At the beginning of the day
	return time.Date(startTime.Year(), startTime.Month(), startTime.Day(),
		0, 0, 0, 0, startTime.Location()), nil
}

func createEndTime(endDate string, loc *time.Location) (time.Time, error) {
	endTime, err := time.ParseInLocation("2006-01-02", endDate, loc)
	if err != nil {
		return endTime, err
	}

	// At the end of the day
	return time.Date(endTime.Year(), endTime.Month(), endTime.Day(),
		23, 59, 59, 0, endTime.Location()), nil
}
