//
//  Author:: Gina Peers <gpeers@chef.io>, Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package util

import (
	"encoding/json"
	"github.com/chef/automate/lib/stringutils"
	"time"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"

	eventErrors "github.com/chef/automate/components/event-feed-service/pkg/errors"
)

type FeedSummary struct {
	Counts map[string]int64
}

type FeedQuery struct {
	UserID     string
	Size       int
	Start      time.Time
	End        time.Time
	Before     int64
	After      int64
	Filters    map[string][]string
	CursorID   string
	CursorDate time.Time
	Ascending  bool
}

type FeedSummaryQuery struct {
	CountsCategory string
	Buckets        bool
	Start          time.Time
	End            time.Time
	Filters        map[string][]string
}

// a feed timeline has multiple lines, one per action (verb)
type ActionLine struct {
	Action    string
	Timeslots []Timeslot
}

// a timeline has multiple timeslots
type Timeslot struct {
	EntryCounts []EntryCount
}

// each timeslot can have multiple categories of entries (profile, scan job, etc.), each with its own count
type EntryCount struct {
	Category string
	Count    int64
}

type FeedConsumerNetworkQuery struct {
	UserID string
}

type FeedConsumerNetwork struct {
	ID          string     `json:"id"`
	ProducerIDs []string   `json:"producer_ids"`
	Producers   []Producer `json:"producer"`
	Created     string     `json:"created"`
}

type Producer struct {
	ID         string   `json:"producer_id"`
	Name       string   `json:"producer_name"`
	ObjectType string   `json:"producer_object_type"`
	PTags      []string `json:"producer_tags"`
}

func (network *FeedConsumerNetwork) ToJSON() ([]byte, error) {
	return json.Marshal(network)
}

func (network *FeedConsumerNetwork) ToString() string {
	bytes, err := network.ToJSON()
	if err != nil {
		logrus.Warn("Couldn't convert feed consumer network struct to byte array")
		return ""
	}
	return BytesToString(bytes)
}

type FeedEntry struct {
	ID                 string    `json:"entity_uuid"`
	ProducerID         string    `json:"producer_id"`
	ProducerName       string    `json:"producer_name"`
	ProducerObjectType string    `json:"producer_object_type"`
	ProducerTags       []string  `json:"producer_tags"`
	FeedType           string    `json:"feed_type"` // e.g., event feed, msg feed, notifications/alerts, audit, other activity feed
	EventType          string    `json:"event_type"`
	Tags               []string  `json:"tags"`
	Published          time.Time `json:"pub_timestamp"`
	ActorID            string    `json:"actor_id"`
	ActorName          string    `json:"actor_name"`
	ActorObjectType    string    `json:"actor_object_type"`
	Verb               string    `json:"verb"`
	ObjectID           string    `json:"object_id"`
	ObjectName         string    `json:"object_name"`
	ObjectObjectType   string    `json:"object_object_type"`
	TargetID           string    `json:"target_id"`
	TargetName         string    `json:"target_name"`
	TargetObjectType   string    `json:"target_object_type"`
	Created            time.Time `json:"created"`
}

func (f *FeedEntry) ToJSON() ([]byte, error) {
	return json.Marshal(f)
}

func (f *FeedEntry) ToString() string {
	bytes, err := f.ToJSON()
	if err != nil {
		logrus.Warn("Couldn't convert feed entry struct to byte array")
		return ""
	}
	return BytesToString(bytes)
}

func BytesToString(data []byte) string {
	return string(data[:])
}

// ConvertAPIKeyToBackendKey Transforms the keys used for the API to the keys used in the
// backend (Elasticsearch).
func ConvertAPIKeyToBackendKey(parameter string) string {
	switch parameter {
	case "event-type":
		return "object_object_type"
	case "task":
		return "verb"
	case "requestorName":
		return "actor_name"
	default:
		return parameter
	}
}

// FormatFilters Will receive an array of filters and will format them into a map of strings
// To be used on filtering Events
//
// Example:
//   [
//    "environment:adios",
//    "environment:hola",
//    "cookbook:awesome",
//    "roles:lalala",
//   ]
//
// The returned filters would look like:
//
// map[string][]string [
// 	"environment": ["adios","hola"],
// 	"cookbook": ["awesome"],
// 	"roles": ["lalala"],
// ]
func FormatFilters(filters []string) (map[string][]string, error) {
	return stringutils.FormatFiltersWithKeyConverter(filters, ConvertAPIKeyToBackendKey)
}

func MillisecondsToTime(dateTime int64) time.Time {
	millisecondsReminder := dateTime % 1000
	return time.Unix(dateTime/1000, millisecondsReminder*1000000)
}

// StringDateRangeToTime will transform a Date Range (start/end) to a Time type
// it verifies that it has a specific layout
//
// The format we want is: YYYY-MM-DD
func StringDateRangeToTime(date string) (time.Time, bool) {
	var (
		loc, _ = time.LoadLocation("UTC")
		layout = "2006-01-02"
	)
	dTime, err := time.ParseInLocation(layout, date, loc)
	if err != nil {
		return dTime, false
	}
	return dTime, true
}

// ValidateMillisecondDateRange will validate that the provided start & end date are valid. That means
// the start time must be less than or equal to the end time
//
// NOTE: If start or end are 0 that's considered an ok "empty" parameter
func ValidateMillisecondDateRange(start int64, end int64) (time.Time, time.Time, error) {
	var (
		startTime time.Time
		endTime   time.Time
	)

	if start != 0 {
		startTime = MillisecondsToTime(start)
	}

	if end != 0 {
		endTime = MillisecondsToTime(end)
	}

	// if both start and end were provided, verify that the
	// end time is after the start time
	if end != 0 && start != 0 {
		if endTime.Before(startTime) {
			return startTime, endTime, eventErrors.GrpcError(codes.InvalidArgument, "Invalid start/end time. End before Start")
		}
	}

	return startTime, endTime, nil
}

// ValidateDateRange will validate that the provided start & end date are valid. That means they
// both have to have the right format and the start time must be less than or equal to the end time
//
// NOTE: If start or end are empty strings ("") that's consider an ok "empty" parameter
func ValidateDateRange(start string, end string) bool {
	var (
		startTime time.Time
		endTime   time.Time
		ok        bool
	)

	if start != "" {
		startTime, ok = StringDateRangeToTime(start)
		if !ok {
			return false
		}
	}

	if end != "" {
		endTime, ok = StringDateRangeToTime(end)
		if !ok {
			return false
		}
	}

	// If both were provided, lets verify that the
	// end time is after the start time
	if end != "" && start != "" {
		if endTime.Equal(startTime) {
			return true
		}
		return endTime.After(startTime)
	}

	return true
}

// ValidatePagingCursorTime will validate the paging parameters
func ValidatePagingCursorTime(before int64, after int64, cursor string, end int64) (time.Time, bool, error) {
	var (
		cursorTime time.Time
		ascending  bool
	)

	if before > 0 && after > 0 {
		return cursorTime, ascending, eventErrors.GrpcError(codes.InvalidArgument,
			"Invalid 'before'/'after' param. Both parameters should not be set")
	}

	if before > 0 && cursor == "" {
		return cursorTime, ascending, eventErrors.GrpcError(codes.InvalidArgument,
			"Invalid 'before' param. If the 'before' parameter is set the 'cursor' must be set also")
	}

	if after > 0 && cursor == "" && after != end {
		return cursorTime, ascending, eventErrors.GrpcError(codes.InvalidArgument,
			"Invalid 'after' param. If the 'after' parameter is set and not the 'cursor', then the 'after' must be equal to the 'end'")
	}

	// First page does not have any paging parameters
	if before == 0 && after == 0 && cursor == "" {
		ascending = false
	} else if before > 0 && cursor != "" { // next page
		ascending = false
		cursorTime = MillisecondsToTime(before)
	} else if after > 0 && cursor == "" && after == end { // last page
		ascending = true
	} else if after > 0 && cursor != "" { // previous page
		ascending = true
		cursorTime = MillisecondsToTime(after)
	}

	return cursorTime, ascending, nil
}

func Remove(s []string, r string) []string {
	for i, v := range s {
		if v == r {
			return append(s[:i], s[i+1:]...)
		}
	}
	return s
}

func StringArrayToInterfaceArray(array []string) []interface{} {
	interfaceArray := make([]interface{}, len(array))
	for i, v := range array {
		interfaceArray[i] = v
	}
	return interfaceArray
}
