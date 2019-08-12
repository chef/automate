//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package params

// This file will host all helper methods to manipulate parameters
// like filters, pagination, etc. It is extending the `rest` package
// so that any endpoint can access them

import (
	"time"

	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/errors"
	"github.com/chef/automate/lib/stringutils"
	"google.golang.org/grpc/codes"
)

// ConvertParamToNodeRunBackend Transforms a parameter provided by consumers
// into a parameter that the 'backend' can understand.
//
// We HAVE to use this function on every parameter query that we collect
func ConvertParamToNodeRunBackend(parameter string) string {
	switch parameter {
	case "id":
		return backend.Id
	case "node_id":
		return backend.Id
	case "name":
		return backend.Name
	case "cookbook":
		return backend.Cookbook
	case "attribute":
		return backend.Attribute
	case "resource_name":
		return backend.ResourceName
	case "recipe":
		return backend.Recipe
	case "organization":
		return backend.Organization
	case "role":
		return backend.Role
	case "chef_version":
		return backend.ChefVersion
	case "chef_tags":
		return backend.ChefTags
	case "error":
		return backend.ErrorMessage
	case "chef_server":
		return backend.ChefServer
	default:
		return parameter
	}
}

// ConvertParamToActionBackend Transforms a parameter provided by consumers
// into a parameter that the 'backend' can understand.
func ConvertParamToActionBackend(parameter string) string {
	switch parameter {
	case "organization":
		return backend.ActionOrganization
	case "source_fqdn":
		return backend.ActionSourceFQDN
	default:
		return parameter
	}
}

// FormatNodeFilters Will receive an array of filters and will format them into a map of strings
// To be used on filtering Node Runs
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
func FormatNodeFilters(filters []string) (map[string][]string, error) {
	return stringutils.FormatFiltersWithKeyConverter(filters, ConvertParamToNodeRunBackend)
}

// FormatActionFilters Will receive an array of filters and will format them into a map of strings
// To be used on filtering Actions.
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
func FormatActionFilters(filters []string) (map[string][]string, error) {
	return stringutils.FormatFiltersWithKeyConverter(filters, ConvertParamToActionBackend)
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

// ValidateMillsecondDateRange will validate that the provided start & end date are valid. That means
// the start time must be less than or equal to the end time
//
// NOTE: If start or end are 0 that's consider an ok "empty" parameter
func ValidateMillsecondDateRange(start int64, end int64) (time.Time, time.Time, error) {
	var (
		startTime time.Time
		endTime   time.Time
	)

	if start != 0 {
		startTime = millisecondsToTime(start)
	}

	if end != 0 {
		endTime = millisecondsToTime(end)
	}

	// If both were provided, lets verify that the
	// end time is after the start time
	if end != 0 && start != 0 {
		if endTime.Before(startTime) {
			return startTime, endTime, errors.GrpcError(codes.InvalidArgument, "Invalid start/end time. End before Start")
		}
	}

	return startTime, endTime, nil
}

// ValidatePagingCursorTime will validate the paging parameters
func ValidatePagingCursorTime(before int64, after int64, cursor string, end int64) (time.Time, bool, error) {
	var (
		cursorTime time.Time
		ascending  bool
	)

	if before > 0 && after > 0 {
		return cursorTime, ascending, errors.GrpcError(codes.InvalidArgument,
			"Invalid 'before'/'after' param. Both parameters should not be set")
	}

	if before > 0 && cursor == "" {
		return cursorTime, ascending, errors.GrpcError(codes.InvalidArgument,
			"Invalid 'before' param. If the 'before' parameter is set the 'cursor' must be set also")
	}

	if after > 0 && cursor == "" && after != end {
		return cursorTime, ascending, errors.GrpcError(codes.InvalidArgument,
			"Invalid 'after' param. If the 'after' parameter is set and not the 'cursor', then the 'after' must be equal to the 'end'")
	}

	// First page does not have any paging parameters
	if before == 0 && after == 0 && cursor == "" {
		ascending = false
	} else if before > 0 && cursor != "" { // next page
		ascending = false
		cursorTime = millisecondsToTime(before)
	} else if after > 0 && cursor == "" && after == end { // last page
		ascending = true
	} else if after > 0 && cursor != "" { // previous page
		ascending = true
		cursorTime = millisecondsToTime(after)
	}

	return cursorTime, ascending, nil
}

func millisecondsToTime(dateTime int64) time.Time {
	millsecondsReminder := dateTime % 1000
	return time.Unix(dateTime/1000, millsecondsReminder*1000000)
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
