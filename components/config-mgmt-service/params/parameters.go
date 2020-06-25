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
// This is used for query of the node-state only for sorting and the converge-history index.
func ConvertParamToNodeRunBackend(parameter string) string {
	switch parameter {
	case "id", "node_id":
		return backend.Id
	case "name":
		return backend.Name
	case "cookbook":
		return backend.Cookbook
	case "attribute":
		return backend.Attribute
	case "resource_name", "resource_names":
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
	case "chef_server", "source_fqdn":
		return backend.ChefServer
	case "environment":
		return backend.Environment
	case "platform":
		return backend.Platform
	case "policy_group":
		return backend.PolicyGroup
	case "policy_name":
		return backend.PolicyName
	case "policy_revision":
		return backend.PolicyRevision
	default:
		return parameter
	}
}

// ConvertParamToNodeStateBackendLowerFilter -
// The fields for the Elasticsearch mapping of node-state index.
// This is using the ".lower" values to match on fields case-insensitively
func ConvertParamToNodeStateBackendLowerFilter(parameter string) string {
	switch parameter {
	case "id", "node_id", backend.Id:
		return backend.Id
	case "name", backend.Name:
		return backend.Name + ".lower"
	case "cookbook", backend.Cookbook:
		return backend.Cookbook + ".lower"
	case "attribute", backend.Attribute:
		return backend.Attribute + ".lower"
	case "resource_name", "resource_names", backend.ResourceName:
		return backend.ResourceName + ".lower"
	case "recipe", backend.Recipe:
		return backend.Recipe + ".lower"
	case "organization", backend.Organization:
		return backend.Organization + ".lower"
	case "role", backend.Role:
		return backend.Role + ".lower"
	case "chef_version", backend.ChefVersion:
		return backend.ChefVersion + ".lower"
	case "chef_tags", backend.ChefTags:
		return backend.ChefTags + ".lower"
	case "error", backend.ErrorMessage:
		return backend.ErrorMessage + ".lower"
	case "error_type", backend.ErrorType:
		return backend.ErrorType + ".lower"
	case "chef_server", "source_fqdn", backend.ChefServer:
		return backend.ChefServer + ".lower"
	case "environment", backend.Environment:
		return backend.Environment + ".lower"
	case "platform", backend.Platform:
		return backend.Platform + ".lower"
	case "policy_group", backend.PolicyGroup:
		return backend.PolicyGroup + ".lower"
	case "policy_name", backend.PolicyName:
		return backend.PolicyName + ".lower"
	case "policy_revision", backend.PolicyRevision:
		return backend.PolicyRevision + ".lower"
	case "cloud_provider", backend.CloudProviderTag:
		return backend.CloudProviderTag + ".lower"
	case "timezone", backend.TimezoneTag:
		return backend.TimezoneTag + ".lower"
	case "kernel_release", backend.KernelReleaseTag:
		return backend.KernelReleaseTag + ".lower"
	case "kernel_version", backend.KernelVersionTag:
		return backend.KernelVersionTag + ".lower"
	case "virtualization_system", backend.VirtualizationSystemTag:
		return backend.VirtualizationSystemTag + ".lower"
	case "virtualization_role", backend.VirtualizationRoleTag:
		return backend.VirtualizationRoleTag + ".lower"
	case "dmi_system_manufacturer", backend.DmiSystemManufacturerTag:
		return backend.DmiSystemManufacturerTag + ".lower"
	case "dmi_system_serial_number", backend.DmiSystemSerialNumberTag:
		return backend.DmiSystemSerialNumberTag + ".lower"
	case "domain", backend.DomainTag:
		return backend.DomainTag + ".lower"
	case "hostname", backend.HostnameTag:
		return backend.HostnameTag + ".lower"
	case "macaddress", backend.MacaddressTag:
		return backend.MacaddressTag + ".lower"
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
	case "source_fqdn", "chef_server":
		return backend.ActionSourceFQDN
	case "event-type":
		return backend.EntityTypeName
	default:
		return parameter
	}
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

// ValidateDateTimeRange will validate that the provided start & end date are valid. That means they
// both have to have the right format and the start time must be less than or equal to the end time
//
// NOTE: If start or end are empty strings ("") that's consider an ok "empty" parameter
func ValidateDateTimeRange(start, end string) bool {
	var (
		startTime time.Time
		endTime   time.Time
		ok        bool
	)

	if start != "" {
		startTime, ok = StringDateTimeRangeToTime(start)
		if !ok {
			return false
		}
	}

	if end != "" {
		endTime, ok = StringDateTimeRangeToTime(end)
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
	dTime, err := time.ParseInLocation("2006-01-02", date, time.UTC)
	if err != nil {
		return dTime, false
	}
	return dTime, true
}

func StringDateTimeRangeToTime(date string) (time.Time, bool) {
	dTime, err := time.ParseInLocation(time.RFC3339, date, time.UTC)
	if err != nil {
		return dTime, false
	}
	return dTime, true
}
