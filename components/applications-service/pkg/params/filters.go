package params

import (
	"fmt"
	"net/url"
	"strings"

	"github.com/pkg/errors"
)

// FormatFilters Will receive an array of filters and will format them into a map of strings
//
// Example:
//   [
//    "status:critical",
//    "service_name:redis",
//    "service_name:postgres",
//    "application:cafe",
//    "environment:prod",
//   ]
//
// The returned filters would look like:
//
// map[string][]string [
// 	"status": ["critical"],
// 	"service_name": ["redis","postgres"],
// 	"application": ["cafe"],
// 	"environment": ["prod"]
// ]
// TODO: (afiune) Migrate this to a common go package that other teams can consume
func FormatFilters(filters []string) (map[string][]string, error) {
	filterMap := make(map[string][]string, len(filters))

	for _, filter := range filters {
		keyValuePair := strings.Split(filter, ":")
		// If we do not have a filter with the format (KEY:VALUE)
		if len(keyValuePair) != 2 || keyValuePair[0] == "" || keyValuePair[1] == "" {
			return nil, errors.Errorf(
				fmt.Sprintf("Invalid filter '%s' (format: key:value)", filter),
			)
		}
		key := keyValuePair[0]
		value, err := decodeValue(keyValuePair[1])
		if err != nil {
			return nil, err
		}

		filterMap[key] = append(filterMap[key], value)
	}
	return filterMap, nil
}

func decodeValue(rawValue string) (string, error) {
	value, err := url.QueryUnescape(rawValue)
	if err != nil {
		return value, err
	}

	trimmedValue := strings.TrimSpace(value)
	if len(trimmedValue) == 0 {
		return trimmedValue, errors.New(
			"Invalid filter; Empty value in format: key:value")
	}

	return trimmedValue, nil
}
