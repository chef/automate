package stringutils

import (
	"errors"
	"net/url"
	"strings"
)

// FormatFilters Will receive an array of filters and will format them into a map of strings
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
	return FormatFiltersWithKeyConverter(filters, func(k string) string { return k })
}

// FormatFiltersWithKeyConverter The same as FormatFilters with the function keyNameConverter that
// converts the keys from aliases.
func FormatFiltersWithKeyConverter(filters []string,
	keyNameConverter func(string) string) (map[string][]string, error) {
	filterMap := make(map[string][]string, len(filters))

	for _, filter := range filters {
		keyValuePair := strings.Split(filter, ":")
		// If we do not have a filter with the format (KEY:VALUE)
		if len(keyValuePair) != 2 || keyValuePair[0] == "" || keyValuePair[1] == "" {
			return nil, errors.New("Invalid filter (format: key:value) " + filter)
		}
		key, err := decodeValue(keyValuePair[0])
		if err != nil {
			return nil, err
		}
		key = keyNameConverter(key)
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
		return trimmedValue, errors.New("Invalid filter; Empty value in format: key:value")
	}

	return trimmedValue, nil
}
