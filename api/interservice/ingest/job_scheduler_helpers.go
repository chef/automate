package ingest

import (
	"errors"
	"fmt"
	"strings"
	"time"
)

// Validate is a helper function for the JobSettings proto message that will validate
// that every field has the right values, if one of them doesn't have a valid value
// then it will return an error with the problem and a suggestion to fix it
func (js *JobSettings) Validate() error {
	// Validate field: Every
	if e := js.GetEvery(); len(e) > 0 {
		_, err := time.ParseDuration(e)
		if err != nil {
			return err
		}
	}

	// Validate field: Threshold
	if t := js.GetThreshold(); len(t) > 0 {
		err := validateDateMath(t)
		if err != nil {
			return err
		}
	}

	return nil
}

// validateDateMath ensures that the provided time unit complies with the following Elasticsearch
// rules documented in the following link:
// => https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#date-math
//
// Allowed units are 'y', 'M', 'w', 'd', 'h', 'H', 'm' and 's'
//
// Example of valid values: '1M', '30d', '24h', '1d', '60m', '12345m', etc.
//
// TODO: @afiune vielleicht move this to a different package to take advantage of its functionality
func validateDateMath(tUnit string) error {
	var (
		// Default error message in case of an invalid value
		err = errors.New(fmt.Sprintf("invalid time unit: '%s'", tUnit))

		// The list of valid characters
		validChars = "yMwdhHms"

		// The list of invalid characters
		numbers = "0123456789"

		// Should start with number
		hasNumber = false
	)

	if tUnit == "" {
		return err
	}

	for tUnit != "" {
		char := string(tUnit[0])
		tUnit = tUnit[1:]

		// Numbers are allowed [0-9]
		if strings.Contains(numbers, char) {
			// But they have to be followed by a valid unit
			if tUnit == "" {
				return err
			}
			hasNumber = true
			continue
		}

		// Valid characters are allowed [yMwdhHms]
		if strings.Contains(validChars, char) {
			// But we have to have already a number
			if hasNumber {
				hasNumber = false
				continue
			}
			return err
		}

		// If we got here, there is an invalid character
		return err
	}

	return nil
}
