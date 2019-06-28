package server

import (
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/external/applications"
	"github.com/stretchr/testify/assert"
)

func TestConvertHealthStatusToProtoMatrix(t *testing.T) {
	cases := []struct {
		message  string
		health   string
		expected applications.HealthStatus
	}{
		{
			message:  "Status OK",
			health:   "OK",
			expected: applications.HealthStatus_OK,
		},
		{
			message:  "Status CRITICAL",
			health:   "CRITICAL",
			expected: applications.HealthStatus_CRITICAL,
		},
		{
			message:  "Status WARNING",
			health:   "WARNING",
			expected: applications.HealthStatus_WARNING,
		},
		{
			message:  "Status NONE",
			health:   "NONE",
			expected: applications.HealthStatus_NONE,
		},
		{
			message:  "Status UNKNOWN",
			health:   "UNKNOWN",
			expected: applications.HealthStatus_UNKNOWN,
		},
		{
			message:  "any other status returns UNKNOWN",
			health:   "FOO",
			expected: applications.HealthStatus_UNKNOWN,
		},
		{
			message:  "empty returns UNKNOWN",
			health:   "",
			expected: applications.HealthStatus_UNKNOWN,
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			actual := convertHealthStatusToProto(test.health)
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestTimeDiffPrettyMatrix(t *testing.T) {
	cases := []struct {
		message  string
		timeA    time.Time
		timeB    time.Time
		expected string
	}{
		{
			message:  "11 seconds",
			timeA:    time.Date(2018, 1, 1, 1, 1, 0, 0, time.UTC),
			timeB:    time.Date(2018, 1, 1, 1, 1, 11, 0, time.UTC),
			expected: "11 seconds",
		},
		{
			message:  "12 minute",
			timeA:    time.Date(2018, 1, 1, 1, 0, 0, 0, time.UTC),
			timeB:    time.Date(2018, 1, 1, 1, 12, 0, 0, time.UTC),
			expected: "12 minutes",
		},
		{
			message:  "23 hours",
			timeA:    time.Date(2018, 1, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2018, 1, 1, 23, 0, 0, 0, time.UTC),
			expected: "23 hours",
		},
		{
			message:  "10 days",
			timeA:    time.Date(2018, 1, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2018, 1, 11, 0, 0, 0, 0, time.UTC),
			expected: "10 days",
		},
		{
			message:  "11 months",
			timeA:    time.Date(2017, 1, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2017, 12, 1, 0, 0, 0, 0, time.UTC),
			expected: "11 months",
		},
		{
			message:  "40 years",
			timeA:    time.Date(2010, 1, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2050, 1, 1, 0, 0, 0, 0, time.UTC),
			expected: "40 years",
		},
		{
			message:  "a second",
			timeA:    time.Date(2018, 1, 1, 1, 1, 0, 0, time.UTC),
			timeB:    time.Date(2018, 1, 1, 1, 1, 1, 0, time.UTC),
			expected: "1 second",
		},
		{
			message:  "a minute",
			timeA:    time.Date(2018, 1, 1, 1, 0, 0, 0, time.UTC),
			timeB:    time.Date(2018, 1, 1, 1, 1, 0, 0, time.UTC),
			expected: "1 minute",
		},
		{
			message:  "an hour",
			timeA:    time.Date(2018, 1, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2018, 1, 1, 1, 0, 0, 0, time.UTC),
			expected: "1 hour",
		},
		{
			message:  "a day",
			timeA:    time.Date(2018, 1, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2018, 1, 2, 0, 0, 0, 0, time.UTC),
			expected: "1 day",
		},
		{
			message:  "a month",
			timeA:    time.Date(2017, 1, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2017, 2, 1, 0, 0, 0, 0, time.UTC),
			expected: "1 month",
		},
		{
			message:  "a year",
			timeA:    time.Date(2018, 1, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2019, 1, 1, 0, 0, 0, 0, time.UTC),
			expected: "1 year",
		},
		{
			message:  "all values must have a diff of 1",
			timeA:    time.Date(2017, 5, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2018, 6, 2, 1, 1, 1, 1, time.UTC),
			expected: "1 year 1 month 1 day 1 hour 1 minute 1 second",
		},
		{
			message:  "singular and plurals",
			timeA:    time.Date(2017, 5, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2028, 6, 21, 1, 30, 22, 1, time.UTC),
			expected: "11 years 1 month 20 days 1 hour 30 minutes 22 seconds",
		},
		{
			message:  "the limit of one month is 30 days",
			timeA:    time.Date(2017, 1, 2, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2017, 2, 1, 0, 0, 0, 0, time.UTC),
			expected: "30 days",
		},
		{
			message:  "feb has 28 days",
			timeA:    time.Date(2016, 2, 2, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2016, 3, 1, 0, 0, 0, 0, time.UTC),
			expected: "28 days",
		},
		{
			message:  "a month and one day",
			timeA:    time.Date(2018, 2, 11, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2019, 1, 12, 0, 0, 0, 0, time.UTC),
			expected: "11 months 1 day",
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			actual := timeDiffPretty(test.timeA, test.timeB)
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestTimeDiffMatrix(t *testing.T) {
	cases := []struct {
		message string
		timeA   time.Time
		timeB   time.Time
		// one single string that contains the returned values from the
		// timeDiff() func format: "y, mo, d, h, mi, s"
		expected string
	}{
		{
			message:  "a second",
			timeA:    time.Date(2018, 1, 1, 1, 1, 0, 0, time.UTC),
			timeB:    time.Date(2018, 1, 1, 1, 1, 1, 0, time.UTC),
			expected: "0 0 0 0 0 1",
		},
		{
			message:  "a minute",
			timeA:    time.Date(2018, 1, 1, 1, 0, 0, 0, time.UTC),
			timeB:    time.Date(2018, 1, 1, 1, 1, 0, 0, time.UTC),
			expected: "0 0 0 0 1 0",
		},
		{
			message:  "an hour",
			timeA:    time.Date(2018, 1, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2018, 1, 1, 1, 0, 0, 0, time.UTC),
			expected: "0 0 0 1 0 0",
		},
		{
			message:  "a day",
			timeA:    time.Date(2018, 1, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2018, 1, 2, 0, 0, 0, 0, time.UTC),
			expected: "0 0 1 0 0 0",
		},
		{
			message:  "a month",
			timeA:    time.Date(2017, 1, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2017, 2, 1, 0, 0, 0, 0, time.UTC),
			expected: "0 1 0 0 0 0",
		},
		{
			message:  "a year",
			timeA:    time.Date(2018, 1, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2019, 1, 1, 0, 0, 0, 0, time.UTC),
			expected: "1 0 0 0 0 0",
		},
		{
			message:  "all values must have a diff of 1",
			timeA:    time.Date(2017, 5, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2018, 6, 2, 1, 1, 1, 1, time.UTC),
			expected: "1 1 1 1 1 1",
		},
		{
			message:  "the limit of one month is 30 days",
			timeA:    time.Date(2017, 1, 2, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2017, 2, 1, 0, 0, 0, 0, time.UTC),
			expected: "0 0 30 0 0 0",
		},
		{
			message:  "feb has 28 days",
			timeA:    time.Date(2016, 2, 2, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2016, 3, 1, 0, 0, 0, 0, time.UTC),
			expected: "0 0 28 0 0 0",
		},
		{
			message:  "a month and one day",
			timeA:    time.Date(2018, 2, 11, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2019, 1, 12, 0, 0, 0, 0, time.UTC),
			expected: "0 11 1 0 0 0",
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			y, mo, d, h, mi, s := timeDiff(test.timeA, test.timeB)
			actual := fmt.Sprintf("%d %d %d %d %d %d", y, mo, d, h, mi, s)
			assert.Equal(t, test.expected, actual)
		})
	}
}
