package timef_test

import (
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/lib/timef"
	"github.com/stretchr/testify/assert"
)

func TestIntervalUntilNow(t *testing.T) {
	// Lets get the time now
	timeNow := time.Now()

	// Wait for a second to test the interval until (the new) "now"
	time.Sleep(1 * time.Second)

	diff := timef.IntervalUntilNow(timeNow)
	assert.Equal(t, "1 second", diff)
}

func TestIntervalUntilNowDefaultTimeUnitsEqual2(t *testing.T) {
	// Lets get the time now
	timeNow := time.Now()

	// This should return "1 year, 1 month, 1 day"
	diff := timef.IntervalUntilNow(timeNow.AddDate(1, 1, 1))

	// but since we cap the time units to a maximum of 2
	assert.Equal(t, "1 year, 1 month", diff)
}

func TestDiffPrettyNUnitsFewFieldsMatrix(t *testing.T) {
	timeA := time.Date(2017, 6, 21, 0, 0, 0, 0, time.UTC)
	timeB := time.Date(2028, 6, 21, 1, 30, 2, 1, time.UTC)
	//expected: "11 years, 1 month, 20 days, 1 hour, 30 minutes, 2 seconds",
	cases := []struct {
		message  string
		units    int
		expected string
	}{
		{
			message:  "all time units",
			units:    6,
			expected: "11 years, 1 hour, 30 minutes, 2 seconds",
		},
		{
			message:  "all time units",
			units:    5,
			expected: "11 years, 1 hour, 30 minutes, 2 seconds",
		},
		{
			message:  "all time units",
			units:    4,
			expected: "11 years, 1 hour, 30 minutes, 2 seconds",
		},
		{
			message:  "all time units",
			units:    3,
			expected: "11 years, 1 hour, 30 minutes",
		},
		{
			message:  "all time units",
			units:    2,
			expected: "11 years, 1 hour",
		},
		{
			message:  "all time units",
			units:    1,
			expected: "11 years",
		},
		{
			message:  "negative value should return at least one field",
			units:    -1,
			expected: "11 years",
		},
		{
			message:  "zero should return at least one field",
			units:    0,
			expected: "11 years",
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			actual := timef.DiffPrettyNUnits(timeA, timeB, test.units)
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestDiffPrettyNUnitsAllFieldsMatrix(t *testing.T) {
	timeA := time.Date(2017, 5, 1, 0, 0, 0, 0, time.UTC)
	timeB := time.Date(2018, 6, 2, 1, 1, 1, 1, time.UTC)
	cases := []struct {
		message  string
		units    int
		expected string
	}{
		{
			message:  "all time units",
			units:    6,
			expected: "1 year, 1 month, 1 day, 1 hour, 1 minute, 1 second",
		},
		{
			message:  "all time units",
			units:    5,
			expected: "1 year, 1 month, 1 day, 1 hour, 1 minute",
		},
		{
			message:  "all time units",
			units:    4,
			expected: "1 year, 1 month, 1 day, 1 hour",
		},
		{
			message:  "all time units",
			units:    3,
			expected: "1 year, 1 month, 1 day",
		},
		{
			message:  "all time units",
			units:    2,
			expected: "1 year, 1 month",
		},
		{
			message:  "all time units",
			units:    1,
			expected: "1 year",
		},
		{
			message:  "negative value should return at least one field",
			units:    -1,
			expected: "1 year",
		},
		{
			message:  "zero should return at least one field",
			units:    0,
			expected: "1 year",
		},
		{
			message:  "more than the max number of time units should return them all six",
			units:    7,
			expected: "1 year, 1 month, 1 day, 1 hour, 1 minute, 1 second",
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			actual := timef.DiffPrettyNUnits(timeA, timeB, test.units)
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestDiffPrettyAllUnitsMatrix(t *testing.T) {
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
			message:  "2 minutes",
			timeA:    time.Date(2018, 1, 1, 1, 0, 0, 0, time.UTC),
			timeB:    time.Date(2018, 1, 1, 1, 2, 0, 0, time.UTC),
			expected: "2 minutes",
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
			message:  "6 months",
			timeA:    time.Date(2017, 1, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2017, 7, 1, 0, 0, 0, 0, time.UTC),
			expected: "6 months",
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
			expected: "1 year, 1 month, 1 day, 1 hour, 1 minute, 1 second",
		},
		{
			message:  "singular and plurals",
			timeA:    time.Date(2017, 5, 1, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2028, 6, 21, 1, 30, 2, 1, time.UTC),
			expected: "11 years, 1 month, 20 days, 1 hour, 30 minutes, 2 seconds",
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
			message:  "11 months and one day",
			timeA:    time.Date(2018, 2, 11, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2019, 1, 12, 0, 0, 0, 0, time.UTC),
			expected: "11 months, 1 day",
		},
		{
			message:  "same 11 months and one day in a different timezone UTC-8",
			timeA:    time.Date(2018, 2, 11, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2019, 1, 12, 0, 0, 0, 0, time.FixedZone("UTC-8", -8*60*60)),
			expected: "11 months, 1 day, 8 hours",
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			actual := timef.DiffPrettyAllUnits(test.timeA, test.timeB)
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestDiffMatrix(t *testing.T) {
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
		{
			message:  "calculate days from the correct month",
			timeA:    time.Date(2015, 1, 11, 0, 0, 0, 0, time.UTC),
			timeB:    time.Date(2015, 3, 10, 0, 0, 0, 0, time.UTC),
			expected: "0 1 27 0 0 0",
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			y, mo, d, h, mi, s := timef.Diff(test.timeA, test.timeB)
			actual := fmt.Sprintf("%d %d %d %d %d %d", y, mo, d, h, mi, s)
			assert.Equal(t, test.expected, actual)
		})
	}
}
