package elastic

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func Test(t *testing.T) {
	cases := []struct {
		description string
		utcTime     time.Time
		expected    string
	}{
		{
			description: "0 hours",
			utcTime:     parseTime(t, "2020-03-15T00:34:00Z"),
			expected:    "-00:00",
		},
		{
			description: "1st hour",
			utcTime:     parseTime(t, "2020-03-15T01:00:00Z"),
			expected:    "-01:00",
		},
		{
			description: "2nd hours",
			utcTime:     parseTime(t, "2020-03-15T02:01:00Z"),
			expected:    "-02:00",
		},
		{
			description: "3rd hours",
			utcTime:     parseTime(t, "2020-03-15T03:59:59Z"),
			expected:    "-03:00",
		},
		{
			description: "4th hours",
			utcTime:     parseTime(t, "2020-03-15T04:15:15Z"),
			expected:    "-04:00",
		},
		{
			description: "5th hours",
			utcTime:     parseTime(t, "2020-03-15T05:15:15Z"),
			expected:    "-05:00",
		},
		{
			description: "6th hours",
			utcTime:     parseTime(t, "2020-03-15T06:11:15Z"),
			expected:    "-06:00",
		},
		{
			description: "7th hours",
			utcTime:     parseTime(t, "2020-03-15T07:32:15Z"),
			expected:    "-07:00",
		},
		{
			description: "8th hours",
			utcTime:     parseTime(t, "2020-03-15T08:40:15Z"),
			expected:    "-08:00",
		},
		{
			description: "9th hours",
			utcTime:     parseTime(t, "2020-03-15T09:46:15Z"),
			expected:    "-09:00",
		},
		{
			description: "10th hours",
			utcTime:     parseTime(t, "2020-03-15T10:46:15Z"),
			expected:    "-10:00",
		},
		{
			description: "11th hours",
			utcTime:     parseTime(t, "2020-03-15T11:22:15Z"),
			expected:    "-11:00",
		},
		{
			description: "12th hours",
			utcTime:     parseTime(t, "2020-03-15T12:22:15Z"),
			expected:    "+12:00",
		},
		{
			description: "13th hours",
			utcTime:     parseTime(t, "2020-03-15T13:22:15Z"),
			expected:    "+11:00",
		},
		{
			description: "14th hours",
			utcTime:     parseTime(t, "2020-03-15T14:22:15Z"),
			expected:    "+10:00",
		},
		{
			description: "22nd hours",
			utcTime:     parseTime(t, "2020-03-15T22:22:15Z"),
			expected:    "+02:00",
		},
		{
			description: "23rd hours",
			utcTime:     parseTime(t, "2020-03-15T23:22:15Z"),
			expected:    "+01:00",
		},
	}

	for _, testCase := range cases {
		t.Run(testCase.description, func(t *testing.T) {

			timeZoneAtStartOfUtcHour := getTimezoneWithStartOfDayAtUtcHour(testCase.utcTime)

			assert.Equal(t, testCase.expected, timeZoneAtStartOfUtcHour)
		})
	}
}

func parseTime(t *testing.T, timeString string) time.Time {
	parsedTime, err := time.Parse(time.RFC3339, timeString)
	require.NoError(t, err)

	return parsedTime
}
