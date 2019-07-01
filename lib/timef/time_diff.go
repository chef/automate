// time format, time calculation, all time related things.
package timef

import (
	"fmt"
	"time"
)

// IntervalUntilNow is the short of 'DiffPretty(timeA, now)'
func IntervalUntilNow(t time.Time) string {
	return DiffPretty(t, time.Now())
}

// DiffPretty returns an interval between two times in a pretty format (single string)
// e.g. 1 year, 4 months, 1 day, 7 hours, 40 minutes, 49 seconds
func DiffPretty(a, b time.Time) string {
	var (
		pretty       = ""
		separator    = ", "
		addSeparator = func(str string) string {
			if str != "" {
				return str + separator
			}
			return str
		}
		addTimeParam = func(str, param string, value int) string {
			if value > 0 {
				str = addSeparator(str)
				if value == 1 { // singular
					return fmt.Sprintf("%s%d %s", str, value, param)
				}
				// plural
				return fmt.Sprintf("%s%d %ss", str, value, param)
			}
			return str
		}
		year, month, day, hour, min, sec = Diff(a, b)
	)

	// @afiune order here matters!
	pretty = addTimeParam(pretty, "year", year)
	pretty = addTimeParam(pretty, "month", month)
	pretty = addTimeParam(pretty, "day", day)
	pretty = addTimeParam(pretty, "hour", hour)
	pretty = addTimeParam(pretty, "minute", min)
	return addTimeParam(pretty, "second", sec)
}

// Diff returns the specific interval between two times
// Reference: https://stackoverflow.com/a/36531443
func Diff(a, b time.Time) (year, month, day, hour, min, sec int) {
	if a.Location() != b.Location() {
		b = b.In(a.Location())
	}
	if a.After(b) {
		a, b = b, a
	}
	var (
		y1, M1, d1 = a.Date()
		y2, M2, d2 = b.Date()

		h1, m1, s1 = a.Clock()
		h2, m2, s2 = b.Clock()
	)

	year = y2 - y1
	month = int(M2 - M1)
	day = d2 - d1
	hour = h2 - h1
	min = m2 - m1
	sec = s2 - s1

	// Normalize negative values
	if sec < 0 {
		sec += 60
		min--
	}
	if min < 0 {
		min += 60
		hour--
	}
	if hour < 0 {
		hour += 24
		day--
	}
	if day < 0 {
		// Reference: https://stackoverflow.com/a/49170822
		t := time.Date(y2, M2, 0, 0, 0, 0, 0, time.UTC)
		day += t.Day()
		month--
	}
	if month < 0 {
		month += 12
		year--
	}
	return year, month, day, hour, min, sec
}
