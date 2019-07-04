// time format, time calculation, all time related things.
package timef

import (
	"fmt"
	"time"
)

const (
	DefaultNumberOfTimeUnits = 2
	MaxNumberOfTimeUnits     = 6
)

// IntervalUntilNow is the short of 'DiffPrettyNUnits(timeA, now, defaulUnits)'
func IntervalUntilNow(t time.Time) string {
	return DiffPrettyNUnits(t, time.Now(), DefaultNumberOfTimeUnits)
}

// DiffPrettyAllUnits returns an interval between two times in a pretty format (single string)
// for all units, it uses the func DiffPrettyNUnits() with the MaxNumberOfTimeUnits const
func DiffPrettyAllUnits(a, b time.Time) string {
	return DiffPrettyNUnits(a, b, MaxNumberOfTimeUnits)
}

// DiffPrettyNUnits returns an interval between two times in a pretty format (single string)
// depending on the provided number of units this function will return 1-All time unit(s),
//
// e.g. when the interval has only days, hours, minutes and seconds:
//   * (all units) 1 day, 7 hours, 40 minutes, 49 seconds
//   * (3 units) 1 day, 7 hours,
//   * (1 units) 1 day
// e.g. when the interval has all units:
//   * (all units) 1 year, 4 months, 1 day, 7 hours, 40 minutes, 49 seconds
//   * (3 units) 1 year, 4 months, 1 day
//   * (1 units) 1 year
func DiffPrettyNUnits(a, b time.Time, numberOfUnits int) string {
	var (
		pretty       = ""
		separator    = " "
		unitCounter  = 0
		applied      bool
		addSeparator = func(str string) string {
			if str != "" {
				return str + separator
			}
			return str
		}
		addTimeUnit = func(str, unit string, value int) (string, bool) {
			if value > 0 {
				str = addSeparator(str)
				if value == 1 { // singular
					return fmt.Sprintf("%s%d %s", str, value, unit), true
				}
				// plural
				return fmt.Sprintf("%s%d %ss", str, value, unit), true
			}
			return str, false
		}
		year, month, day, hour, min, sec = Diff(a, b)

		// @afiune order here matters!
		// we do not use go maps since they don't have order, read more about it at:
		// https://blog.golang.org/go-maps-in-action
		timeValues = []int{year, month, day, hour, min, sec}
		timeUnits  = []string{"year", "month", "day", "hour", "minute", "second"}
	)

	for k := range timeUnits {
		pretty, applied = addTimeUnit(pretty, timeUnits[k], timeValues[k])
		if applied {
			unitCounter++
			if unitCounter >= numberOfUnits {
				return pretty
			}
		}
	}

	return pretty
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
