package relaxting

import (
	"errors"
	"fmt"
	"time"

	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/sirupsen/logrus"
)

type DateRange struct {
	StartTime time.Time
	EndTime   time.Time
}

type Straddle struct {
	left  *DateRange
	right *DateRange
}

const (
	WholeCalMonthBadDateRangeErrMsg   string = "bad date range. for computing whole calendar months, end date must be greater than or equal to start date and start year and end year must be equal"
	StartDateGreaterThanEndDateErrMsg string = "bad date range. end date must be greater than or equal to start date"
	RestOfDaysDiffMonthOrYearErrMsg   string = "restOfDays only computes indices within the same month. spanning months not supported"
	TimeParseError                    string = "could not parse %s error: %s.  please check format of date"
	CompDailySumIndexPrefix           string = mappings.IndexNameSum + "-"
	CompDailyRepIndexPrefix           string = mappings.IndexNameRep + "-"
	ComplianceDailySumTwenty          string = CompDailySumIndexPrefix + "20*"
	ComplianceDailyRepTwenty          string = CompDailyRepIndexPrefix + "20*"
	CompProfilesIndex                 string = mappings.IndexNameProf
)

type nowFuncT func() time.Time

var NowFunc nowFuncT

func init() {
	ResetClockImplementation()
}

func ResetClockImplementation() {
	NowFunc = func() time.Time {
		return time.Now()
	}
}

// function to return current time stamp in UTC
func now() time.Time {
	return NowFunc().UTC()
}

//IndexDates takes a start time and end time, both as strings,  formatted as RFC3339 UTC, and returns a comma separated list of strings, each of which is an ES index wildcard.
func IndexDates(prefix string, startTimeAsStringRFC3338 string, endTimeAsStringRFC3338 string) (indices string, err error) {
	var startTime, endTime time.Time

	if len(startTimeAsStringRFC3338) == 0 {
		startTime = time.Date(2017, time.January, 1, 0, 0, 0, 0, time.UTC)
	} else {
		startTime, err = time.Parse(time.RFC3339, startTimeAsStringRFC3338)
		if err != nil {
			logrus.Error(err)
			err = errors.New(fmt.Sprintf(TimeParseError, "startTime", startTimeAsStringRFC3338))
			return indices, err
		}
	}

	if len(endTimeAsStringRFC3338) == 0 {
		endTime = now().UTC()
	} else {
		endTime, err = time.Parse(time.RFC3339, endTimeAsStringRFC3338)
		if err != nil {
			logrus.Error(err)
			err = errors.New(fmt.Sprintf(TimeParseError, "endTime", endTimeAsStringRFC3338))
			return indices, err
		}
	}

	logrus.Debugf("startTime: %s endTime: %s", startTime, endTime)
	if startTime.After(endTime) {
		err = errors.New(StartDateGreaterThanEndDateErrMsg)
		return indices, err
	}
	indicesYears, straddleYears, _ := wholeCalendarYears(prefix, startTime, endTime)
	indices += indicesYears
	logrus.Debugf("indices years %s", indices)

	indices = appendIndices(indices, processYearsStraddle(prefix, straddleYears.left))
	indices = appendIndices(indices, processYearsStraddle(prefix, straddleYears.right))

	return indices, err
}

func processYearsStraddle(prefix string, dateRange *DateRange) (indices string) {
	if dateRange != nil {
		logrus.Debugf("processYearsStraddle with startTime: %s endTime: %s", dateRange.StartTime, dateRange.EndTime)
		indicesMonths, straddleMonths, _ := wholeCalendarMonths(prefix, dateRange.StartTime, dateRange.EndTime)
		if len(indicesMonths) > 0 {
			indices = appendIndices(indices, indicesMonths)
		}
		if straddleMonths.left != nil {
			indicesDaysLeft, _ := restOfDays(prefix, straddleMonths.left.StartTime, straddleMonths.left.EndTime)
			indices = appendIndices(indices, indicesDaysLeft)
		}
		if straddleMonths.right != nil {
			indicesDaysRight, _ := restOfDays(prefix, straddleMonths.right.StartTime, straddleMonths.right.EndTime)
			indices = appendIndices(indices, indicesDaysRight)
		}
	}
	return indices
}

func wholeCalendarYears(prefix string, startTime time.Time, endTime time.Time) (indices string, straddle Straddle, err error) {
	if startTime.After(endTime) {
		err = errors.New(StartDateGreaterThanEndDateErrMsg)
		return indices, straddle, err
	}
	endOfStartYear := time.Date(startTime.Year(), time.December, 31, 0, 0, 0, 0, time.UTC)

	firstWholeCalYear := startTime.Year()
	if !(startTime.Day() == 1 && startTime.Month() == 1 && (endTime.Equal(endOfStartYear) || endTime.After(endOfStartYear))) {
		firstWholeCalYear++ //exclude from full year
		straddle.left = &DateRange{StartTime: startTime}

		if endTime.After(endOfStartYear) {
			straddle.left.EndTime = endOfStartYear
		} else {
			straddle.left.EndTime = endTime
		}
	}

	lastWholeCalYear := endTime.Year()
	if endTime.Year() == startTime.Year() || endTime.Day() != 31 || endTime.Month() != time.December {
		lastWholeCalYear-- //exclude from full year

		if endTime.After(endOfStartYear) {
			startOfEndYear := time.Date(endTime.Year(), time.January, 1, 0, 0, 0, 0, time.UTC)
			straddle.right = &DateRange{StartTime: startOfEndYear, EndTime: endTime}
		}
	}

	for year := firstWholeCalYear; year <= lastWholeCalYear; year++ {
		indices = appendIndices(indices, fmt.Sprintf("%s%d*", prefix, year))
	}

	return indices, straddle, err
}

func wholeCalendarMonths(prefix string, startTime time.Time, endTime time.Time) (indices string, straddle Straddle, err error) {
	if startTime.After(endTime) || startTime.Year() != endTime.Year() {
		err = errors.New(WholeCalMonthBadDateRangeErrMsg)
		return indices, straddle, err
	}

	endOfStartMonth := time.Date(startTime.Year(), startTime.Month(), daysIn(startTime.Month(), startTime.Year()),
		0, 0, 0, 0, time.UTC)

	logrus.Debugf("wholeCalendarMonths with startTime: %s endTime: %s endOfStartMonth %s", startTime, endTime, endOfStartMonth)

	firstWholeCalMonth := startTime.Month()
	if !(startTime.Day() == 1 && (endTime.Equal(endOfStartMonth) || endTime.After(endOfStartMonth))) {
		firstWholeCalMonth++ //exclude from full month
		straddle.left = &DateRange{StartTime: startTime}

		if endTime.After(endOfStartMonth) {
			straddle.left.EndTime = endOfStartMonth
		} else {
			straddle.left.EndTime = endTime
		}
	}

	lastWholeCalMonth := endTime.Month()
	if endTime.Month() == startTime.Month() || endTime.Day() != daysIn(endTime.Month(), endTime.Year()) {
		if endTime.Month() != startTime.Month() {
			lastWholeCalMonth-- //exclude from full month
			if endTime.After(endOfStartMonth) {
				startOfEndMonth := time.Date(endTime.Year(), endTime.Month(), 1, 0, 0, 0, 0, time.UTC)
				straddle.right = &DateRange{StartTime: startOfEndMonth, EndTime: endTime}
			}
		}
	}

	for month := firstWholeCalMonth; month <= lastWholeCalMonth; month++ {
		indexFormat := "%s%d.%d*"
		if month < 10 {
			indexFormat = "%s%d.0%d*"
		}
		indices = appendIndices(indices, fmt.Sprintf(indexFormat, prefix, startTime.Year(), month))
	}

	return indices, straddle, err
}

func ones(prefix string, startTime time.Time, endTime time.Time, indexFormat string) (indices string) {
	if startTime.Day() == 1 && endTime.Day() >= 9 {
		indices = appendIndices(indices, fmt.Sprintf(indexFormat+".0*", prefix, startTime.Year(), int(startTime.Month())))
		logrus.Debugf("ones (all): %s", indices)
		return indices
	}

	if startTime.Day() < 10 {
		daysToAdd := 0
		if endTime.Day() >= 10 {
			daysToAdd = 9 - startTime.Day()
		} else {
			daysToAdd = endTime.Day() - startTime.Day()
		}
		for i := startTime.Day(); i <= startTime.Day()+daysToAdd; i++ {
			indices = appendIndices(indices, fmt.Sprintf(indexFormat+".0%d*", prefix, startTime.Year(), int(startTime.Month()), i))
		}
	}
	logrus.Debugf("ones (some): %s", indices)

	return indices
}

func tens(prefix string, startTime time.Time, endTime time.Time, indexFormat string) (indices string) {
	startRange := 10
	endRange := 19

	if startTime.Day() <= startRange && endTime.Day() >= endRange {
		indices = appendIndices(indices, fmt.Sprintf(indexFormat+".1*", prefix, startTime.Year(), int(startTime.Month())))
		logrus.Debugf("tens (all): %s", indices)

		return indices
	}

	if startTime.Day() <= endRange && endTime.Day() >= startRange {
		if startTime.Day() > startRange {
			startRange = startTime.Day()
		}
		if endTime.Day() < endRange {
			endRange = endTime.Day()
		}

		for i := startRange; i <= endRange; i++ {
			indices = appendIndices(indices, fmt.Sprintf(indexFormat+".%d*", prefix, startTime.Year(), int(startTime.Month()), i))
		}
	}

	logrus.Debugf("tens (some): %s", indices)
	return indices
}

func twenties(prefix string, startTime time.Time, endTime time.Time, indexFormat string) (indices string) {
	startRange := 20
	endRange := 29

	if startTime.Month() == time.February {
		daysInMonth := daysIn(startTime.Month(), startTime.Year())
		if daysInMonth < endRange {
			//it's not leap year, adjust accordingly
			endRange = daysInMonth
		}
	}

	if startTime.Day() <= startRange && endTime.Day() >= endRange {
		indices = appendIndices(indices, fmt.Sprintf(indexFormat+".2*", prefix, startTime.Year(), int(startTime.Month())))
		logrus.Debugf("twenties (all): %s", indices)
		return indices
	}

	if startTime.Day() <= endRange && endTime.Day() >= startRange {
		if startTime.Day() > startRange {
			startRange = startTime.Day()
		}
		if endTime.Day() < endRange {
			endRange = endTime.Day()
		}

		for i := startRange; i <= endRange; i++ {
			indices = appendIndices(indices, fmt.Sprintf(indexFormat+".%d*", prefix, startTime.Year(), int(startTime.Month()), i))
		}
	}
	logrus.Debugf("twenties (some): %s", indices)
	return indices
}

func thirties(prefix string, startTime time.Time, endTime time.Time, indexFormat string) (indices string) {
	startRange := 30
	endRange := 31

	logrus.Debugf("starttime: %s, endtime: %s", startTime, endTime)
	if startTime.Day() <= startRange && endTime.Day() >= endRange {
		indices = appendIndices(indices, fmt.Sprintf(indexFormat+".3*", prefix, startTime.Year(), int(startTime.Month())))
		logrus.Debugf("thirties (all): %s", indices)
		return indices
	}

	if startTime.Day() <= endRange && endTime.Day() >= startRange {
		if startTime.Day() > startRange {
			startRange = startTime.Day()
		}
		if endTime.Day() < endRange {
			endRange = endTime.Day()
		}

		for i := startRange; i <= endRange; i++ {
			indices = appendIndices(indices, fmt.Sprintf(indexFormat+".%d*", prefix, startTime.Year(), int(startTime.Month()), i))
		}
	}
	logrus.Debugf("thirties (some): %s", indices)
	return indices
}

func restOfDays(prefix string, startTime time.Time, endTime time.Time) (indices string, err error) {
	if startTime.Month() != endTime.Month() || startTime.Year() != endTime.Year() {
		return indices, errors.New(RestOfDaysDiffMonthOrYearErrMsg)
	}

	var indexFormat string
	if startTime.Month() < 10 {
		indexFormat = "%s%d.0%d"
	} else {
		indexFormat = "%s%d.%d"
	}

	logrus.Debugf("starttime: %s, endtime: %s", startTime, endTime)

	indices = appendIndices(indices, ones(prefix, startTime, endTime, indexFormat))
	indices = appendIndices(indices, tens(prefix, startTime, endTime, indexFormat))
	indices = appendIndices(indices, twenties(prefix, startTime, endTime, indexFormat))
	indices = appendIndices(indices, thirties(prefix, startTime, endTime, indexFormat))

	return indices, err
}

// daysIn returns the number of days in a month for a given year.
func daysIn(m time.Month, year int) int {
	// This is equivalent to time.daysIn(m, year).
	return time.Date(year, m+1, 0, 0, 0, 0, 0, time.UTC).Day()
}

func appendIndices(indicesToAppendTo string, indicesToAppend string) string {
	if len(indicesToAppend) > 0 {
		if len(indicesToAppendTo) > 0 {
			return indicesToAppendTo + "," + indicesToAppend
		}
		return indicesToAppend
	}
	return indicesToAppendTo
}
