package relaxting_test

import (
	"testing"

	"fmt"

	"time"

	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/stretchr/testify/assert"
)

func TestIndexDatesBadRange(t *testing.T) {
	startTime := "2006-01-02T15:04:05Z"
	endTime := "2005-03-02T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	assert.EqualError(t, err, relaxting.StartDateGreaterThanEndDateErrMsg, "An error was expected")
	assert.Equal(t, "", esIndex)
}

func TestIndexDatesUnparsableStartTime(t *testing.T) {
	unparsableStartTime := "20061-01-02T15:04:05Z"
	endTime := "2005-03-02T22:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, unparsableStartTime, endTime)

	assert.EqualError(t, err, fmt.Sprintf(relaxting.TimeParseError, "startTime", unparsableStartTime),
		"An error was expected")
	assert.Equal(t, "", esIndex)
}

func TestIndexDatesUnparsableEndTime(t *testing.T) {
	startTime := "2006-01-02T15:04:05Z"
	unparsableEndTime := "20051-03-02T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, unparsableEndTime)

	assert.EqualError(t, err, fmt.Sprintf(relaxting.TimeParseError, "endTime", unparsableEndTime),
		"An error was expected")
	assert.Equal(t, "", esIndex)
}

func TestIndexDatesNoEndTimeYesStartTime(t *testing.T) {
	startTime := "2006-01-02T15:04:05Z"
	emptyEndTime := ""

	defer relaxting.ResetClockImplementation()

	relaxting.NowFunc = func() time.Time {
		return time.Date(2017, 11, 17, 0, 0, 00, 0, time.UTC)
	}

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, emptyEndTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2007*,%[1]s2008*,%[1]s2009*,%[1]s2010*,%[1]s2011*,"+
			"%[1]s2012*,%[1]s2013*,%[1]s2014*,%[1]s2015*,%[1]s2016*,%[1]s2006.02*,%[1]s2006.03*,%[1]s2006.04*,"+
			"%[1]s2006.05*,%[1]s2006.06*,%[1]s2006.07*,%[1]s2006.08*,%[1]s2006.09*,%[1]s2006.10*,%[1]s2006.11*,"+
			"%[1]s2006.12*,%[1]s2006.01.02*,%[1]s2006.01.03*,%[1]s2006.01.04*,%[1]s2006.01.05*,%[1]s2006.01.06*,"+
			"%[1]s2006.01.07*,%[1]s2006.01.08*,%[1]s2006.01.09*,%[1]s2006.01.1*,%[1]s2006.01.2*,%[1]s2006.01.3*,"+
			"%[1]s2017.01*,%[1]s2017.02*,%[1]s2017.03*,%[1]s2017.04*,%[1]s2017.05*,%[1]s2017.06*,%[1]s2017.07*,"+
			"%[1]s2017.08*,%[1]s2017.09*,%[1]s2017.10*,%[1]s2017.11.0*,%[1]s2017.11.10*,%[1]s2017.11.11*,"+
			"%[1]s2017.11.12*,%[1]s2017.11.13*,%[1]s2017.11.14*,%[1]s2017.11.15*,%[1]s2017.11.16*,%[1]s2017.11.17*",
			relaxting.CompDailySumIndexPrefix), esIndex)
	}
}

func TestIndexDatesYesEndTimeNoStartTime(t *testing.T) {
	emptyStartTime := ""
	endTime := "2018-01-02T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, emptyStartTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2017*,%[1]s2018.01.01*,%[1]s2018.01.02*",
			relaxting.CompDailySumIndexPrefix), esIndex)
	}
}

func TestIndexDatesNoEndTimeNoStartTime(t *testing.T) {
	emptyStartTime := ""
	endTime := ""

	defer relaxting.ResetClockImplementation()

	relaxting.NowFunc = func() time.Time {
		return time.Date(2017, 11, 17, 0, 0, 00, 0, time.UTC)
	}

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, emptyStartTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2017.01*,%[1]s2017.02*,%[1]s2017.03*,%[1]s2017.04*,%[1]s2017.05*,"+
			"%[1]s2017.06*,%[1]s2017.07*,%[1]s2017.08*,%[1]s2017.09*,%[1]s2017.10*,%[1]s2017.11.0*,%[1]s2017.11.10*,"+
			"%[1]s2017.11.11*,%[1]s2017.11.12*,%[1]s2017.11.13*,%[1]s2017.11.14*,%[1]s2017.11.15*,%[1]s2017.11.16*,"+
			"%[1]s2017.11.17*", relaxting.CompDailySumIndexPrefix), esIndex)
	}
}

func TestIndexDatesSameDay(t *testing.T) {
	startTime := "2006-01-02T15:04:05Z"
	endTime := "2006-01-02T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2006.01.02*", relaxting.CompDailySumIndexPrefix), esIndex)
	}
}

func TestIndexDatesStartTimeIsFirstDayOfMonth(t *testing.T) {
	startTime := "2006-10-01T15:04:05Z"
	endTime := "2006-10-11T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2006.10.0*,%[1]s2006.10.10*,%[1]s2006.10.11*",
			relaxting.CompDailySumIndexPrefix), esIndex)
	}
}

func TestIndexDatesEndDateIsThirtiethOnAMonthWithThirtyOneDays(t *testing.T) {
	startTime := "2006-01-28T15:04:05Z"
	endTime := "2006-01-30T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2006.01.28*,%[1]s2006.01.29*,%[1]s2006.01.30*",
			relaxting.CompDailySumIndexPrefix),
			esIndex)
	}
}

func TestIndexDatesEndDateIsThirtyFirstOnAMonthWithThirtyOneDays(t *testing.T) {
	startTime := "2006-01-28T15:04:05Z"
	endTime := "2006-01-31T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2006.01.28*,%[1]s2006.01.29*,%[1]s2006.01.3*",
			relaxting.CompDailySumIndexPrefix),
			esIndex)
	}
}

func TestIndexDatesEndsWithWholeMonthAndNoMoreAfter(t *testing.T) {
	startTime := "2017-12-28T15:04:05Z"
	endTime := "2018-01-31T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2017.12.28*,%[1]s2017.12.29*,%[1]s2017.12.3*,%[1]s2018.01*",
			relaxting.CompDailySumIndexPrefix),
			esIndex)
	}
}

func TestIndexDatesNoStartDateAndEndsWithWholeMonthAndNoMoreAfter(t *testing.T) {
	startTime := ""
	endTime := "2018-01-31T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2017*,%[1]s2018.01*", relaxting.CompDailySumIndexPrefix), esIndex)
	}
}

func TestIndexDatesStartAndEndDateIsThirtyFirst(t *testing.T) {
	startTime := "2006-01-31T15:04:05Z"
	endTime := "2006-01-31T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2006.01.31*", relaxting.CompDailySumIndexPrefix), esIndex)
	}
}

func TestIndexDatesEndDateIsEndOfTensRange(t *testing.T) {
	startTime := "2006-01-02T15:04:05Z"
	endTime := "2006-01-19T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2006.01.02*,%[1]s2006.01.03*,%[1]s2006.01.04*,%[1]s2006.01.05*,"+
			"%[1]s2006.01.06*,%[1]s2006.01.07*,%[1]s2006.01.08*,%[1]s2006.01.09*,%[1]s2006.01.1*",
			relaxting.CompDailySumIndexPrefix), esIndex)
	}
}

func TestIndexDatesStartDateIsAfterStartOfTensRangeAndEndDateIsEndOfTensRange(t *testing.T) {
	startTime := "2006-01-18T15:04:05Z"
	endTime := "2006-01-19T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2006.01.18*,%[1]s2006.01.19*", relaxting.CompDailySumIndexPrefix),
			esIndex)
	}
}

func TestIndexDatesStartDateIsAfterStartOfTwentiesRangeAndEndDateIsEndOfTwentiesRange(t *testing.T) {
	startTime := "2006-01-28T15:04:05Z"
	endTime := "2006-01-29T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2006.01.28*,%[1]s2006.01.29*", relaxting.CompDailySumIndexPrefix),
			esIndex)
	}
}

func TestIndexDatesEndDateIsAfterEndOfTensRange(t *testing.T) {
	startTime := "2006-01-02T15:04:05Z"
	endTime := "2006-01-20T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2006.01.02*,%[1]s2006.01.03*,%[1]s2006.01.04*,%[1]s2006.01.05*,"+
			"%[1]s2006.01.06*,%[1]s2006.01.07*,%[1]s2006.01.08*,%[1]s2006.01.09*,%[1]s2006.01.1*,%[1]s2006.01.20*",
			relaxting.CompDailySumIndexPrefix), esIndex)
	}
}

func TestIndexDatesStartAndEndDateIsTwentyNinth(t *testing.T) {
	startTime := "2006-01-29T15:04:05Z"
	endTime := "2006-01-29T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2006.01.29*", relaxting.CompDailySumIndexPrefix),
			esIndex)
	}
}

func TestIndexDatesStartAndEndDateIsNinth(t *testing.T) {
	startTime := "2006-01-09T15:04:05Z"
	endTime := "2006-01-09T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2006.01.09*", relaxting.CompDailySumIndexPrefix),
			esIndex)
	}
}

func TestIndexDatesStartMonthDoubleDigitMonth(t *testing.T) {
	startTime := "2006-10-02T15:04:05Z"
	endTime := "2007-01-02T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2006.11*,%[1]s2006.12*,%[1]s2006.10.02*,%[1]s2006.10.03*,"+
			"%[1]s2006.10.04*,%[1]s2006.10.05*,%[1]s2006.10.06*,%[1]s2006.10.07*,%[1]s2006.10.08*,%[1]s2006.10.09*,"+
			"%[1]s2006.10.1*,%[1]s2006.10.2*,%[1]s2006.10.3*,%[1]s2007.01.01*,%[1]s2007.01.02*",
			relaxting.CompDailySumIndexPrefix), esIndex)
	}
}

func TestIndexDatesSpanTwoYearsButOnlyOneWholeCalYear(t *testing.T) {
	startTime := "2006-01-02T15:04:05Z"
	endTime := "2008-03-02T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2007*,%[1]s2006.02*,%[1]s2006.03*,%[1]s2006.04*,%[1]s2006.05*,"+
			"%[1]s2006.06*,%[1]s2006.07*,%[1]s2006.08*,%[1]s2006.09*,%[1]s2006.10*,%[1]s2006.11*,%[1]s2006.12*,"+
			"%[1]s2006.01.02*,%[1]s2006.01.03*,%[1]s2006.01.04*,%[1]s2006.01.05*,%[1]s2006.01.06*,%[1]s2006.01.07*,"+
			"%[1]s2006.01.08*,%[1]s2006.01.09*,%[1]s2006.01.1*,%[1]s2006.01.2*,%[1]s2006.01.3*,%[1]s2008.01*,"+
			"%[1]s2008.02*,%[1]s2008.03.01*,%[1]s2008.03.02*", relaxting.CompDailySumIndexPrefix), esIndex)
	}
}

func TestIndexDatesSpanTwoYearsStartJan1(t *testing.T) {
	startTime := "2006-01-01T15:04:05Z"
	endTime := "2008-03-02T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2006*,%[1]s2007*,%[1]s2008.01*,%[1]s2008.02*,%[1]s2008.03.01*,"+
			"%[1]s2008.03.02*", relaxting.CompDailySumIndexPrefix), esIndex)
	}
}

func TestIndexDatesLeapYear(t *testing.T) {
	startTime := "2016-02-21T15:04:05Z"
	endTime := "2016-03-02T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2016.02.21*,%[1]s2016.02.22*,%[1]s2016.02.23*,%[1]s2016.02.24*,"+
			"%[1]s2016.02.25*,%[1]s2016.02.26*,%[1]s2016.02.27*,%[1]s2016.02.28*,%[1]s2016.02.29*,%[1]s2016.03.01*,"+
			"%[1]s2016.03.02*", relaxting.CompDailySumIndexPrefix), esIndex)
	}
}

func TestIndexDatesNonLeapYear(t *testing.T) {
	startTime := "2015-02-21T15:04:05Z"
	endTime := "2016-03-02T15:04:05Z"

	esIndex, err := relaxting.IndexDates(relaxting.CompDailySumIndexPrefix, startTime, endTime)

	if assert.NoError(t, err) {
		assert.Equal(t, fmt.Sprintf("%[1]s2015.03*,%[1]s2015.04*,%[1]s2015.05*,%[1]s2015.06*,%[1]s2015.07*,"+
			"%[1]s2015.08*,%[1]s2015.09*,%[1]s2015.10*,%[1]s2015.11*,%[1]s2015.12*,%[1]s2015.02.21*,%[1]s2015.02.22*,"+
			"%[1]s2015.02.23*,%[1]s2015.02.24*,%[1]s2015.02.25*,%[1]s2015.02.26*,%[1]s2015.02.27*,%[1]s2015.02.28*,"+
			"%[1]s2016.01*,%[1]s2016.02*,%[1]s2016.03.01*,%[1]s2016.03.02*", relaxting.CompDailySumIndexPrefix),
			esIndex)
	}
}
