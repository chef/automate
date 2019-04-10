package relaxting

import (
	"testing"
	"time"

	"os"

	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
)

const timeFormat string = time.RFC3339

func TestMain(m *testing.M) {
	logrus.SetLevel(logrus.InfoLevel)
	os.Exit(m.Run())
}

func TestRestOfDaysWithDiffMonthSameYear(t *testing.T) {
	startTime, err := time.Parse(timeFormat, "2015-02-21T15:04:05Z")
	if err != nil {
		logrus.Error(err)
		return
	}
	endTime, err := time.Parse(timeFormat, "2015-03-02T15:04:05Z")
	if err != nil {
		logrus.Error(err)
		return
	}

	esIndex, err := restOfDays(CompDailySumIndexPrefix, startTime, endTime)

	assert.EqualError(t, err, RestOfDaysDiffMonthOrYearErrMsg, "An error was expected")
	assert.Equal(t, "", esIndex)
}

func TestWholeCalendarYearBadRange(t *testing.T) {
	startTime, err := time.Parse(timeFormat, "2006-01-02T15:04:05Z")
	if err != nil {
		logrus.Error(err)
		return
	}
	endTime, err := time.Parse(timeFormat, "2005-03-02T15:04:05Z")
	if err != nil {
		logrus.Error(err)
		return
	}

	esIndex, straddle, err := wholeCalendarYears(CompDailySumIndexPrefix, startTime, endTime)

	assert.EqualError(t, err, StartDateGreaterThanEndDateErrMsg, "An error was expected")
	assert.Nil(t, straddle.right)
	assert.Nil(t, straddle.left)
	assert.Equal(t, "", esIndex)
}

func TestWholeCalendarMonthBadRange(t *testing.T) {
	startTime, err := time.Parse(timeFormat, "2006-01-02T15:04:05Z")
	if err != nil {
		logrus.Error(err)
		return
	}
	endTime, err := time.Parse(timeFormat, "2005-03-02T15:04:05Z")
	if err != nil {
		logrus.Error(err)
		return
	}

	esIndex, straddle, err := wholeCalendarMonths(CompDailySumIndexPrefix, startTime, endTime)

	assert.EqualError(t, err, WholeCalMonthBadDateRangeErrMsg, "An error was expected")
	assert.Nil(t, straddle.right)
	assert.Nil(t, straddle.left)
	assert.Equal(t, "", esIndex)
}

func TestWholeCalendarMonthExactlyWholeMonths(t *testing.T) {
	startTime, err := time.Parse(timeFormat, "2018-01-01T00:04:05Z")
	if err != nil {
		logrus.Error(err)
		return
	}
	endTime, err := time.Parse(timeFormat, "2018-01-31T15:04:05Z")
	if err != nil {
		logrus.Error(err)
		return
	}

	esIndex, straddle, err := wholeCalendarMonths(CompDailySumIndexPrefix, startTime, endTime)
	if assert.NoError(t, err) {
		assert.Equal(t, CompDailySumIndexPrefix+"2018.01*", esIndex)
	}

	assert.Nil(t, straddle.right)
	assert.Nil(t, straddle.left)
}

func TestNowOverride(t *testing.T) {
	assert.WithinDuration(t, now(), time.Now().UTC(), 10*time.Millisecond)

	testTime := time.Date(2017, 11, 17, 0, 0, 00, 0, time.UTC)
	NowFunc = func() time.Time {
		return testTime
	}

	assert.Equal(t, now(), testTime)

	ResetClockImplementation()

	//make sure calling ResetClockImplementation sets time clock back to normal
	assert.WithinDuration(t, now(), time.Now().UTC(), 10*time.Millisecond)
}
