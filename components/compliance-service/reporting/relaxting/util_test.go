package relaxting

import (
	"testing"
	"time"

	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/stretchr/testify/assert"
)

var stringError = "computeIndexDate - could not parse end_time %s"

func TestGetEsIndexNoTimeLast24h(t *testing.T) {
	sum := mappings.IndexNameSum
	filters := make(map[string][]string)

	todayTime := time.Now().UTC()

	index, err := GetEsIndex(filters, true)
	assert.NoError(t, err)

	today := todayTime.Format("2006.01.02")
	if todayTime.Day() == 31 {
		today = todayTime.Format("2006.01.")
		//if it's the 31st, shave off the 1 and make it 3*, which will be the 30th and 31st
		assert.Equal(t, sum+"-"+today+"3*", index)
	} else {
		yesterday := todayTime.Add(-24 * time.Hour).Format("2006.01.02")
		assert.Equal(t, sum+"-"+yesterday+"*,"+sum+"-"+today+"*", index)
	}
}

func TestGetEsIndexWithOnlyEndTimeFilter(t *testing.T) {
	sum := mappings.IndexNameSum
	rep := mappings.IndexNameRep

	filters := make(map[string][]string)
	filters["profile_id"] = []string{"prof1", "prof2"}
	filters["end_time"] = []string{"2019-01-05T23:59:59Z", "2019-01-03T23:59:59Z"} // 2019-01-06 should be ignored

	index, err := GetEsIndex(filters, true)
	assert.Equal(t, sum+"-2019.01.05*", index)
	assert.NoError(t, err)

	index, err = GetEsIndex(filters, false)
	assert.Equal(t, rep+"-2019.01.05*", index)
	assert.NoError(t, err)
}

func TestGetEsIndexWithStartAndEndTime(t *testing.T) {
	// Let debug logs show up when running tests
	//logrus.SetLevel(logrus.DebugLevel)

	rep := mappings.IndexNameRep

	filters := make(map[string][]string)
	filters["profile_id"] = []string{"prof1", "prof2"}
	filters["start_time"] = []string{"2100-01-12T23:59:59Z"}
	filters["end_time"] = []string{"2019-01-05T23:59:59Z", "2019-01-06T23:59:59Z"} // 2019-01-06 should be ignore

	index, err := GetEsIndex(filters, false)
	assert.EqualError(t, err, "bad date range. end date must be greater than or equal to start date")

	filters["start_time"] = []string{"2019-01-02T23:59:59Z", "2019-01-03T23:59:59Z"}
	index, err = GetEsIndex(filters, false)
	assert.Equal(t, rep+"-2019.01.02*,"+rep+"-2019.01.03*,"+rep+"-2019.01.04*,"+rep+"-2019.01.05*", index)
	assert.NoError(t, err)
}

func TestFetchLatestDataOrNot(t *testing.T) {
	filters := make(map[string][]string)
	filters["start_time"] = []string{"2019-01-02T23:59:59Z"}
	filters["end_time"] = []string{"2019-03-03T23:59:59Z"}
	latestOnly := FetchLatestDataOrNot(filters)
	assert.Equal(t, true, latestOnly)

	filters["job_id"] = []string{"job 1"}
	latestOnly = FetchLatestDataOrNot(filters)
	assert.Equal(t, false, latestOnly)
}

func TestGetRepOrSumIndexWithStartTimeAndEndTimeParsingError(t *testing.T) {
	filters := make(map[string][]string)
	filters["end_time"] = []string{"2019-03-05T23:59oiZ", "2019-03-05T23:59oiZ"}

	_, err := GetEsIndex(filters, true)
	assert.Error(t, err, stringError)

	_, err = GetEsIndex(filters, false)
	assert.Error(t, err, stringError)

}

func TestGetControlIndexWithStartTimeAndEndTime(t *testing.T) {
	mapping := mappings.IndexNameControl

	filters := make(map[string][]string)
	filters["end_time"] = []string{"2019-02-03T23:59:59Z", "2019-02-07T23:59:59Z"} // 2019-01-06 should be ignored

	index, err := getControlIndex(filters)
	assert.Equal(t, mapping+"-2019.02.03*", index)
	assert.NoError(t, err)
}

func TestGetControlIndexWithEndTimeWithParsingError(t *testing.T) {
	filters := make(map[string][]string)
	filters["end_time"] = []string{"2019-01-05T23:59oiZ", "2019-01-06T23:59oiZ"}

	_, err := getControlIndex(filters)

	assert.Error(t, err, stringError)
}

func TestGetControlIndexWithStartTimeWithParsingError(t *testing.T) {
	filters := make(map[string][]string)
	filters["start_time"] = []string{"2019-01-04T23:59oiZ", "2019-01-06T23:59oiZ"}

	_, err := getControlIndex(filters)

	assert.Error(t, err, stringError)
}
