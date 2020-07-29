package util

import (
	"strings"
	"testing"
	"unicode/utf8"

	"time"

	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/golang/protobuf/ptypes"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
)

const (
	csvHeader = "Node Name,End Time,Platform Name,Platform Release,Environment,IPAddress,FQDN,Profile Name,Profile Title,Profile Version,Profile Summary,Control ID,Control Title,Control Impact,Waived (true/false),Result Status,Result Run Time,Result Code Description,Result Message,Result Skip Message,Waiver Justification,Waiver Expiration\n"
)

func TestMaxCharLimit(t *testing.T) {
	var testString string
	my1kstring := "m3dVoHqxd5CTZFdCfjstCuKaiwAaXVgvSqYvJI4nkI5vBQUyzr2EJXyPOVzezT4Bb8T9CpyeoRknGLpvdKt2iKCtBDzyiH6Oys8sqzCCPoHAReOiZxfCqT9k9JcgEy4tudvIHNfEuqbkC1wmcxqANjlEwZi8rlWn1GR22d03rhvsYM8IbwHVtCMXbZ2vIrz0fYA4VAatq6RyQRyJlt1kSAnFSiWaNfz3ECwY7QcQ6rbFunKi4EMwttGl8nTlpEuDrNt0IMCEuUFIlpb0muR64nXa4komXz6iySpf7O4YjpFBOfw2YSGae98Np0vHy0s8XqUaGQD3gunU15lFK0e9AoOnTP3yU6yDXy93DK80HYbDWEu1kuq6YOHBunpfFfEgzRRD2wi5qcg61IYsAKNJHe2nUlxyVFiUKYE5d8jCFA2CtdP4BVT80cI4e747BwT1CqnqJD55Ps1IfMvKmNwENzve9JMSwT8g855zGT7r6q6E1FPl0NoBr0CVNGxJOBByRFBV2LO33gTtIoRwJ8JgCDAFsYDeKVJvK6JZLRscTCwAnHeMeAKUoBn4izxuGl2PeansTVJzDPznzTAbSmFiWFlsx8JlgPYYwoK5RuY1w2VMYPLRDgHlbLX4utOcyRnAjRTQQ0dJ5zRuKMOwSx729lag5PgQljGNabVg6BnkECVSvTdcKsxUQ2IIED281TuzQzoA9T5xQ3QExBi5icfxFxGCtMJKDSKo0YRqQAaciZCXpDIaVv3YRtMTi1WwmzDEDhnid8w4V9gZAo0wBqPLGROxMR6UPe7eFjP3RqoczeAe7xHRIK6rX3bXM1ZoT3iGjjkmalgfrQYjuyKhBS0HBp5RutYqVz6oeeH1eBpBnqLkagDlduJdT8QDgn58oLyqYoC5FBcNdTs4ciimJGZanN3lq5DpIwIp5XgQQHmYUxujq9ufjsdZyLnNEjHHGUkVw8Q8rvEnXktTwunXqJURfRHMBIHpAFNB3ItwyLpv"
	for i := 1; i <= 32; i++ {
		testString = testString + my1kstring
	}

	// testString should be 32k chars
	logrus.Infof("char count in string: %d", utf8.RuneCountInString(testString))
	returnedString := maxCharLimit(testString)
	assert.Equal(t, testString, returnedString)

	testString = testString + "a"
	// testString should be 32,001 chars
	logrus.Infof("char count in string: %d", utf8.RuneCountInString(testString))
	returnedString = maxCharLimit(testString)
	assert.Equal(t, strings.HasPrefix(returnedString, "character limit exceeds 32,000. truncating message"), true)
	assert.Equal(t, utf8.RuneCountInString(returnedString) <= 32000, true)
}

func TestEmptyExport(t *testing.T) {
	data, err := ReportToCSV(&reportingapi.Report{})
	assert.Nil(t, err, "ReportsToCSV with empty report works.")
	assert.Equal(t, csvHeader, data, "ReportsToCSV with empty list of reports works.")
}

func TestNilExport(t *testing.T) {
	_, err := ReportToCSV(nil)
	assert.NotNil(t, err, "ReportsToCSV with nil report throws an error.")
}

func TestSimpleExport(t *testing.T) {
	report := getSampleReport()
	data, err := ReportToCSV(report)
	assert.Nil(t, err, "ReportToCSV with a simple report generates without errors.")

	// Please note: We test the format for RFC 4180 compliance and
	// we also test if commas are escaped properly!
	// See https://tools.ietf.org/html/rfc4180
	line := "Node1,0001-01-01T00:00:00Z,,,test-env,10.23.149.1,api.example.com,ProfileName,\"Profile, Title\",1.2.3,Profile summary,ControlID1,Control Title,0.12,false,passed,12345.000,super complex code,all done,we don't skip,,\n"
	assert.Equal(t, csvHeader+line, data, "ReportToCSV with empty simple report works.")
}

func getSampleReport() *reportingapi.Report {
	results := []*reportingapi.Result{
		{
			Status:      "passed",
			RunTime:     12345,
			CodeDesc:    "super complex code",
			Message:     "all done",
			SkipMessage: "we don't skip",
		}}
	controls := []*reportingapi.Control{
		{
			Id:      "ControlID1",
			Title:   "Control Title",
			Impact:  0.123,
			Results: results,
		}}
	profiles := []*reportingapi.Profile{
		{
			Name:     "ProfileName",
			Title:    "Profile, Title",
			Version:  "1.2.3",
			Summary:  "Profile summary",
			Controls: controls,
		}}

	endTime, _ := time.Parse("2006-01-02T15:04:05", "2018-02-09T09:18:41Z")
	endTimeTimestamp, _ := ptypes.TimestampProto(endTime)

	report := &reportingapi.Report{
		Id:          "ReportID",
		EndTime:     endTimeTimestamp,
		NodeId:      "ID1",
		NodeName:    "Node1",
		Profiles:    profiles,
		Environment: "test-env",
		Fqdn:        "api.example.com",
		Ipaddress:   "10.23.149.1",
	}
	return report
}

func TestExportWaiverData(t *testing.T) {
	report := getSampleReport()
	report.Profiles[0].Controls[0].WaivedStr = "yes_run"
	report.Profiles[0].Controls[0].WaiverData = &reportingapi.OrigWaiverData{
		ExpirationDate: "some-date",
		Justification:  "some reason",
	}

	data, err := ReportToCSV(report)
	assert.Nil(t, err, "ReportToCSV with a waiver report including waiver data generates without errors.")
	line := "Node1,0001-01-01T00:00:00Z,,,test-env,10.23.149.1,api.example.com,ProfileName,\"Profile, Title\",1.2.3,Profile summary,ControlID1,Control Title,0.12,true,passed,12345.000,super complex code,all done,we don't skip,some reason,some-date\n"
	assert.Equal(t, csvHeader+line, data, "ReportToCSV with a waiver report works.")
}
