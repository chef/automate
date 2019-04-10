package util

import (
	"testing"

	"time"

	reportingapi "github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
)

const (
	csvHeader = "Node Name,End Time,Platform Name,Platform Release,Environment,IPAddress,FQDN,Profile Name,Profile Title,Profile Version,Profile Summary,Control ID,Control Title,Control Impact,Result Status,Result Run Time,Result Code Description,Result Message,Result Skip Message\n"
)

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
	data, err := ReportToCSV(report)
	assert.Nil(t, err, "ReportToCSV with a simple report generates without errors.")

	// Please note: We test the format for RFC 4180 compliance and
	// we also test if commas are escaped properly!
	// See https://tools.ietf.org/html/rfc4180
	line := "Node1,0001-01-01T00:00:00Z,,,test-env,10.23.149.1,api.example.com,ProfileName,\"Profile, Title\",1.2.3,Profile summary,ControlID1,Control Title,0.12,passed,12345.000,super complex code,all done,we don't skip\n"
	assert.Equal(t, csvHeader+line, data, "ReportToCSV with empty simple report works.")
}
