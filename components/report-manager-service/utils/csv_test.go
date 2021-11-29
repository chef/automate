package utils

import (
	"testing"

	"time"

	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
)

var (
	csvHeader = [][]string{
		{"Node Name", "End Time", "Platform Name", "Platform Release", "Environment", "IPAddress", "FQDN", "Profile Name", "Profile Title", "Profile Version", "Profile Summary", "Control ID", "Control Title", "Control Impact", "Waived (true/false)", "Result Status", "Result Run Time", "Result Code Description", "Result Message", "Result Skip Message", "Waiver Justification", "Waiver Expiration"},
	}
)

func getCSVHeader() [][]string {
	return csvHeader
}

func TestEmptyExport(t *testing.T) {
	data, err := ReportToCSV(&reportingapi.Report{}, true)
	assert.NoError(t, err)
	expected := getCSVHeader()
	assert.Equal(t, expected, data)
}

func TestNilExport(t *testing.T) {
	_, err := ReportToCSV(nil, true)
	assert.Error(t, err)
	assert.Equal(t, "received nil to be converted to CSV", err.Error())
}

func TestSimpleExport(t *testing.T) {
	report := getSampleReport(t)
	data, err := ReportToCSV(report, true)
	assert.NoError(t, err)
	line := []string{"Node1", `2021-11-18 00:00:00 +0000 UTC`, "", "", "test-env", "test.ip.address", "api.example.com", "ProfileName", `Profile, Title`, "1.2.3", "Profile summary", "ControlID1", "Control Title", "0.12", "false", "passed", "12345.000", "super complex code", "all done", `we don't skip`, "", ""}
	expected := getCSVHeader()
	expected = append(expected, line)
	assert.Equal(t, expected, data, "ReportToCSV with empty simple report works.")
}

func getSampleReport(t *testing.T) *reportingapi.Report {
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

	endTime, err := time.Parse("2006-01-02", "2021-11-18")
	assert.NoError(t, err)
	endTimeTimestamp, _ := ptypes.TimestampProto(endTime)

	report := &reportingapi.Report{
		Id:          "ReportID",
		EndTime:     endTimeTimestamp,
		NodeId:      "ID1",
		NodeName:    "Node1",
		Profiles:    profiles,
		Environment: "test-env",
		Fqdn:        "api.example.com",
		Ipaddress:   "test.ip.address",
	}
	return report
}

func TestExportWaiverData(t *testing.T) {
	report := getSampleReport(t)
	report.Profiles[0].Controls[0].WaivedStr = "yes_run"
	report.Profiles[0].Controls[0].WaiverData = &reportingapi.OrigWaiverData{
		ExpirationDate: "some-date",
		Justification:  "some reason",
	}
	data, err := ReportToCSV(report, true)
	assert.NoError(t, err, "ReportToCSV with a waiver report including waiver data generates without errors.")
	line := []string{"Node1", "2021-11-18 00:00:00 +0000 UTC", "", "", "test-env", "test.ip.address", "api.example.com", "ProfileName", `Profile, Title`, "1.2.3", "Profile summary", "ControlID1", "Control Title", "0.12", "true", "passed", "12345.000", `super complex code`, `all done`, `we don't skip`, "some reason", "some-date"}
	expected := getCSVHeader()
	expected = append(expected, line)
	assert.Equal(t, expected, data)
}

func TestTimeStampError(t *testing.T) {
	report := getSampleReport(t)
	report.EndTime = nil
	resp, err := ReportToCSV(report, true)
	assert.NoError(t, err)
	assert.Equal(t, "0001-01-01 00:00:00 +0000 UTC", resp[1][1])
}
