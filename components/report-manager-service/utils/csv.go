package utils

import (
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/sirupsen/logrus"

	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
)

type csvFields struct {
	NodeName              string    `csv:"Node Name"`
	EndTime               time.Time `csv:"End Time"`
	PlatformName          string    `csv:"Platform Name"`
	PlatformRelease       string    `csv:"Platform Release"`
	Environment           string    `csv:"Environment"`
	IPAddress             string    `csv:"IPAddress"`
	FQDN                  string    `csv:"FQDN"`
	ProfileName           string    `csv:"Profile Name"`
	ProfileTitle          string    `csv:"Profile Title"`
	ProfileVersion        string    `csv:"Profile Version"`
	ProfileSummary        string    `csv:"Profile Summary"`
	ControlID             string    `csv:"Control ID"`
	ControlTitle          string    `csv:"Control Title"`
	ControlImpact         string    `csv:"Control Impact"`
	ControlWaived         bool      `csv:"Waived (true/false)"`
	ResultStatus          string    `csv:"Result Status"`
	ResultRunTime         string    `csv:"Result Run Time"`
	ResultCodeDescription string    `csv:"Result Code Description"`
	ResultMessage         string    `csv:"Result Message"`
	ResultSkipMessage     string    `csv:"Result Skip Message"`
	WaiverJustification   string    `csv:"Waiver Justification"`
	WaiverExpiration      string    `csv:"Waiver Expiration"`
}

// ReportToCSV converts a report to its CSV representation as a string
// which includes a header line.
func ReportToCSV(report *reportingapi.Report, includeHeader bool) (response [][]string, err error) {
	if report == nil {
		return [][]string{}, fmt.Errorf("received nil to be converted to CSV")
	}
	header := []string{"Node Name", "End Time", "Platform Name", "Platform Release", "Environment", "IPAddress",
		"FQDN", "Profile Name", "Profile Title", "Profile Version", "Profile Summary", "Control ID",
		"Control Title", "Control Impact", "Waived (true/false)", "Result Status", "Result Run Time",
		"Result Code Description", "Result Message", "Result Skip Message", "Waiver Justification",
		"Waiver Expiration"}

	if includeHeader {
		response = append(response, header)
	}
	for _, profile := range report.Profiles {
		for _, control := range profile.Controls {
			for _, result := range control.Results {
				timestamp, err := ptypes.Timestamp(report.EndTime)
				if err != nil {
					logrus.Errorf(err.Error())
					timestamp = time.Time{}
				}
				waived := false
				if strings.HasPrefix(control.WaivedStr, "yes") {
					waived = true
				}
				if control.WaiverData == nil {
					// prevent nil pointer failure
					control.WaiverData = &reportingapi.OrigWaiverData{}
				}
				row := []string{}
				for _, item := range header {
					switch item {
					case "Node Name":
						row = append(row, report.NodeName)
					case "End Time":
						row = append(row, timestamp.String())
					case "Environment":
						row = append(row, report.Environment)
					case "FQDN":
						row = append(row, report.Fqdn)
					case "IPAddress":
						row = append(row, report.Ipaddress)
					case "Platform Name":
						row = append(row, report.GetPlatform().GetName())
					case "Platform Release":
						row = append(row, report.GetPlatform().GetRelease())
					case "Profile Name":
						row = append(row, profile.Name)
					case "Profile Title":
						row = append(row, profile.Title)
					case "Profile Version":
						row = append(row, profile.Version)
					case "Profile Summary":
						row = append(row, profile.Summary)
					case "Control ID":
						row = append(row, control.Id)
					case "Control Title":
						row = append(row, control.Title)
					case "Control Impact":
						row = append(row, strconv.FormatFloat(float64(control.Impact), 'f', 2, 32))
					case "Waived (true/false)":
						row = append(row, strconv.FormatBool(waived))
					case "Result Status":
						row = append(row, result.Status)
					case "Result Run Time":
						row = append(row, strconv.FormatFloat(float64(result.RunTime), 'f', 3, 32))
					case "Result Code Description":
						row = append(row, result.CodeDesc)
					case "Result Message":
						row = append(row, result.Message)
					case "Result Skip Message":
						row = append(row, result.SkipMessage)
					case "Waiver Justification":
						row = append(row, control.WaiverData.Justification)
					case "Waiver Expiration":
						row = append(row, control.WaiverData.ExpirationDate)
					}
				}
				response = append(response, row)
			}
		}
	}
	return response, nil
}
