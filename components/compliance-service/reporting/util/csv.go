package util

import (
	"fmt"
	"strconv"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/sirupsen/logrus"

	reportingapi "github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/gocarina/gocsv"
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
	ResultStatus          string    `csv:"Result Status"`
	ResultRunTime         string    `csv:"Result Run Time"`
	ResultCodeDescription string    `csv:"Result Code Description"`
	ResultMessage         string    `csv:"Result Message"`
	ResultSkipMessage     string    `csv:"Result Skip Message"`
}

// ReportToCSV converts a report to its CSV representation as a string
// which includes a header line.
func ReportToCSV(report *reportingapi.Report) (string, error) {
	var cache = []csvFields{}

	if report == nil {
		return "", fmt.Errorf("Received an empty report to be converted to CSV.")
	}
	for _, profile := range report.Profiles {
		for _, control := range profile.Controls {
			for _, result := range control.Results {
				timestamp, err := ptypes.Timestamp(report.EndTime)
				if err != nil {
					logrus.Errorf(err.Error())
					timestamp = time.Time{}
				}
				cache = append(cache, csvFields{
					NodeName:              report.NodeName,
					EndTime:               timestamp,
					Environment:           report.Environment,
					FQDN:                  report.Fqdn,
					IPAddress:             report.Ipaddress,
					PlatformName:          report.GetPlatform().GetName(),
					PlatformRelease:       report.GetPlatform().GetRelease(),
					ProfileName:           profile.Name,
					ProfileTitle:          profile.Title,
					ProfileVersion:        profile.Version,
					ProfileSummary:        profile.Summary,
					ControlID:             control.Id,
					ControlTitle:          control.Title,
					ControlImpact:         strconv.FormatFloat(float64(control.Impact), 'f', 2, 32),
					ResultStatus:          result.Status,
					ResultRunTime:         strconv.FormatFloat(float64(result.RunTime), 'f', 3, 32),
					ResultCodeDescription: result.CodeDesc,
					ResultMessage:         result.Message,
					ResultSkipMessage:     result.SkipMessage,
				})
			}
		}
	}

	// export everything
	content, err := gocsv.MarshalString(&cache)
	if err != nil {
		return "", fmt.Errorf("Failed to marshal CSV report: %+v", err)
	}
	return content, nil
}
