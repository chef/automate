package util

import (
	"fmt"
	"strconv"
	"strings"
	"time"
	"unicode/utf8"

	"github.com/golang/protobuf/ptypes"
	"github.com/sirupsen/logrus"

	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
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
				waived := false
				if strings.HasPrefix(control.WaivedStr, "yes") {
					waived = true
				}
				if control.WaiverData == nil {
					// prevent nil pointer failure
					control.WaiverData = &reportingapi.OrigWaiverData{}
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
					ControlWaived:         waived,
					ResultStatus:          result.Status,
					ResultRunTime:         strconv.FormatFloat(float64(result.RunTime), 'f', 3, 32),
					ResultCodeDescription: maxCharLimit(result.CodeDesc),
					ResultMessage:         maxCharLimit(result.Message),
					ResultSkipMessage:     result.SkipMessage,
					WaiverJustification:   control.WaiverData.Justification,
					WaiverExpiration:      control.WaiverData.ExpirationDate,
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

func maxCharLimit(item string) string {
	if utf8.RuneCountInString(item) > 32000 {
		msg := "character limit exceeds 32,000. truncating message: "
		countToReturn := 32000 - utf8.RuneCountInString(msg)
		return fmt.Sprintf("%s%s", msg, string([]rune(item)[0:countToReturn]))
	}
	return item
}
