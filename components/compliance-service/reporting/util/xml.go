package util

import (
	"encoding/xml"
	"fmt"

	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
)

type xmlFields struct {
	XMLName          xml.Name  `xml:"scan"`
	ID               string    `xml:"id,omitempty"`
	NodeID           string    `xml:"node_id,omitempty"`
	NodeName         string    `xml:"node_name,omitempty"`
	EndTime          int64     `xml:"end_time>seconds,omitempty"`
	Status           string    `xml:"status,omitempty"`
	Platform         Platform  `xml:"platform,omitempty"`
	Environment      string    `xml:"environment,omitempty"`
	IPAddress        string    `xml:"ipaddress,omitempty"`
	FQDN             string    `xml:"fqdn,omitempty"`
	Profiles         []Profile `xml:"profiles>profile,omitempty"`
	Version          string    `xml:"version,omitempty"`
	ChefServer       string    `xml:"chef_server,omitempty"`
	ChefOrganization string    `xml:"chef_organization,omitempty"`
	Statistics       float32   `xml:"statistics>duration,omitempty"`
}

// Platform detail
type Platform struct {
	Name    string `xml:"platform>name,omitempty"`
	Release string `xml:"platform>release,omitempty"`
	Full    string `xml:"platform>full,omitempty"`
}

// Profile detail
type Profile struct {
	Name     string    `xml:"name,omitempty"`
	Title    string    `xml:"title,omitempty"`
	Version  string    `xml:"version,omitempty"`
	Summary  string    `xml:"summary,omitempty"`
	Controls []Control `xml:"controls>control,omitempty"`
}

// Control detail
type Control struct {
	ID                  string   `xml:"id,omitempty"`
	Title               string   `xml:"title,omitempty"`
	Impact              float32  `xml:"impact,omitempty"`
	Waived              string   `xml:"waived,omitempty"`
	WaiverJustification string   `xml:"waiver>justification,omitempty"`
	WaiverExpiration    string   `xml:"waiver>expiration,omitempty"`
	Results             []Result `xml:"results>result,omitempty"`
}

// Result detail
type Result struct {
	Status          string  `xml:"status,omitempty"`
	RunTime         float32 `xml:"run_time,omitempty"`
	CodeDescription string  `xml:"code_desc,omitempty"`
	Message         string  `xml:"message,omitempty"`
	SkipMessage     string  `xml:"skip_message,omitempty"`
}

// ReportToXML converts a report to its XML representation as a string
func ReportToXML(report *reportingapi.Report) ([]byte, error) {
	if report == nil {
		return []byte{}, fmt.Errorf("Received an empty report to be converted to XML")
	}

	exportProfiles := make([]Profile, len(report.Profiles))

	for i, profile := range report.Profiles {
		exportControls := make([]Control, len(profile.Controls))

		for j, control := range profile.Controls {
			exportResults := make([]Result, len(control.Results))

			for k, result := range control.Results {
				exportResults[k] = Result{
					Status:          result.Status,
					RunTime:         result.RunTime,
					CodeDescription: result.CodeDesc,
					Message:         result.Message,
					SkipMessage:     result.SkipMessage,
				}
			} // End of results loop

			exportControls[j] = Control{
				ID:                  control.Id,
				Title:               control.Title,
				Impact:              control.Impact,
				Waived:              control.WaivedStr,
				WaiverJustification: control.WaiverData.GetJustification(),
				WaiverExpiration:    control.WaiverData.GetExpirationDate(),
				Results:             exportResults,
			}
		} // End of controls loop

		exportProfiles[i] = Profile{
			Name:     profile.Name,
			Title:    profile.Title,
			Version:  profile.Version,
			Summary:  profile.Summary,
			Controls: exportControls,
		}
	} // End of profiles loop

	xF := xmlFields{
		ID:       report.Id,
		NodeID:   report.NodeId,
		NodeName: report.NodeName,
		EndTime:  report.EndTime.GetSeconds(),

		Platform: Platform{
			Name:    report.GetPlatform().GetName(),
			Release: report.GetPlatform().GetRelease(),
			Full:    report.GetPlatform().GetFull(),
		},

		Environment:      report.Environment,
		IPAddress:        report.Ipaddress,
		FQDN:             report.Fqdn,
		Profiles:         exportProfiles,
		Version:          report.Version,
		ChefServer:       report.ChefServer,
		ChefOrganization: report.ChefOrganization,
		Statistics:       report.Statistics.Duration,
	}

	content, err := xml.MarshalIndent(xF, "  ", "    ")
	if err != nil {
		return []byte{}, fmt.Errorf("Failed to marshal XML report: %+v", err)
	}

	return content, nil
}
