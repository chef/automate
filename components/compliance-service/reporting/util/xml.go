package util

import (
	"encoding/xml"
	"fmt"

	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
)

type xmlFields struct {
	XMLName          xml.Name  `xml:"Node"`
	ID               string    `xml:"ID,omitempty"`
	NodeID           string    `xml:"NodeID,omitempty"`
	NodeName         string    `xml:"NodeName,omitempty"`
	EndTime          int64     `xml:"EndTime>Seconds,omitempty"`
	Platform         Platform  `xml:"Platform,omitempty"`
	Environment      string    `xml:"Environment,omitempty"`
	IPAddress        string    `xml:"IPAddress,omitempty"`
	FQDN             string    `xml:"FQDN,omitempty"`
	Profiles         []Profile `xml:"Profiles>Profile,omitempty"`
	Version          string    `xml:"Version,omitempty"`
	ChefServer       string    `xml:"ChefServer,omitempty"`
	ChefOrganization string    `xml:"ChefOrganization,omitempty"`
}

// Platform detail
type Platform struct {
	Name    string `xml:"Platform>Name,omitempty"`
	Release string `xml:"Platform>Release,omitempty"`
	Full    string `xml:"Platform>Full,omitempty"`
}

// Profile detail
type Profile struct {
	Name      string    `xml:"Name,omitempty"`
	Title     string    `xml:"Title,omitempty"`
	Version   string    `xml:"Version,omitempty"`
	Summary   string    `xml:"Summary,omitempty"`
	Controles []Control `xml:"Controles>Control,omitempty"`
}

// Control detail
type Control struct {
	ID                  string   `xml:"ID,omitempty"`
	Title               string   `xml:"Title,omitempty"`
	Impact              float32  `xml:"Impact,omitempty"`
	Waived              string   `xml:"Waived,omitempty"`
	WaiverJustification string   `xml:"Waiver>Justification,omitempty"`
	WaiverExpiration    string   `xml:"Waiver>Expiration,omitempty"`
	Results             []Result `xml:"Results>Result,omitempty"`
}

// Result detail
type Result struct {
	Status          string  `xml:"Status,omitempty"`
	RunTime         float32 `xml:"RunTime,omitempty"`
	CodeDescription string  `xml:"CodeDescription,omitempty"`
	Message         string  `xml:"Message,omitempty"`
	SkipMessage     string  `xml:"SkipMessage,omitempty"`
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
			Name:      profile.Name,
			Title:     profile.Title,
			Version:   profile.Version,
			Summary:   profile.Summary,
			Controles: exportControls,
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
	}

	content, err := xml.MarshalIndent(xF, "  ", "    ")
	if err != nil {
		return []byte{}, fmt.Errorf("Failed to marshal XML report: %+v", err)
	}

	return content, nil
}
