package compliance

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"
	"unicode/utf8"

	"github.com/golang/protobuf/jsonpb"
	structpb "github.com/golang/protobuf/ptypes/struct"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	inspec_api "github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	inspecTypes "github.com/chef/automate/components/compliance-service/inspec"
	reportingTypes "github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/lib/pcmp"
	"github.com/chef/automate/lib/stringutils"
)

const DocVersion = "1"

// Elasticsearch rejects documents with values for keyword fields with more
// than this number of bytes.
// https://www.elastic.co/guide/en/elasticsearch/reference/6.4/ignore-above.html
const (
	maxESKeywordBytesv1 = 32766
	maxESKeywordBytesv2 = 1024
)

// ProfileControlSummary returns a NodeControlSummary struct with the counted controls based on their status and criticality,
// This is working on all profiles embedded in a full json report.
// total: count for all controls in the report, e.g. 100
// waived: count for all waived controls, e.g. 5
// passed.total: count for all controls that executed successfully, e.g. 35
// skipped.total: count for all skipped controls, e.g. 10
// failed.total: count for all failed controls, e.g. 50
// failed.minor/major/critical: split the total failed controls in 3 buckets based on the criticality,
// e.g. minor: 10, major: 15, critical: 25
func ProfileControlSummary(profile *inspec_api.Profile) *reportingTypes.NodeControlSummary {
	summary := reportingTypes.NodeControlSummary{}
	for _, control := range profile.Controls {
		summary.Total++
		if control.WaiverData != nil && !pcmp.DeepEqual(*control.WaiverData, inspec_api.WaiverData{}) && !strings.HasPrefix(control.WaiverData.Message, "Waiver expired") {
			// Expired waived controls are not waived. This way, we can use the actual status of the executed control
			summary.Waived.Total++
		} else {
			switch control.Status(WaivedStr(control.WaiverData)) {
			case inspec.ResultStatusPassed:
				summary.Passed.Total++
			case inspec.ResultStatusSkipped:
				summary.Skipped.Total++
			case inspec.ResultStatusFailed:
				summary.Failed.Total++
				switch control.ImpactName() {
				case inspec.ControlImpactCritical:
					summary.Failed.Critical++
				case inspec.ControlImpactMajor:
					summary.Failed.Major++
				case inspec.ControlImpactMinor:
					summary.Failed.Minor++
				}
			}
		}
	}
	return &summary
}

// AddControlSummary adds the control counts from 'sum' to 'total'
func AddControlSummary(total *reportingTypes.NodeControlSummary, sum reportingTypes.NodeControlSummary) {
	total.Total += sum.Total
	total.Passed.Total += sum.Passed.Total
	total.Skipped.Total += sum.Skipped.Total
	total.Failed.Total += sum.Failed.Total
	total.Failed.Minor += sum.Failed.Minor
	total.Failed.Major += sum.Failed.Major
	total.Failed.Critical += sum.Failed.Critical
	total.Waived.Total += sum.Waived.Total
}

// ReportComplianceStatus returns the overall compliance status of a report based on the passed/failed/skipped/waived control counts
func ReportComplianceStatus(summary *reportingTypes.NodeControlSummary) (status string) {
	if summary.Failed.Total > 0 {
		status = inspec.ResultStatusFailed
	} else if summary.Total == summary.Skipped.Total && summary.Total > 0 {
		status = inspec.ResultStatusSkipped
	} else if summary.Total == summary.Waived.Total && summary.Total > 0 {
		status = inspec.ResultStatusWaived
	} else {
		status = inspec.ResultStatusPassed
	}
	return status
}

// WaivedStr returns a string label based on the control waived status
func WaivedStr(data *inspec_api.WaiverData) (str string) {
	if data == nil || pcmp.DeepEqual(*data, inspec_api.WaiverData{}) {
		return inspec.ControlWaivedStrNo
	}

	if strings.HasPrefix(data.Message, "Waiver expired") {
		return inspec.ControlWaivedStrNoExpired
	}

	if data.Run {
		return inspec.ControlWaivedStrYesRun
	}

	return inspec.ControlWaivedStrYes
}

// ReportProfilesFromInSpecProfiles extracts the reports specific information
// from the profile, leaving out the static profile data
func ReportProfilesFromInSpecProfiles(profiles []*inspec_api.Profile, profilesSums []relaxting.ESInSpecSummaryProfile,
	enableLargeReporting bool) (profilesRep []relaxting.ESInSpecReportProfile) {
	// Creating a profilesSums hash to lookup the sums based on the profile (name|sha) string
	profilesSumsHash := make(map[string]relaxting.ESInSpecSummaryProfile, len(profilesSums))
	for _, profileSums := range profilesSums {
		profilesSumsHash[profileSums.Profile] = profileSums
	}

	for _, profile := range profiles {
		minControls := make([]relaxting.ESInSpecReportControl, len(profile.Controls))
		profileNameSha := NameSha(profile)
		profileSums := profilesSumsHash[profileNameSha]

		for i, control := range profile.Controls {
			minResults := make([]*relaxting.ESInSpecReportControlsResult, len(control.Results))
			for i, result := range control.Results {
				var trimSize int
				trimSize = maxESKeywordBytesv1
				if enableLargeReporting {
					trimSize = maxESKeywordBytesv2
				}
				minResults[i] = &relaxting.ESInSpecReportControlsResult{
					Status:      result.Status,
					CodeDesc:    stringLimitBytes(result.CodeDesc, trimSize),
					RunTime:     result.RunTime,
					Message:     stringLimitBytes(result.Message, trimSize),
					SkipMessage: stringLimitBytes(result.SkipMessage, trimSize),
				}
			}
			stringTags := make([]relaxting.ESInSpecReportControlStringTags, 0)
			if control.Tags != nil {
				for tKey, tValue := range control.Tags.Fields {
					if newStringTag := relaxting.StringTagsFromProtoFields(tKey, tValue); newStringTag != nil {
						stringTags = append(stringTags, *newStringTag)
					}
				}
			}

			refs := make([]relaxting.ESInSpecReportControlRefs, 0)
			for _, ref := range control.Refs {
				var refVal, urlVal string
				if len(ref.Fields) != 2 {
					logrus.Warnf("ref object contains more than two fields: %v", ref)
					continue
				}
				for key, val := range ref.Fields {
					if key == "ref" {
						refVal = val.GetStringValue()
					} else {
						// both url and uri values accepted here.
						urlVal = val.GetStringValue()
					}

				}

				refs = append(refs, relaxting.ESInSpecReportControlRefs{
					Ref: refVal,
					Url: urlVal,
				})
			}

			controlWaivedStr := WaivedStr(control.WaiverData)
			logrus.Infof("The current status: %+v", control.Status(controlWaivedStr))

			minControls[i] = relaxting.ESInSpecReportControl{
				ID:         control.Id,
				Title:      control.Title,
				Impact:     control.Impact,
				Status:     control.Status(controlWaivedStr),
				Results:    minResults,
				StringTags: stringTags,
				Refs:       refs,
				WaivedStr:  controlWaivedStr,
			}

			if control.RemovedResultsCounts != nil {
				minControls[i].RemovedResultsCounts = &relaxting.ESInSpecReportControlRemovedResultsCounts{
					Failed:  int(control.RemovedResultsCounts.Failed),
					Skipped: int(control.RemovedResultsCounts.Skipped),
					Passed:  int(control.RemovedResultsCounts.Passed),
				}
			}

			// In ingestion old inspec reports can come without control `waiver_data` and new ones can have `waiver_data: {}`
			if controlWaivedStr != inspec.ControlWaivedStrNo {
				minControls[i].WaiverData = &relaxting.ESInSpecReportControlsWaiverData{
					ExpirationDate:     control.WaiverData.ExpirationDate,
					Justification:      control.WaiverData.Justification,
					Run:                control.WaiverData.Run,
					SkippedDueToWaiver: control.WaiverData.SkippedDueToWaiver,
					Message:            control.WaiverData.Message,
				}
			}
		}

		minDepends := make([]relaxting.ESInSpecReportDepends, len(profile.Depends))
		for i, dependency := range profile.Depends {
			minDepends[i] = relaxting.ESInSpecReportDepends{
				Name:        dependency.Name,
				Status:      dependency.Status,
				SkipMessage: dependency.SkipMessage,
			}
		}

		statusMessage := profile.StatusMessage
		if profile.StatusMessage == "" {
			// Legacy message only available for the skipped status
			statusMessage = profile.SkipMessage
		}

		profilesRep = append(profilesRep, relaxting.ESInSpecReportProfile{
			Name:          profile.Name,
			Title:         profile.Title,
			Profile:       profileSums.Profile,
			Version:       profile.Version,
			Full:          stringutils.GetFullProfileName(profile.Title, profile.Version),
			SHA256:        profile.Sha256,
			Controls:      minControls,
			ControlsSums:  profileSums.ControlsSums,
			Depends:       minDepends,
			Status:        profileSums.Status,
			StatusMessage: statusMessage,
		})
	}
	return profilesRep
}

type AttributeOption struct {
	Description *string     `json:"description,omitempty"`
	Default     interface{} `json:"default,omitempty"`
}

// ProfilesFromReport takes the profiles array of an inspec full json report and returns the profiles
// with only the static information, without the results of the controls as the report has it
func ProfilesFromReport(reportProfiles []*inspec_api.Profile) (profiles []*relaxting.ESInspecProfile, err error) {
	for _, reportProfile := range reportProfiles {
		esProfile := relaxting.ESInspecProfile{
			Name:           reportProfile.Name,
			Title:          reportProfile.Title,
			Version:        reportProfile.Version,
			Sha256:         reportProfile.Sha256,
			Summary:        reportProfile.Summary,
			Maintainer:     reportProfile.Maintainer,
			License:        reportProfile.License,
			Copyright:      reportProfile.Copyright,
			CopyrightEmail: reportProfile.CopyrightEmail,
			DocVersion:     DocVersion,
			ESTimestamp:    CurrentTime(),
		}

		esProfile.Groups = make([]inspecTypes.Group, len(reportProfile.Groups))
		for i, group := range reportProfile.Groups {
			esGroup := inspecTypes.Group{ID: group.Id, Controls: group.Controls}
			if group.Title != "" {
				esGroup.Title = &group.Title
			}
			esProfile.Groups[i] = esGroup
		}
		esProfile.Supports = reportProfile.Supports

		esProfile.Dependencies = make([]inspecTypes.Dependency, len(reportProfile.Depends))
		for i, dep := range reportProfile.Depends {
			esDependency := inspecTypes.Dependency{Name: dep.Name, URL: dep.Url, Path: dep.Path, Git: dep.Git, Branch: dep.Branch,
				Tag: dep.Tag, Commit: dep.Commit, Version: dep.Version, Supermarket: dep.Supermarket, Compliance: dep.Compliance}
			esProfile.Dependencies[i] = esDependency
		}

		controls := make([]relaxting.ESInspecControl, len(reportProfile.Controls))
		for i, control := range reportProfile.Controls {
			jsonTags, err := structToJson(control.Tags)
			if err != nil {
				return profiles, err
			}
			jsonRefs, err := arrayOfStructToJson(control.Refs)
			if err != nil {
				return profiles, err
			}
			esControl := relaxting.ESInspecControl{
				ID:     control.Id,
				Code:   control.Code,
				Desc:   control.Desc,
				Impact: control.Impact,
				Title:  control.Title,
				Tags:   jsonTags,
				Refs:   jsonRefs,
			}

			if control.SourceLocation != nil {
				esControl.SourceLocation.Line = int(control.SourceLocation.Line)
				esControl.SourceLocation.Ref = control.SourceLocation.Ref
			}

			controls[i] = esControl
		}
		esProfile.Controls = controls

		esProfile.Attributes = make([]relaxting.ESInspecAttribute, len(reportProfile.Attributes))
		for i, attribute := range reportProfile.Attributes {
			esAttribute := relaxting.ESInspecAttribute{Name: attribute.Name}

			var esOption AttributeOption
			esOptionJson, err := structToJson(attribute.Options)
			if err != nil {
				return profiles, err
			}
			err = json.Unmarshal([]byte(esOptionJson), &esOption)
			if err != nil {
				return profiles, fmt.Errorf("Error unmarshalling attribute options: %s", err)
			}
			jsonBytes, err := json.Marshal(esOption.Default)
			if err != nil {
				return profiles, fmt.Errorf("Error unmarshalling attribute options default: %s", err)
			}
			if esOption.Description != nil {
				esAttribute.Options.Description = *esOption.Description
			}
			esAttribute.Options.Default = string(jsonBytes)

			esProfile.Attributes[i] = esAttribute
		}
		profiles = append(profiles, &esProfile)
	}
	return profiles, nil
}

func CurrentTime() string {
	return time.Now().UTC().Format(time.RFC3339)
}

// structToJson marshals a proto buffer Struct into a json
func structToJson(obj *structpb.Struct) (string, error) {
	if obj == nil {
		return "{}", nil
	}
	// TODO: once the json package can marshal pb structs into standard json, move away from jsonpb; https://github.com/golang/protobuf/issues/256
	jsonBytes, err := (&jsonpb.Marshaler{OrigName: true}).MarshalToString(obj)
	if err != nil {
		return "", fmt.Errorf("Error marshalling pbstruct to json: %s", err)
	}
	return jsonBytes, nil
}

// arrayOfStructToJson marshals an array of proto buffer Structs into a json
func arrayOfStructToJson(objs []*structpb.Struct) (string, error) {
	if objs == nil {
		return "[]", nil
	}
	jsonString := "["
	n := len(objs)
	for i, obj := range objs {
		objJson, err := structToJson(obj)
		if err != nil {
			return "", fmt.Errorf("Error marshalling array pbstruct to json: %s", err)
		}
		jsonString += objJson
		if i < n-1 {
			jsonString += ","
		}
	}
	jsonString += "]"
	return jsonString, nil
}

// FixInheritedProfiles updates profiles so they include all controls from the profiles they depend on
// Dependent profiles will be removed from the list of profiles
func FixInheritedProfiles(reportProfiles []*inspec_api.Profile) (fixedProfiles []*inspec_api.Profile) {
	// gather all profile dependencies first
	dependencies := make([]string, 0)
	for _, profile := range reportProfiles {
		if len(profile.Depends) > 0 {
			for _, dep := range profile.Depends {
				dependencies = append(dependencies, dep.Name)
			}
		}
	}

	for _, profile := range reportProfiles {
		// ParentProfile introduced due to this bug: https://github.com/inspec/inspec/issues/3135
		if stringutils.SliceContains(dependencies, profile.Name) || profile.ParentProfile != "" {
			// skip dependent profiles as their controls will be merged in their wrapper profile
			continue
		}

		// merge the results and controls from the dependent profiles
		controlsHash := hashControls(profile.Controls)
		for _, controlFromDependency := range getControlsFromDeps(false, profile, reportProfiles) {
			mergeControls(controlsHash[controlFromDependency.Id], controlFromDependency)
		}

		fixedProfiles = append(fixedProfiles, profile)
	}

	return fixedProfiles
}

func NameSha(p *inspec_api.Profile) string {
	return fmt.Sprintf("%s|%s", p.Name, p.Sha256)
}

// mergeControls workarounds an inspec report bug where inherited controls that show up
// under the wrapper profile have missing or blank code and result fields
func mergeControls(dst *inspec_api.Control, src *inspec_api.Control) {
	if dst == nil {
		return
	}
	if dst.Code == "" {
		dst.Code = src.Code
	}
	if dst.Results == nil {
		dst.Results = src.Results
	}
}

// takes an array of *Control
func hashControls(controls []*inspec_api.Control) map[string]*inspec_api.Control {
	controlsHash := make(map[string]*inspec_api.Control, 0)
	for _, control := range controls {
		controlsHash[control.Id] = control
	}
	return controlsHash
}

func getControlsFromDeps(ownControls bool, forProfile *inspec_api.Profile, reportProfiles []*inspec_api.Profile) (allControls []*inspec.Control) {
	if ownControls == true {
		allControls = forProfile.Controls
	}
	for _, dep := range forProfile.Depends {
		for _, p := range reportProfiles {
			// first, look for a profile with a name that matches the name in the depends
			// second, look for parent_profile introduced due to this bug: https://github.com/inspec/inspec/issues/3135
			if dep.Name == p.Name || forProfile.Name == p.ParentProfile {
				allControls = append(allControls, getControlsFromDeps(true, p, reportProfiles)...)
				continue
			}
		}
	}
	return allControls
}

// stringLimitBytes returns a substring of the passed string constructed by
// limiting the string to at most byteCount bytes.
func stringLimitBytes(str string, byteCount int) string {
	if len(str) <= byteCount {
		return str
	}

	str = str[:byteCount]
	// Find the start of the last rune we might have
	lastRuneStartIdx := byteCount - 1
	for !utf8.RuneStart(str[lastRuneStartIdx]) {
		lastRuneStartIdx--
	}

	// Throw out all bytes related to this rune if it isn't valid
	if utf8.ValidString(str[lastRuneStartIdx:]) {
		return str
	} else {
		return str[:lastRuneStartIdx]
	}
}
