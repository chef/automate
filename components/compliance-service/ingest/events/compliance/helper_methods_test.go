package compliance

import (
	"io/ioutil"
	"testing"

	"encoding/json"

	"fmt"

	"strings"

	"sort"

	"github.com/chef/automate/components/compliance-service/ingest/events/inspec"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/golang/protobuf/jsonpb"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
)

func TestSummary(t *testing.T) {
	// ------------------------------- ProfileControlSummary test --------------------------------- //
	p1json := `{
	  "name":"profile1",
    "version":"1.1.1",
    "sha256":"d2dcf9bf37feec01b31d743fbee6965f55218f730512e21308260c840f4f6654",
    "title": "Profile1 title",
    "controls":[
      {
        "id":"ctrl-01",
        "impact": 0,
        "title": "Checking for something1",
        "refs": [],
        "tags": {
          "firewall": null,
          "gtitle": "SRG-OS-000023-GPOS-00006",
          "satisfies": [
            "SRG-OS-000023-GPOS-00006",
            "SRG-OS-000024-GPOS-00007"
          ],
          "stig_id": "RHEL-07-010050",
          "cci": [
            "CCI-000048"
          ],
          "hashhash": {
            "bad.one": [6]
          },
          "documentable": false
        },
        "code": "control 'ctrl-01' do ...",
        "results":[
          {
            "status": "passed",
						"code_desc": "File /etc/passwd should be file",
						"run_time": 0.002524643,
						"start_time": "2017-10-18 09:18:40 +0000"
          },
          {
            "status":"passed"
          }
        ]
      },{
        "id":"ctrl-02",
        "refs": [],
        "tags": { "tag1": "value1" },
        "impact": 0.8,
        "results":[ {"status":"failed"}, {"status":"passed"} ]
      },{
        "id":"ctrl-03",
        "refs": [],
        "tags": {},
        "impact": 1.0,
        "results":[ {"status":"skipped"} ]
      }
    ],
    "status": "loaded"
  }`
	profile1 := parseProfile(&p1json)
	summary1 := ProfileControlSummary(profile1)
	assert.Equal(t, 3, summary1.Total, "3 total controls in profile")
	assert.Equal(t, 1, summary1.Passed.Total, "1 passed control in profile")
	assert.Equal(t, 1, summary1.Skipped.Total, "1 skipped control in profile")
	assert.Equal(t, 1, summary1.Failed.Total, "1 failed control in profile")
	assert.Equal(t, 1, summary1.Failed.Critical, "1 critical failed control in profile")
	assert.Equal(t, 0, summary1.Failed.Major, "0 major failed control in profile")

	// ------------------------------- ProfileControlSummary test --------------------------------- //

	p2json := `{
	  "name":"profile2",
    "version":"2.2.2",
    "sha256":"9490b16f32922b284a82a36d4f111e1474fcd9b53c4689f77de7ef68a1664487",
    "controls":[
      {
        "id":"sysctl-01",
        "refs": [],
        "tags": {},
        "results":[ {"status":"passed"}, {"status":"passed"} ]
      },{
        "refs": [],
        "tags": {},
        "id":"sysctl-02",
        "results":[ {"status":"passed"}, {"status":"skipped"} ]
      },{
        "refs": [],
        "tags": {},
        "id":"sysctl-03",
        "results":[ {"status":"passed"} ]
      }
    ],
    "status": "loaded"
  }`
	profile2 := parseProfile(&p2json)
	summary2 := ProfileControlSummary(profile2)
	assert.Equal(t, 3, summary2.Total, "3 total controls in profile")
	assert.Equal(t, 1, summary2.Skipped.Total, "1 skipped control in profile")
	assert.Equal(t, 0, summary2.Failed.Total, "0 failed control in profile")
	assert.Equal(t, 2, summary2.Passed.Total, "2 passed control in profile")

	// ------------------------------- ReportComplianceStatus tests --------------------------------- //

	assert.Equal(t, inspec.ResultStatusFailed, ReportComplianceStatus(summary1), "Report status is failed")
	assert.Equal(t, inspec.ResultStatusPassed, ReportComplianceStatus(summary2), "Report status is passed")
	summary3 := reporting.NodeControlSummary{}
	assert.Equal(t, inspec.ResultStatusSkipped, ReportComplianceStatus(&summary3), "Report status is skipped")
	summary3.Total = 3
	summary3.Skipped.Total = 3
	assert.Equal(t, inspec.ResultStatusSkipped, ReportComplianceStatus(&summary3), "Report status is skipped")

	// ------------------------------- AddControlSummary test --------------------------------- //

	summaryProfiles := make([]relaxting.ESInSpecSummaryProfile, 2)
	summaryProfiles[0] = relaxting.ESInSpecSummaryProfile{
		Profile:      fmt.Sprintf("%s|%s", profile1.Name, profile1.Sha256),
		ControlsSums: *summary1,
	}
	summaryProfiles[1] = relaxting.ESInSpecSummaryProfile{
		Profile:      fmt.Sprintf("%s|%s", profile2.Name, profile2.Sha256),
		ControlsSums: *summary2,
	}

	AddControlSummary(summary2, *summary1)
	assert.Equal(t, 6, summary2.Total, "6 total controls in profile")
	assert.Equal(t, 2, summary2.Skipped.Total, "2 skipped control in profile")
	assert.Equal(t, 1, summary2.Failed.Total, "1 failed control in profile")
	assert.Equal(t, 3, summary2.Passed.Total, "3 passed control in profile")

	// ------------------------------- Control Status and ImpactName tests --------------------------------- //

	ctrl := inspec.Control{Id: "one", Impact: 0}
	assert.Equal(t, inspec.ResultStatusPassed, ctrl.Status(), "Control with no results is passed")
	assert.Equal(t, inspec.ControlImpactMinor, ctrl.ImpactName(), "Control is minor")

	ctrl = inspec.Control{Id: "two", Impact: 0.1234, Results: []*inspec.Result{{Status: inspec.ResultStatusSkipped}, {Status: inspec.ResultStatusSkipped}}}
	assert.Equal(t, inspec.ResultStatusSkipped, ctrl.Status(), "Control is skipped")
	assert.Equal(t, inspec.ControlImpactMinor, ctrl.ImpactName(), "Control is minor")

	ctrl = inspec.Control{Id: "three", Impact: 0.4, Results: []*inspec.Result{{Status: inspec.ResultStatusPassed}, {Status: inspec.ResultStatusSkipped}}}
	assert.Equal(t, inspec.ResultStatusSkipped, ctrl.Status(), "Control is skipped")
	assert.Equal(t, inspec.ControlImpactMajor, ctrl.ImpactName(), "Control is major")

	ctrl = inspec.Control{Id: "four", Impact: 0.6999, Results: []*inspec.Result{{Status: inspec.ResultStatusSkipped}, {Status: inspec.ResultStatusPassed}}}
	assert.Equal(t, inspec.ResultStatusSkipped, ctrl.Status(), "Control is skipped")
	assert.Equal(t, inspec.ControlImpactMajor, ctrl.ImpactName(), "Control is major")

	ctrl = inspec.Control{Id: "five", Impact: 0.7, Results: []*inspec.Result{{Status: inspec.ResultStatusFailed}, {Status: inspec.ResultStatusPassed}}}
	assert.Equal(t, inspec.ResultStatusFailed, ctrl.Status(), "Control is failed")
	assert.Equal(t, inspec.ControlImpactCritical, ctrl.ImpactName(), "Control is critical")

	ctrl = inspec.Control{Id: "six", Impact: 1.0, Results: []*inspec.Result{{Status: inspec.ResultStatusPassed}, {Status: inspec.ResultStatusFailed}}}
	assert.Equal(t, inspec.ResultStatusFailed, ctrl.Status(), "Control is failed")
	assert.Equal(t, inspec.ControlImpactCritical, ctrl.ImpactName(), "Control is critical")

	ctrl = inspec.Control{Id: "seven", Impact: 1, Results: []*inspec.Result{{Status: inspec.ResultStatusSkipped}, {Status: inspec.ResultStatusFailed}}}
	assert.Equal(t, inspec.ResultStatusFailed, ctrl.Status(), "Control is failed")
	assert.Equal(t, inspec.ControlImpactCritical, ctrl.ImpactName(), "Control is critical")

	ctrl = inspec.Control{Id: "eight", Impact: 8, Results: []*inspec.Result{{Status: inspec.ResultStatusFailed}, {Status: inspec.ResultStatusSkipped}}}
	assert.Equal(t, inspec.ResultStatusFailed, ctrl.Status(), "Control is failed")
	assert.Equal(t, inspec.ControlImpactCritical, ctrl.ImpactName(), "Control is critical")

	ctrl = inspec.Control{Id: "nine", Impact: 0.8, Results: []*inspec.Result{{Status: inspec.ResultStatusPassed}, {Status: inspec.ResultStatusPassed}}}
	assert.Equal(t, inspec.ResultStatusPassed, ctrl.Status(), "Control is passed")

	// ------------------------------- ReportProfilesFromInSpecProfiles test --------------------------------- //

	actualProfilesMin := ReportProfilesFromInSpecProfiles([]*inspec.Profile{profile1, profile2}, summaryProfiles)
	profilesJson := fileContents("test_data/inspec_report_profiles_min_out.json")
	expectedProfilesMin := parseProfilesMin(&profilesJson)

	// To avoid random test failures due to StringTags coming out with random order
	for _, profileMin := range actualProfilesMin {
		for _, controlMin := range profileMin.Controls {
			sort.Slice(controlMin.StringTags, func(i, j int) bool {
				return controlMin.StringTags[i].Key < controlMin.StringTags[j].Key
			})
		}
	}
	assert.Equal(t, expectedProfilesMin, actualProfilesMin, "profiles_min match")

	p3json := `{
		"name": "mywindows",
		"version": "0.1.3",
		"sha256": "a41afaeb1d0ac15b9078d7ea139e741b6b27c1706c39ab0b09f3983f43c5940e",
		"title": "My Demo Windows Profile",
		"maintainer": "Demo, Inc.",
		"summary": "Verify that Windows nodes are configured securely",
		"license": "Proprietary, All rights reserved",
		"copyright": "Demo, Inc.",
		"copyright_email": "support@example.com",
		"supports": [
			{
				"platform-family": "windows"
			}
		],
		"attributes": [],
		"groups": [],
		"controls": [],
		"status": "skipped",
		"skip_message": "Skipping profile: 'mywindows' on unsupported platform: 'mac_os_x/17.7.0'."
	}`
	profile3 := parseProfile(&p3json)
	assert.Equal(t, profile3.Status, "skipped", "profile status match")
	assert.Equal(t, profile3.SkipMessage, "Skipping profile: 'mywindows' on unsupported platform: 'mac_os_x/17.7.0'.", "profile skip message match")

	// ------------------------------- ProfilesFromReport test --------------------------------- //
	fixedProfiles := FixInheritedProfiles([]*inspec.Profile{profile1, profile2})
	actualProfiles, err := ProfilesFromReport(fixedProfiles)
	assert.Equal(t, nil, err, "ProfilesFromReport nil err")
	for _, profile := range actualProfiles {
		assert.Regexp(t, "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z", profile.ESTimestamp)
		// overwriting the dynamic timestamp to avoid test failures
		profile.ESTimestamp = "2017-10-26T18:38:09Z"
	}
	expectedProfiles := parseESProfiles(fileContents("test_data/inspec_report_profiles_out.json"))

	assert.Equal(t, expectedProfiles, actualProfiles, "profiles doc match")
}

// ------------------------------- Inherited test --------------------------------- //
func TestInheritanceFix(t *testing.T) {
	reportJson := fileContents("test_data/inspec_report_inherited_in.json")
	actualReport := parseReport(&reportJson)
	fixedProfiles := FixInheritedProfiles(actualReport.Profiles)
	actualProfiles, err := ProfilesFromReport(fixedProfiles)
	assert.Equal(t, nil, err, "ProfilesFromReport nil err")
	for _, profile := range actualProfiles {
		assert.Regexp(t, "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z", profile.ESTimestamp)
		// overwriting the dynamic timestamp to avoid test failures
		profile.ESTimestamp = "2017-10-26T19:29:21Z"
	}
	expectedProfiles := parseESProfiles(fileContents("test_data/inspec_report_inherited_profiles_out.json"))

	// Dump actualProfiles
	// println(profilesToJson(actualProfiles))

	assert.Equal(t, expectedProfiles, actualProfiles, "fix inherited profiles test")
}

// ------------------------------- Inherited test2 --------------------------------- //
func TestInheritanceWithParentProfileFix(t *testing.T) {
	reportJson := fileContents("test_data/inspec_report_inherited_parent_profile_in.json")
	actualReport := parseReport(&reportJson)
	fixedProfiles := FixInheritedProfiles(actualReport.Profiles)
	actualProfiles, err := ProfilesFromReport(fixedProfiles)
	assert.Equal(t, nil, err, "ProfilesFromReport nil err")
	for _, profile := range actualProfiles {
		assert.Regexp(t, "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z", profile.ESTimestamp)
		// overwriting the dynamic timestamp to avoid test failures
		profile.ESTimestamp = "2017-10-26T19:29:21Z"
	}
	expectedProfiles := parseESProfiles(fileContents("test_data/inspec_report_inherited_profiles_parent_profile_out.json"))

	// Dump actualProfiles
	// println(profilesToJsonMin(actualProfiles))

	assert.Equal(t, expectedProfiles, actualProfiles, "fix inherited profiles with parent_profile test")
}

func TestStrLimitBytes(t *testing.T) {
	utf8String := "日本語"
	asciiString := "English"

	assert.Equal(t, utf8String, stringLimitBytes(utf8String, 100))
	assert.Equal(t, utf8String, stringLimitBytes(utf8String, 9))
	assert.Equal(t, "日本", stringLimitBytes(utf8String, 6))
	assert.Equal(t, "日", stringLimitBytes(utf8String, 4))
	assert.Equal(t, "日", stringLimitBytes(utf8String, 5))
	assert.Equal(t, "", stringLimitBytes(utf8String, 1))

	assert.Equal(t, asciiString, stringLimitBytes(asciiString, 100))
	assert.Equal(t, asciiString, stringLimitBytes(asciiString, 7))
	assert.Equal(t, "Englis", stringLimitBytes(asciiString, 6))
	assert.Equal(t, "En", stringLimitBytes(asciiString, 2))
	assert.Equal(t, "E", stringLimitBytes(asciiString, 1))
}

func parseProfile(js *string) *inspec.Profile {
	var p inspec.Profile
	err := (&jsonpb.Unmarshaler{}).Unmarshal(strings.NewReader(*js), &p)
	if err != nil {
		panic(fmt.Sprintf("Error unmarshalling profile: %s", err))
	}
	return &p
}

func parseProfilesMin(js *string) (profiles []relaxting.ESInSpecReportProfile) {
	err := json.Unmarshal([]byte(*js), &profiles)
	if err != nil {
		panic(fmt.Sprintf("Error unmarshalling profiles min: %s", err))
	}
	for i := range profiles {
		if profiles[i].Depends == nil {
			profiles[i].Depends = []relaxting.ESInSpecReportDepends{}
		}
		for ci := range profiles[i].Controls {
			if profiles[i].Controls[ci].Refs == nil {
				profiles[i].Controls[ci].Refs = []relaxting.ESInSpecReportControlRefs{}
			}
		}
	}
	return profiles
}

func parseProfiles(js *string) (profiles []*inspec.Profile) {
	err := json.Unmarshal([]byte(*js), &profiles)
	if err != nil {
		panic(fmt.Sprintf("Error unmarshalling profiles: %s", err))
	}
	return profiles
}

func parseReport(js *string) *Report {
	rep := Report{}
	err := jsonpb.Unmarshal(strings.NewReader(*js), &rep)
	if err != nil {
		logrus.Error(err.Error())
		panic(fmt.Sprintf("Error unmarshalling report: %s", err))
	}
	return &rep
}

func parseESProfiles(js string) (profiles []*relaxting.ESInspecProfile) {
	err := json.Unmarshal([]byte(js), &profiles)
	if err != nil {
		panic(fmt.Sprintf("Error unmarshalling ES profiles: %s", err))
	}
	return profiles
}

func fileContents(file string) (str string) {
	contents, err := ioutil.ReadFile(file)
	if err != nil {
		panic(fmt.Sprintf("Error reading from file %s: %s", file, err))
	}
	return string(contents)
}

func profilesToJson(profiles []*relaxting.ESInspecProfile) (str string) {
	profsBytes, err := json.MarshalIndent(profiles, "", "  ")
	if err != nil {
		panic(fmt.Sprintf("Error marshalling profiles: %s", err))
	}
	return string(profsBytes)
}

func profilesToJsonMin(profiles []*relaxting.ESInspecProfile) (str string) {
	profsBytes, err := json.Marshal(profiles)
	if err != nil {
		panic(fmt.Sprintf("Error marshalling profiles: %s", err))
	}
	return string(profsBytes)
}
