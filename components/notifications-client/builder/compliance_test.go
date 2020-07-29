package builder

import (
	"io/ioutil"
	"path"
	"strings"
	"testing"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	. "github.com/chef/automate/components/notifications-client/api"
	"github.com/golang/protobuf/jsonpb"
	proto "github.com/golang/protobuf/proto"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const URL = "http://localhost"

func TestSingleProfileMultipleControlSingleCriticalFailure(t *testing.T) {
	var report compliance.Report
	// This test has 1 profile, with 2 controls. 1 of those controls
	// has 1 of 3 failing tests.
	fromJSON("inspec-report-single-failure.json", &report)

	ev, _ := Compliance(URL, &report)

	failure := ev.GetComplianceFailure()
	// Test some of the metadata
	assert.Equal(t, "b4df00d05", failure.GetId())
	assert.Equal(t, "deadbeef05", failure.GetNodeId())
	assert.Equal(t, "pretty-chipper-node", failure.GetNodeName())

	// Make sure the top level counts are correct
	assert.Equal(t,
		ComplianceFailure_ControlTotals{
			Skipped:        int32(0),
			Passed:         int32(1),
			Failed:         int32(1),
			Critical:       int32(2),
			CriticalFailed: int32(1),
		},
		*failure.GetTestTotals())

	// The failing profile is included
	assert.Equal(t, 1, len(failure.GetFailedProfiles()))

	profile := failure.GetFailedProfiles()[0]

	// Make sure the control counts in the profile are correct
	assert.Equal(t,
		Profile_ControlTotals{
			NumTests:        2,
			NumFailedTests:  1,
			NumSkippedTests: 0,
			NumPassedTests:  1,
		},
		*profile.GetStats())

	// In the 1 failing profile, there is one failing control
	assert.Equal(t, 1, len(profile.GetFailedControls()))

	control := profile.GetFailedControls()[0]

	// There's 1 failing result in the control
	assert.Equal(t, 1, len(control.GetFailedResults()))

	result := control.GetFailedResults()[0]
	// Check the result to make sure it has the correct data
	assert.Equal(t, "failed", result.GetStatus())
	assert.NotZero(t, result.GetCodeDesc(), "expected CodeDesc")
	assert.NotZero(t, result.GetRunTime(), "expected RunTime")
	assert.NotZero(t, result.GetStartTime(), "expected StartTime")
	assert.NotZero(t, result.GetMessage(), "expected Message")
}

func TestMultipleFailuresInMultipleProfiles(t *testing.T) {
	var report compliance.Report
	// This test has 1 profile, with 2 controls. 1 of those controls
	// has 1 of 3 failing tests.
	fromJSON("inspec-mixed.json", &report)

	ev, _ := Compliance(URL, &report)
	failure := ev.GetComplianceFailure()

	// Make sure the top level counts are correct
	assert.Equal(t,
		ComplianceFailure_ControlTotals{
			Skipped:        int32(1),
			Passed:         int32(1),
			Failed:         int32(2),
			Critical:       int32(3),
			CriticalFailed: int32(1),
		},
		*failure.GetTestTotals())

	// The failing profiles are included
	assert.Equal(t, 2, len(failure.GetFailedProfiles()))

	failed_profile_1 := failure.GetFailedProfiles()[0]
	failed_profile_2 := failure.GetFailedProfiles()[1]

	// Make sure the control counts in the profiles are correct
	assert.Equal(t,
		Profile_ControlTotals{
			NumTests:        1,
			NumFailedTests:  1,
			NumSkippedTests: 0,
			NumPassedTests:  0,
		},
		*failed_profile_1.GetStats())

	assert.Equal(t,
		Profile_ControlTotals{
			NumTests:        1,
			NumFailedTests:  1,
			NumSkippedTests: 0,
			NumPassedTests:  0,
		},
		*failed_profile_2.GetStats())

	assert.Equal(t, 1, len(failed_profile_1.GetFailedControls()))
	assert.Equal(t, 1, len(failed_profile_2.GetFailedControls()))

	control_1 := failed_profile_1.GetFailedControls()[0]
	control_2 := failed_profile_2.GetFailedControls()[0]

	results_1 := control_1.GetFailedResults()
	results_2 := control_2.GetFailedResults()

	assert.Equal(t,
		Profile_Control_ResultTotals{
			NumTests:        3,
			NumFailedTests:  1,
			NumSkippedTests: 0,
			NumPassedTests:  2,
		},
		*control_1.GetStats())

	assert.Equal(t,
		Profile_Control_ResultTotals{
			NumTests:        3,
			NumFailedTests:  2,
			NumSkippedTests: 0,
			NumPassedTests:  1,
		},
		*control_2.GetStats())

	assert.Equal(t, 1, len(results_1))
	assert.Equal(t, 2, len(results_2))

	for _, result := range results_1 {
		assert.Equal(t, "failed", result.GetStatus())
	}

	for _, result := range results_2 {
		assert.Equal(t, "failed", result.GetStatus())
	}
}

// There is a problem with custom InSpec reports where they sometimes have an array for the
// 'controls.refs.ref' instead of a string.
// "controls": [
// 	{
// 		"title": "Checking for /etc/passwd",
// 		"desc": "Checking for /etc/passwd desc",
// 		"impact": 0.6,
// 		"refs": [
// 			{
// 				"ref": []
// 			}
// 		],
func TestParseInspecReportWithRefBug(t *testing.T) {
	var report compliance.Report
	fromJSON("inspec-ref-bug-report.json", &report)

	_, err := Compliance(URL, &report)
	assert.NoError(t, err)
}

func TestRemovedControlsFailure(t *testing.T) {
	var report compliance.Report

	fromJSON("inspec-removed-controls-failure.json", &report)

	ev, _ := Compliance(URL, &report)

	failure := ev.GetComplianceFailure()

	require.Equal(t, 1, len(failure.FailedProfiles))
	failedProfile := failure.FailedProfiles[0]

	require.Equal(t, 1, len(failedProfile.FailedControls))
	failedControl := failedProfile.FailedControls[0]

	assert.Equal(t, int32(311), failedControl.Stats.NumTests)
	assert.Equal(t, int32(154), failedControl.Stats.NumFailedTests)
	assert.Equal(t, int32(100), failedControl.Stats.NumSkippedTests)
	assert.Equal(t, int32(57), failedControl.Stats.NumPassedTests)
}

func fromJSON(name string, out proto.Message) error {
	p := path.Join("testdata", name)

	content, err := ioutil.ReadFile(p)
	if err != nil {
		panic(err)
	}

	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	err = unmarshaler.Unmarshal(strings.NewReader(string(content)), out)

	if err != nil {
		panic(err)
	}

	return nil
}
