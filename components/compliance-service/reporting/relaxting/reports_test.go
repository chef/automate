package relaxting

import (
	"testing"

	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/stretchr/testify/assert"
)

func TestConvertControlProcessesTags(t *testing.T) {
	profileControlsMap := make(map[string]*reportingapi.Control, 2)

	profileControlsMap["123"] = &reportingapi.Control{
		Id:     "123",
		Impact: 1,
		Title:  "Running worker process as non-privileged user",
	}
	inspecReportControl := ESInSpecReportControl{
		ID: "123",
		StringTags: []ESInSpecReportControlStringTags{
			{Key: "single string"},
			{Key: "test", Values: []string{"one", "two"}},
			{Key: "test 2", Values: []string{"one val"}},
		},
	}

	convertedControl := convertControl(profileControlsMap, inspecReportControl, map[string][]string{})

	stringTags := make(map[string]*reportingapi.TagValues, 0)
	stringTags["test"] = &reportingapi.TagValues{Values: []string{"one", "two"}}
	stringTags["single string"] = &reportingapi.TagValues{Values: []string{"null"}}
	stringTags["test 2"] = &reportingapi.TagValues{Values: []string{"one val"}}

	assert.Equal(t, &reportingapi.Control{
		Id:         "123",
		Impact:     1,
		Title:      "Running worker process as non-privileged user",
		StringTags: stringTags,
		Refs:       make([]*reportingapi.Ref, 0),
		Results:    make([]*reportingapi.Result, 0),
	}, convertedControl)
}

func setup() (map[string][]string, map[string]*reportingapi.Control) {
	filters := make(map[string][]string, 0)
	profileControlsMap := make(map[string]*reportingapi.Control, 3)

	profileControlsMap["123"] = &reportingapi.Control{
		Id:     "123",
		Impact: 1,
		Title:  "Running worker process as non-privileged user",
	}

	profileControlsMap["456"] = &reportingapi.Control{
		Id:     "456",
		Impact: 1,
		Title:  "Another Control",
	}

	profileControlsMap["789"] = &reportingapi.Control{
		Id:     "789",
		Impact: 1,
		Title:  "Another Control",
	}
	return filters, profileControlsMap
}

func TestConvertControlFiltersByTagNoMatch(t *testing.T) {
	filters, profileControlsMap := setup()
	filters["control_tag:nist"] = []string{"test-1"}

	inspecReportControl123 := ESInSpecReportControl{
		ID: "123",
		StringTags: []ESInSpecReportControlStringTags{
			{Key: "single string"},
			{Key: "test", Values: []string{"one", "two"}},
		},
	}

	convertedControl := convertControl(profileControlsMap, inspecReportControl123, filters)

	expected := &reportingapi.Control{}
	expected = nil

	assert.Equal(t, expected, convertedControl)
}

func TestConvertControlFiltersByTagValMatch(t *testing.T) {
	filters, profileControlsMap := setup()

	inspecReportControl456 := ESInSpecReportControl{
		ID: "456",
		StringTags: []ESInSpecReportControlStringTags{
			{Key: "nist", Values: []string{"test-1"}},
		},
	}

	convertedControl := convertControl(profileControlsMap, inspecReportControl456, filters)

	stringTags := make(map[string]*reportingapi.TagValues, 0)
	stringTags["nist"] = &reportingapi.TagValues{Values: []string{"test-1"}}

	assert.Equal(t, &reportingapi.Control{
		Id:         "456",
		Impact:     1,
		Title:      "Another Control",
		StringTags: stringTags,
		Refs:       make([]*reportingapi.Ref, 0),
		Results:    make([]*reportingapi.Result, 0),
	}, convertedControl)
}
func TestConvertControlFiltersByTagOnlyMatch(t *testing.T) {
	filters, profileControlsMap := setup()

	inspecReportControl789 := ESInSpecReportControl{
		ID: "789",
		StringTags: []ESInSpecReportControlStringTags{
			{Key: "cci", Values: []string{""}},
		},
		WaivedStr: "yes_run",
		WaiverData: &ESInSpecReportControlsWaiverData{
			ExpirationDate:     "2025-06-01",
			Run:                true,
			Justification:      "Some reason",
			SkippedDueToWaiver: false,
			Message:            "Some message",
		},
		RemovedResultsCounts: &ESInSpecReportControlRemovedResultsCounts{
			Failed:  11,
			Passed:  12,
			Skipped: 13,
		},
	}

	filters["control_tag:cci"] = []string{""}
	convertedControl := convertControl(profileControlsMap, inspecReportControl789, filters)

	stringTags := make(map[string]*reportingapi.TagValues, 0)
	stringTags["cci"] = &reportingapi.TagValues{Values: []string{""}}

	assert.Equal(t, &reportingapi.Control{
		Id:         "789",
		Impact:     1,
		Title:      "Another Control",
		StringTags: stringTags,
		Refs:       make([]*reportingapi.Ref, 0),
		Results:    make([]*reportingapi.Result, 0),
		WaivedStr:  "yes_run",
		WaiverData: &reportingapi.OrigWaiverData{
			ExpirationDate:     "2025-06-01",
			Run:                true,
			Justification:      "Some reason",
			SkippedDueToWaiver: false,
			Message:            "Some message",
		},
		RemovedResultsCounts: &reportingapi.RemovedResultsCounts{
			Failed:  11,
			Passed:  12,
			Skipped: 13,
		},
	}, convertedControl)
}

func TestDoesControlTagMatchFilter(t *testing.T) {
	// json tags set one
	tags1 := map[string]*reportingapi.TagValues{}
	tags1["test"] = &reportingapi.TagValues{
		Values: []string{"one", "two"},
	}
	// json tags set two
	tags2 := map[string]*reportingapi.TagValues{}
	tags2["shoe"] = &reportingapi.TagValues{
		Values: []string{"blue"},
	}
	tags2["sock"] = &reportingapi.TagValues{
		Values: []string{"yellow", "pink"},
	}
	// json tags set three
	tags3 := map[string]*reportingapi.TagValues{}
	tags3["key_only"] = &reportingapi.TagValues{
		Values: []string{"null"},
	}

	filterSock := map[string][]string{}
	filterSock["control_tag:sock"] = []string{"yellow"}

	filterShoe := map[string][]string{}
	filterShoe["control_tag:shoe"] = []string{"blue"}

	filterNull := map[string][]string{}
	filterNull["control_tag:key_only"] = []string{"null"}

	// test matching filter
	assert.Equal(t, true, doesControlTagMatchFilter(filterSock, tags2))
	assert.Equal(t, true, doesControlTagMatchFilter(filterShoe, tags2))

	// test no match filter
	assert.Equal(t, false, doesControlTagMatchFilter(filterSock, tags3))
	assert.Equal(t, false, doesControlTagMatchFilter(filterNull, tags2))

	// test null match
	assert.Equal(t, true, doesControlTagMatchFilter(filterNull, tags3))

	// test multiple control tag filters
	multFilters := filterSock
	multFilters["control_tag:shoe"] = []string{"blue"}
	assert.Equal(t, true, doesControlTagMatchFilter(multFilters, tags2))
}
