package reportingtest

import (
	"reflect"
	"testing"

	"github.com/chef/automate/components/compliance-service/api/reporting"
)

func TestExport(t *testing.T) {
	t.Log("Testing the json export with profile filter of apache-baseline, expecting success!")
	profileFilter := reporting.ListFilter{Type: "profile_id", Values: []string{"apache-baseline"}}
	filters := []*reporting.ListFilter{&profileFilter}
	q := reporting.Query{Type: "json", Filters: filters}
	res, err := Export(&q)
	if err != nil {
		t.Errorf("Expected json report results, got error %v", err)
		t.Fail()
	}
	exportData := reporting.ExportData{}
	if reflect.TypeOf(res) != reflect.TypeOf(&exportData) {
		t.Errorf("Expected json report results, got %v", res)
		t.Fail()
	}
	t.Log("Report Json Retrieved")
}

func TestExportWithNonExistentProfile(t *testing.T) {
	t.Log("Testing the json export with non-existent profile, expecting success!")
	profileFilter := reporting.ListFilter{Type: "profile_id", Values: []string{"i-am-not-here"}}
	filters := []*reporting.ListFilter{&profileFilter}
	q := reporting.Query{Type: "json", Filters: filters}
	res, err := Export(&q)
	if err != nil {
		t.Errorf("Expected json report results, got error %v", err)
		t.Fail()
	}
	exportData := reporting.ExportData{}
	if reflect.TypeOf(res) != reflect.TypeOf(&exportData) {
		t.Errorf("Expected json report results, got %v", res)
		t.Fail()
	}
	t.Log("Report Json Retrieved")
}
