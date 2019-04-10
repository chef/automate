package builder

import (
	"fmt"
	"strings"
	"time"

	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/events/inspec"
	. "github.com/chef/automate/components/notifications-client/api"
	"github.com/golang/protobuf/jsonpb"
	proto "github.com/golang/protobuf/proto"
)

// Inspec builds an inspec event that can be sent to the notifier
// url is the base automate url
// TODO: url is the url where we can find more information about the event
func Compliance(url string, report *compliance.Report) (*Event, error) {
	var ev Event
	ev.Id = report.GetReportUuid()
	stats, err := collect(report)

	if err != nil {
		return nil, err
	}

	compliance_url := complianceUrlFor(url, report.GetNodeUuid())

	if stats.failed_total > 0 {
		// report failure
		compliance_failure := ComplianceFailure{
			Id:            report.GetReportUuid(),
			NodeName:      report.GetNodeName(),
			NodeId:        report.GetNodeUuid(),
			InspecVersion: report.GetVersion(),
			TestTotals: &ComplianceFailure_ControlTotals{
				Skipped:        stats.skipped,
				Passed:         stats.passed_total,
				Failed:         stats.failed_total,
				Critical:       stats.num_critical,
				CriticalFailed: stats.failed_critical,
			},
			FailedProfiles: stats.failed_profiles,
			ComplianceUrl:  compliance_url,
			EndTime:        report.GetEndTime(),
			Timestamp:      time.Now().Format(time.RFC3339),
		}
		ev.Event = &Event_ComplianceFailure{&compliance_failure}
	} else {
		compliance_success := ComplianceSuccess{
			Id:            report.GetReportUuid(),
			NodeName:      report.GetNodeName(),
			NodeId:        report.GetNodeUuid(),
			ComplianceUrl: compliance_url,
			EndTime:       report.GetEndTime(),
			Timestamp:     time.Now().Format(time.RFC3339),
		}
		ev.Event = &Event_ComplianceSuccess{&compliance_success}
	}

	return &ev, nil
}

//TODO: move to gateway
func complianceUrlFor(base_url string, node_uuid string) string {
	return fmt.Sprintf("%s/compliance/reporting/nodes/%s", base_url, node_uuid)
}

type complianceReportStats struct {
	total           int32
	passed_total    int32
	num_critical    int32
	skipped         int32
	failed_total    int32
	failed_minor    int32
	failed_major    int32
	failed_critical int32
	failed_profiles []*Profile
}

func collectNotificationsProfile(profile *inspec.Profile, agg *complianceReportStats) (*Profile, error) {
	var notifications_profile Profile
	err := transform(profile, &notifications_profile)
	if err != nil {
		return nil, err
	}

	notifications_profile.FailedControls = make([]*Profile_Control, 0, len(profile.GetControls()))
	var stats Profile_ControlTotals
	for _, control := range profile.GetControls() {
		transformed_control, err := collectNotificationsControl(control, agg)
		if err != nil {
			return nil, err
		}
		stats.NumTests += 1
		if transformed_control.Stats.NumFailedTests > 0 {
			stats.NumFailedTests += 1
			notifications_profile.FailedControls = append(notifications_profile.FailedControls, transformed_control)
		} else if transformed_control.Stats.NumPassedTests == 0 {
			stats.NumSkippedTests += 1
		} else {
			stats.NumPassedTests += 1
		}
	}
	notifications_profile.Stats = &stats
	return &notifications_profile, nil
}

func collectNotificationsControl(control *inspec.Control, agg *complianceReportStats) (*Profile_Control, error) {
	var notifications_control Profile_Control
	err := transform(control, &notifications_control)
	if err != nil {
		return nil, err
	}

	passed := 0
	skipped := 0
	failed := 0
	total := 0

	// Each control can have more than 1 test
	notifications_control.FailedResults = make([]*Profile_Control_Result, 0, len(control.GetResults()))
	for _, result := range control.GetResults() {
		total = total + 1
		switch result.GetStatus() {
		case "passed":
			passed += 1
		case "skipped":
			skipped += 1
		case "failed":
			var transformed_result Profile_Control_Result
			err = transform(result, &transformed_result)
			if err != nil {
				return nil, err
			}
			notifications_control.FailedResults = append(notifications_control.FailedResults, &transformed_result)
			failed += 1
		}
	}

	agg.total += 1
	impact := control.GetImpact()
	if failed != 0 {
		agg.failed_total += 1
		if impact < 0.4 {
			agg.failed_minor += 1
		} else if impact < 0.7 {
			agg.failed_major += 1
		} else {
			agg.failed_critical += 1
		}
	} else if passed == 0 {
		// failed was 0 and passed was 0... either the control is empty
		// or everything was skipped. Either way, skipped is appropriate
		// for the control
		agg.skipped += 1
	} else {
		agg.passed_total += 1
	}

	if impact >= 0.7 {
		agg.num_critical += 1
	}

	var stats Profile_Control_ResultTotals
	stats.NumTests = int32(total)
	stats.NumFailedTests = int32(failed)
	stats.NumSkippedTests = int32(skipped)
	stats.NumPassedTests = int32(passed)

	notifications_control.Stats = &stats

	return &notifications_control, nil
}

//TODO: We have ideas for a nicer transformer that wont go to json and back
func transform(in proto.Message, out proto.Message) error {
	str, err := (&jsonpb.Marshaler{OrigName: true}).MarshalToString(in)

	if err != nil {
		return err
	}

	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	err = unmarshaler.Unmarshal(strings.NewReader(str), out)

	if err != nil {
		return err
	}

	return nil
}

func collect(report *compliance.Report) (*complianceReportStats, error) {
	var stats complianceReportStats
	for _, profile := range report.GetProfiles() {
		p, err := collectNotificationsProfile(profile, &stats)
		if err != nil {
			return nil, err
		} else if p.Stats.NumFailedTests > 0 {
			stats.failed_profiles = append(stats.failed_profiles, p)
		}
	}
	return &stats, nil
}
