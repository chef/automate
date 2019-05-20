package publisher

import (
	"context"
	"time"

	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	ingest_inspec "github.com/chef/automate/components/compliance-service/ingest/events/inspec"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/golang/protobuf/ptypes"
	tspb "github.com/golang/protobuf/ptypes/timestamp"
	log "github.com/sirupsen/logrus"
)

func BuildNodeManagerPublisher(nodeManagerClient manager.NodeManagerServiceClient) message.CompliancePipe {
	return func(in <-chan message.Compliance) <-chan message.Compliance {
		return nodeManagerPublisher(in, nodeManagerClient)
	}
}

func nodeManagerPublisher(in <-chan message.Compliance, nodeManagerClient manager.NodeManagerServiceClient) <-chan message.Compliance {
	ctx := context.Background()
	maxNumberOfBundledMsgs := 100
	out := make(chan message.Compliance, maxNumberOfBundledMsgs)
	go func() {
		for msg := range in {
			// send to node manager from here.
			log.Infof("send info about node %s to node manager", msg.Report.NodeName)

			nodeMetadata, err := gatherInfoForNode(msg)
			if err != nil {
				log.Errorf("unable parse node data to be send to manager. aborting attempt to send info to mgr for node %s -- %v", msg.Report.NodeName, err)
				out <- msg
				continue
			}
			_, err = nodeManagerClient.ProcessNode(ctx, nodeMetadata)
			if err != nil {
				log.Errorf("unable to send info about node %s to node manager", msg.Report.NodeName)
			}

			out <- msg
		}
		close(out)
	}()

	return out
}

func gatherInfoForNode(in message.Compliance) (*manager.NodeMetadata, error) {
	// if the end_time on the report is not parsable, we want to set
	// the node last_contact time to beg of time, b/c we don't know when
	// the last_contact was
	endTime, err := time.Parse(time.RFC3339, in.Report.EndTime)
	if err != nil {
		log.Errorf("ProcessComplianceReport unable to parse report end_time, setting end_time to beg of time")
		endTime = time.Time{}
	}
	endTimeTimestamp, err := ptypes.TimestampProto(endTime)
	if err != nil {
		log.Errorf("ProcessComplianceReport unable to parse end_time as proto timestamp, setting end_time timestamp to beg of time")
		endTimeTimestamp = &tspb.Timestamp{}
	}
	log.Debugf("Calling sendNodeInfoToManager for report id %s", in.Report.ReportUuid)

	return &manager.NodeMetadata{
		Uuid:            in.Report.NodeUuid,
		Name:            in.Report.NodeName,
		PlatformName:    in.Report.Platform.GetName(),
		PlatformRelease: in.Report.Platform.GetRelease(),
		JobUuid:         in.Report.JobUuid,
		LastContact:     endTimeTimestamp,
		SourceId:        in.Report.SourceId,
		SourceRegion:    in.Report.SourceRegion,
		SourceAccountId: in.Report.SourceAccountId,
		Tags:            in.Report.Tags,
		// ProjectsData:    gatherProjectsData(in),
		// Projects: in.InspecReport.Projects,
		ScanData: &nodes.LastContactData{
			Id:      in.Report.ReportUuid,
			EndTime: endTimeTimestamp,
			Status:  getReportStatus(in.Report.Profiles),
		},
	}, nil
}

func gatherProjectsData(in *compliance.Report) map[string][]string {
	projectsData := make(map[string][]string)
	if len(in.GetEnvironment()) != 0 {
		projectsData["environment"] = []string{in.GetEnvironment()}
	}
	if len(in.GetRoles()) != 0 {
		projectsData["roles"] = in.GetRoles()
	}
	if len(in.GetPolicyName()) != 0 {
		projectsData["policy_name"] = []string{in.GetPolicyName()}
	}
	if len(in.GetPolicyGroup()) != 0 {
		projectsData["policy_group"] = []string{in.GetPolicyGroup()}
	}
	if len(in.GetOrganizationName()) != 0 {
		projectsData["organization_name"] = []string{in.GetOrganizationName()}
	}
	if len(in.GetChefTags()) != 0 {
		projectsData["chef_tags"] = in.GetChefTags()
	}
	if len(in.GetSourceFqdn()) != 0 {
		projectsData["chef_server"] = []string{in.GetSourceFqdn()}
	}
	return projectsData
}

func getReportStatus(profiles []*ingest_inspec.Profile) nodes.LastContactData_Status {
	// start with a status of passed as the default status
	status := nodes.LastContactData_PASSED
	skippedCounter := 0
	for _, profile := range profiles {
		profileStatus := getProfileStatus(profile)
		// if any profile is failed, report is failed
		if profileStatus == "failed" {
			status = nodes.LastContactData_FAILED
			break
		}
		// if all profiles are skipped, the report is skipped
		// so we keep a count of skipped profiles and later
		// check if the amount of profiles matches the skipped counter,
		// setting the status to skipped if they match.
		if profileStatus == "skipped" {
			skippedCounter++
		}
	}
	if skippedCounter == len(profiles) {
		status = nodes.LastContactData_SKIPPED
	}
	return status
}

func getProfileStatus(profile *ingest_inspec.Profile) string {
	// a profile skipped due to platform exceptions will have a status of skipped
	if profile.Status == "skipped" {
		return "skipped"
	}
	// set the defaults
	status := "passed"
	skippedCounter := 0
	for _, control := range profile.Controls {
		controlStatus := getControlStatus(control.Results)
		// if any control is failed, report is failed
		if controlStatus == "failed" {
			status = "failed"
			break
		}
		// if all controls are skipped, the profile is skipped
		// so we keep a count of skipped profiles and later
		// check if the amount of profiles matches the skipped counter,
		// setting the status to skipped if they match.
		if controlStatus == "skipped" {
			skippedCounter++
		}
	}
	if skippedCounter == len(profile.Controls) {
		status = "skipped"
	}
	return status
}

func getControlStatus(results []*ingest_inspec.Result) string {
	// set the defaults
	status := "passed"
	skippedCounter := 0
	for _, result := range results {
		// if any result is failed, control is failed
		if result.Status == "failed" {
			status = "failed"
			break
		}
		// if all results are skipped, the control is skipped
		if result.Status == "skipped" {
			skippedCounter++
		}
	}
	if skippedCounter == len(results) {
		status = "skipped"
	}
	return status
}
