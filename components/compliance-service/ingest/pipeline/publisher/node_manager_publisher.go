package publisher

import (
	"context"
	"time"

	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
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
			if err := msg.Ctx.Err(); err != nil {
				msg.FinishProcessingCompliance(err)
				continue
			}
			// send to node manager from here.
			log.Debugf("send info about node %s to node manager", msg.Report.NodeName)

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

			message.Propagate(out, &msg)
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

	// translate status
	status := nodes.LastContactData_UNKNOWN
	switch in.InspecReport.Status {
	case "passed":
		status = nodes.LastContactData_PASSED
	case "failed":
		status = nodes.LastContactData_FAILED
	case "skipped":
		status = nodes.LastContactData_SKIPPED
	}

	// translate tags
	tags := in.Report.GetTags()
	for _, tag := range in.Report.GetChefTags() {
		tags = append(tags, &common.Kv{
			Key:   "chef-tag",
			Value: tag,
		})
	}
	if in.Report.GetEnvironment() != "" {
		tags = append(tags, &common.Kv{Key: "environment", Value: in.Report.GetEnvironment()})
	}

	mgrType := in.Report.GetAutomateManagerType()
	if in.Report.GetSourceFqdn() != "" {
		mgrType = "chef"
	}
	return &manager.NodeMetadata{
		Uuid:            in.Report.GetNodeUuid(),
		Name:            in.Report.GetNodeName(),
		PlatformName:    in.Report.GetPlatform().GetName(),
		PlatformRelease: in.Report.GetPlatform().GetRelease(),
		JobUuid:         in.Report.GetJobUuid(),
		LastContact:     endTimeTimestamp,
		SourceId:        in.Report.GetSourceId(),
		SourceRegion:    in.Report.GetSourceRegion(),
		SourceAccountId: in.Report.GetSourceAccountId(),
		Tags:            tags,
		ProjectsData:    gatherProjectsData(&in.Report),
		Projects:        in.InspecReport.Projects,
		ScanData: &nodes.LastContactData{
			Id:      in.Report.ReportUuid,
			EndTime: endTimeTimestamp,
			Status:  status,
		},
		ManagerId:   in.Report.GetAutomateManagerId(),
		ManagerType: mgrType,
	}, nil
}

func gatherProjectsData(in *compliance.Report) []*nodes.ProjectsData {
	projectsData := make([]*nodes.ProjectsData, 0)
	if len(in.GetEnvironment()) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "environment", Values: []string{in.GetEnvironment()}})
	}
	if len(in.GetRoles()) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "roles", Values: in.GetRoles()})
	}
	if len(in.GetPolicyName()) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "policy_name", Values: []string{in.GetPolicyName()}})
	}
	if len(in.GetPolicyGroup()) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "policy_group", Values: []string{in.GetPolicyGroup()}})
	}
	if len(in.GetOrganizationName()) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "organization_name", Values: []string{in.GetOrganizationName()}})
	}
	if len(in.GetChefTags()) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "chef_tags", Values: in.GetChefTags()})
	}
	if len(in.GetSourceFqdn()) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "chef_server", Values: []string{in.GetSourceFqdn()}})
	}
	return projectsData
}
