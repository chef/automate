package publisher

import (
	"context"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/golang/protobuf/ptypes"
	log "github.com/sirupsen/logrus"
)

func BuildNodeManagerPublisher(nodeManagerClient manager.NodeManagerServiceClient) message.ChefRunPipe {
	return func(in <-chan message.ChefRun) <-chan message.ChefRun {
		return nodeManagerPublisher(in, nodeManagerClient)
	}
}

func nodeManagerPublisher(in <-chan message.ChefRun, nodeManagerClient manager.NodeManagerServiceClient) <-chan message.ChefRun {
	ctx := context.Background()
	maxNumberOfBundledMsgs := 100
	out := make(chan message.ChefRun, maxNumberOfBundledMsgs)
	go func() {
		for msg := range in {
			// send to node manager from here.
			log.Infof("send info about node %s to node manager", msg.Node.NodeName)

			nodeMetadata, err := gatherInfoForNode(msg.Node)
			if err != nil {
				log.Errorf("unable parse node data to be send to manager. aborting attempt to send info to mgr for node %s -- %v", msg.Node.NodeName, err)
				out <- msg
				continue
			}
			_, err = nodeManagerClient.ProcessNode(ctx, nodeMetadata)
			if err != nil {
				log.Errorf("unable to send info about node %s to node manager", msg.Node.NodeName)
			}

			out <- msg
		}
		close(out)
	}()

	return out
}

func gatherInfoForNode(node backend.Node) (*manager.NodeMetadata, error) {
	// convert node check in time to proto timestamp
	timestamp, err := ptypes.TimestampProto(node.Checkin)
	if err != nil {
		return nil, err
	}

	// translate status
	status := nodes.LastContactData_UNKNOWN
	switch node.Status {
	case "success":
		status = nodes.LastContactData_PASSED
	case "failure":
		status = nodes.LastContactData_FAILED
	}

	// translate tags
	tags := make([]*common.Kv, len(node.ChefTags))
	for i, tag := range node.ChefTags {
		tags[i] = &common.Kv{
			Key:   "chef-tag",
			Value: tag,
		}
	}

	return &manager.NodeMetadata{
		Uuid:            node.EntityUuid,
		Name:            node.NodeName,
		PlatformName:    node.Platform,
		PlatformRelease: node.PlatformVersion,
		LastContact:     timestamp,
		SourceId:        node.Ec2.InstanceId,
		SourceRegion:    node.Ec2.PlacementAvailabilityZone,
		Tags:            tags,
		ProjectsData:    gatherProjectsData(node),
		Projects:        node.Projects,
		RunData: &nodes.LastContactData{
			Id:      node.LatestRunID,
			EndTime: timestamp,
			Status:  status,
		},
	}, nil
}

func gatherProjectsData(in backend.Node) []*nodes.ProjectsData {
	projectsData := make([]*nodes.ProjectsData, 0)
	if len(in.Environment) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "environment", Values: []string{in.Environment}})
	}
	if len(in.Roles) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "roles", Values: in.Roles})
	}
	if len(in.PolicyName) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "policy_name", Values: []string{in.PolicyName}})
	}
	if len(in.PolicyGroup) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "policy_group", Values: []string{in.PolicyGroup}})
	}
	if len(in.OrganizationName) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "organization_name", Values: []string{in.OrganizationName}})
	}
	if len(in.ChefTags) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "chef_tags", Values: in.ChefTags})
	}
	if len(in.SourceFqdn) != 0 {
		projectsData = append(projectsData, &nodes.ProjectsData{Key: "chef_server", Values: []string{in.SourceFqdn}})
	}
	return projectsData
}
