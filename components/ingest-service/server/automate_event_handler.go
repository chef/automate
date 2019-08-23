package server

import (
	"context"
	"fmt"

	"github.com/golang/protobuf/ptypes"
	tspb "github.com/golang/protobuf/ptypes/timestamp"
	"github.com/sirupsen/logrus"
	log "github.com/sirupsen/logrus"

	chef "github.com/chef/automate/api/external/ingest/request"
	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	ingest_api "github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/components/event-service/server"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/config"
	project_update_lib "github.com/chef/automate/lib/authz"
	event_ids "github.com/chef/automate/lib/event"
)

type AutomateEventHandlerServer struct {
	client           backend.Client
	chefIngestServer ChefIngestServer
	updateManager    *project_update_lib.DomainProjectUpdateManager
}

func NewAutomateEventHandlerServer(client backend.Client, chefIngestServer ChefIngestServer,
	authzProjectsClient iam_v2.ProjectsClient, eventServiceClient automate_event.EventServiceClient,
	configManager *config.Manager) *AutomateEventHandlerServer {
	updateManager := project_update_lib.NewDomainProjectUpdateManager(client, authzProjectsClient,
		eventServiceClient, configManager, event_ids.InfraClientRunsProducerID)
	server := &AutomateEventHandlerServer{
		client:           client,
		chefIngestServer: chefIngestServer,
		updateManager:    updateManager,
	}
	return server
}

func (s *AutomateEventHandlerServer) HandleEvent(ctx context.Context,
	req *automate_event.EventMsg) (*automate_event.EventResponse, error) {
	logrus.Debugf("ingest-service is handling your event %s", req.EventID)
	response := &automate_event.EventResponse{}
	if req.Type.Name == server.NodeTerminated {
		instanceID := req.Object.ID
		// It is very likely that there is only one instanceId to nodeId
		// just in case this is not true we will handle it
		nodeIDs, err := s.client.FindNodeIDByInstanceId(ctx, instanceID)
		if err != nil {
			logrus.Warnf("Error finding node by instance Id %s: %s", instanceID, err)
			return response, err
		}
		if len(nodeIDs) == 0 {
			logrus.Infof("No nodes found in client runs for terminated instance %s", instanceID)
		}
		for _, nodeID := range nodeIDs {
			nodeDelete := &chef.Delete{
				Id:              req.EventID,
				NodeId:          nodeID,
				ServiceHostname: "Node Manager",
			}
			logrus.Infof("ingest-service is deleting node with instance ID %s", instanceID)
			_, err := s.chefIngestServer.ProcessNodeDelete(ctx, nodeDelete)
			if err != nil {
				logrus.Warnf("ingest service can't handle event: %s", err)
				return response, err
			}
		}
	} else if req.Type.Name == server.ProjectRulesUpdate {
		projectUpdateID, err := getProjectUpdateID(req)
		if err != nil {
			logrus.Errorf("Project Rule Update sent without a ProjectUpdateID eventID %q",
				req.EventID)
			return response, err
		}

		s.updateManager.Start(projectUpdateID)
	} else if req.Type.Name == server.ProjectRulesCancelUpdate {
		projectUpdateID, err := getProjectUpdateID(req)
		if err != nil {
			logrus.Errorf("Project Rule Update Cancel sent without a ProjectUpdateID. eventID %q",
				req.EventID)
			return response, err
		}

		s.updateManager.Cancel(projectUpdateID)
	}
	return response, nil
}

func (s *AutomateEventHandlerServer) ProjectUpdateStatus(ctx context.Context,
	req *ingest_api.ProjectUpdateStatusReq) (*ingest_api.ProjectUpdateStatusResp, error) {
	time, err := ptypes.TimestampProto(s.updateManager.EstimatedTimeComplete())
	if err != nil {
		log.Errorf("Could not convert EstimatedTimeComplete to protobuf Timestamp %v", err)
		time = &tspb.Timestamp{}
	}
	return &ingest_api.ProjectUpdateStatusResp{
		State:                 s.updateManager.State(),
		PercentageComplete:    float32(s.updateManager.PercentageComplete()),
		EstimatedTimeComplete: time,
	}, nil
}

func getProjectUpdateID(event *automate_event.EventMsg) (string, error) {
	if event.Data != nil && event.Data.Fields != nil &&
		event.Data.Fields["ProjectUpdateID"] != nil &&
		event.Data.Fields["ProjectUpdateID"].GetStringValue() != "" {
		return event.Data.Fields["ProjectUpdateID"].GetStringValue(), nil
	}

	return "", fmt.Errorf("Project Rule Update sent without a ProjectUpdateID eventID: %q",
		event.EventID)
}
