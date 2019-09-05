package server

import (
	"context"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	chef "github.com/chef/automate/api/external/ingest/request"
	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	ingest_api "github.com/chef/automate/api/interservice/ingest"
	event "github.com/chef/automate/components/event-service/config"
	"github.com/chef/automate/components/ingest-service/backend"
)

type AutomateEventHandlerServer struct {
	client           backend.Client
	chefIngestServer ChefIngestServer
}

func NewAutomateEventHandlerServer(client backend.Client, chefIngestServer ChefIngestServer,
	authzProjectsClient iam_v2.ProjectsClient, eventServiceClient automate_event.EventServiceClient) *AutomateEventHandlerServer {

	server := &AutomateEventHandlerServer{
		client:           client,
		chefIngestServer: chefIngestServer,
	}
	return server
}

func (s *AutomateEventHandlerServer) HandleEvent(ctx context.Context,
	req *automate_event.EventMsg) (*automate_event.EventResponse, error) {
	logrus.Debugf("ingest-service is handling your event %s", req.EventID)
	response := &automate_event.EventResponse{}
	if req.Type.Name == event.NodeTerminatedEventName {
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
	}
	return response, nil
}

func (s *AutomateEventHandlerServer) ProjectUpdateStatus(ctx context.Context,
	req *ingest_api.ProjectUpdateStatusReq) (*ingest_api.ProjectUpdateStatusResp, error) {

	return nil, status.Error(codes.Unimplemented, "Endpoint no longer used")
}
