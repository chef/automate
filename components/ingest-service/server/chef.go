package server

import (
	"context"
	"errors"
	"time"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/api/external/ingest/response"
	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline"
	"github.com/chef/automate/lib/version"
)

type ChefIngestServer struct {
	chefRunPipeline    pipeline.ChefRunPipeline
	chefActionPipeline pipeline.ChefActionPipeline
	client             backend.Client
	authzClient        authz.ProjectsServiceClient
	nodeMgrClient      manager.NodeManagerServiceClient
	nodesClient        nodes.NodesServiceClient
}

// NewChefIngestServer creates a new server instance and it automatically
// initializes the ChefRun Pipeline by consuming the provided
// backend client
func NewChefIngestServer(client backend.Client, authzClient authz.ProjectsServiceClient,
	nodeMgrClient manager.NodeManagerServiceClient,
	nodesClient nodes.NodesServiceClient,
	actionPipeline pipeline.ChefActionPipeline,
	chefRunPipeline pipeline.ChefRunPipeline) *ChefIngestServer {
	return &ChefIngestServer{
		chefRunPipeline:    chefRunPipeline,
		chefActionPipeline: actionPipeline,
		client:             client,
		authzClient:        authzClient,
		nodeMgrClient:      nodeMgrClient,
		nodesClient:        nodesClient,
	}
}

func (s *ChefIngestServer) TotalChefRunMessages() int64 {
	return s.chefRunPipeline.GetTotalMessages()
}
func (s *ChefIngestServer) TotalChefActionMessages() int64 {
	return s.chefActionPipeline.GetTotalMessages()
}

// ProcessChefRun
func (s *ChefIngestServer) ProcessChefRun(ctx context.Context, run *chef.Run) (*response.ProcessChefRunResponse, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")
	var err error

	// Only ingesting 'run_converge' messages
	if run.GetMessageType() == "run_converge" {
		errc := make(chan error)
		defer close(errc)

		if errQ := s.chefRunPipeline.Run(ctx, run, errc); errQ != nil {
			err = errQ
		} else {
			err = <-errc
		}

		if err != nil {
			log.WithError(err).Error("Chef run ingestion failure")
		}
	} else if run.GetMessageType() == "run_start" {
		log.WithFields(log.Fields{
			"message_type": run.GetMessageType(),
		}).Info("Unsupported message (currently not processing)")
	} else {
		err = status.Errorf(codes.Unimplemented, "Unsupported message type %q", run.GetMessageType())
		log.WithError(err).Error("Unsupported message (not processing)")
	}

	return &response.ProcessChefRunResponse{}, err
}

// ProcessChefAction
func (s *ChefIngestServer) ProcessChefAction(ctx context.Context, action *chef.Action) (*response.ProcessChefActionResponse, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	if action.GetMessageType() == "action" {
		if action.GetTask() == "" || action.GetEntityType() == "" {
			return &response.ProcessChefActionResponse{}, status.Error(codes.InvalidArgument, "Message missing task or entity_type")
		}

		errc := make(chan error)
		defer close(errc)

		var err error
		if errQ := s.chefActionPipeline.Run(ctx, action, errc); errQ != nil {
			err = errQ
		} else {
			err = <-errc
		}

		if err != nil {
			log.WithError(err).Error("Chef Action ingestion failure")
		}
		return &response.ProcessChefActionResponse{}, err
	}

	err := status.Errorf(codes.Unimplemented, "Unsupported message type %q", action.GetMessageType())
	log.WithError(err).Error("Unsupported message (not processing)")

	return &response.ProcessChefActionResponse{}, err
}

func (s *ChefIngestServer) ProcessLivenessPing(ctx context.Context, liveness *chef.Liveness) (*response.ProcessLivenessResponse, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")
	if liveness.Source == "liveness_agent" &&
		liveness.EventType == "node_ping" &&
		liveness.EntityUuid != "" {

		iLiveness := backend.Liveness{
			NodeID:          liveness.EntityUuid,
			Checkin:         time.Now().UTC(),
			LivenessManaged: true,
			Organization:    liveness.OrganizationName,
			SourceFQDN:      liveness.ChefServerFqdn,
			NodeName:        liveness.NodeName,
		}
		err := s.client.RecordLivenessPing(ctx, iLiveness)
		return &response.ProcessLivenessResponse{}, err
	}

	errMsg := "Unknown ping event or missing node id."
	log.WithFields(log.Fields{
		"message_type": "liveness",
		"error":        errMsg,
	}).Error("Unable to record node ping")

	return &response.ProcessLivenessResponse{}, status.Errorf(codes.InvalidArgument, "Missing one or more necessary fields. %s", errMsg)
}

// ProcessMultipleNodeDeletes send multiple deletes actions
func (s *ChefIngestServer) ProcessMultipleNodeDeletes(ctx context.Context,
	multipleNodeDeleteRequest *chef.MultipleNodeDeleteRequest) (*response.ProcessMultipleNodeDeleteResponse, error) {
	log.WithFields(log.Fields{
		"func":    nameOfFunc(),
		"Request": multipleNodeDeleteRequest}).Debug("rpc call")

	_, err := s.client.MarkForDeleteMultipleNodesByID(ctx, multipleNodeDeleteRequest.NodeIds)
	if err != nil {
		return &response.ProcessMultipleNodeDeleteResponse{}, err
	}

	for _, nodeID := range multipleNodeDeleteRequest.NodeIds {
		_, err = s.nodesClient.Delete(ctx, &nodes.Id{Id: nodeID})
		if err != nil {
			return &response.ProcessMultipleNodeDeleteResponse{}, err
		}
	}

	return &response.ProcessMultipleNodeDeleteResponse{}, nil
}

// ProcessNodeDelete send a delete action threw the action pipeline
func (s *ChefIngestServer) ProcessNodeDelete(ctx context.Context,
	delete *chef.Delete) (*response.ProcessNodeDeleteResponse, error) {
	var (
		nodeIDs []string
		err     error
	)
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	if delete.GetNodeId() != "" {
		filters := map[string]string{
			"entity_uuid": delete.GetNodeId(),
		}
		nodeIDs, err = s.client.FindNodeIDsByFields(ctx, filters)
		if err != nil {
			return &response.ProcessNodeDeleteResponse{}, err
		}
	} else if delete.GetOrganizationName() != "" &&
		delete.GetServiceHostname() != "" &&
		delete.GetNodeName() != "" {
		filters := map[string]string{
			"organization_name": delete.GetOrganizationName(),
			"node_name":         delete.GetNodeName(),
			"source_fqdn":       delete.GetServiceHostname(),
		}
		nodeIDs, err = s.client.FindNodeIDsByFields(ctx, filters)
		if err != nil {
			return &response.ProcessNodeDeleteResponse{}, err
		}
	} else {
		errMsg := "Unknown NodeName, OrganizationName and RemoteHostname, or NodeId"
		log.WithFields(log.Fields{
			"message_type": "delete",
			"error":        errMsg,
		}).Error("not processing")

		return &response.ProcessNodeDeleteResponse{}, status.Errorf(codes.InvalidArgument, "Missing one or more necessary fields. %s", errMsg)
	}

	if len(nodeIDs) == 0 {
		return &response.ProcessNodeDeleteResponse{}, errors.New("NodeId not found")
	}

	for _, nodeID := range nodeIDs {
		action := chef.Action{
			Id:               delete.GetId(),
			EntityName:       delete.GetNodeName(),
			OrganizationName: delete.GetOrganizationName(),
			ServiceHostname:  delete.GetServiceHostname(),
			NodeId:           delete.GetNodeId(),
			MessageType:      "action",
			Task:             "delete",
			EntityType:       "node",
			RecordedAt:       time.Now().Format(time.RFC3339),
		}

		_, err := s.ProcessChefAction(ctx, &action)
		if err != nil {
			return &response.ProcessNodeDeleteResponse{}, err
		}

		_, err = s.nodesClient.Delete(ctx, &nodes.Id{Id: nodeID})
		if err != nil {
			return &response.ProcessNodeDeleteResponse{}, err
		}
	}

	return &response.ProcessNodeDeleteResponse{}, nil
}

// GetVersion returns the service version
func (s *ChefIngestServer) GetVersion(ctx context.Context, empty *ingest.VersionRequest) (*ingest.Version, error) {
	return &ingest.Version{
		Version: version.Version,
		Built:   version.BuildTime,
		Name:    SERVICE_NAME,
		Sha:     version.GitSHA,
	}, nil
}
