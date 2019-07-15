package server

import (
	"time"

	log "github.com/sirupsen/logrus"
	"golang.org/x/net/context"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/api/external/ingest/response"
	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline"
	"github.com/chef/automate/components/ingest-service/serveropts"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/lib/version"
)

type ChefIngestServer struct {
	chefRunPipeline    pipeline.ChefRunPipeline
	chefActionPipeline pipeline.ChefActionPipeline
	client             backend.Client
	authzClient        iam_v2.ProjectsClient
	nodeMgrClient      manager.NodeManagerServiceClient
}

// NewChefIngestServer creates a new server instance and it automatically
// initializes the ChefRun Pipeline by consuming the provided
// backend client
func NewChefIngestServer(client backend.Client, authzClient iam_v2.ProjectsClient,
	nodeMgrClient manager.NodeManagerServiceClient,
	chefIngestServerConfig serveropts.ChefIngestServerConfig) *ChefIngestServer {
	return &ChefIngestServer{
		chefRunPipeline: pipeline.NewChefRunPipeline(client, authzClient,
			nodeMgrClient, chefIngestServerConfig.MaxNumberOfBundledRunMsgs,
			chefIngestServerConfig.NumberOfRunMsgsTransformers),
		chefActionPipeline: pipeline.NewChefActionPipeline(client, authzClient,
			chefIngestServerConfig.MaxNumberOfBundledActionMsgs),
		client:        client,
		authzClient:   authzClient,
		nodeMgrClient: nodeMgrClient,
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

		s.chefRunPipeline.Run(run, errc)
		err = <-errc

		if err != nil {
			log.WithError(err).Error("Message failure")
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

		s.chefActionPipeline.Run(action, errc)
		err := <-errc

		if err != nil {
			log.WithError(err).Error("Message failure")
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
func (s *ChefIngestServer) ProcessMultipleNodeDeletes(ctx context.Context, multipleNodeDeleteRequest *chef.MultipleNodeDeleteRequest) (*response.ProcessMultipleNodeDeleteResponse, error) {
	log.WithFields(log.Fields{
		"func":    nameOfFunc(),
		"Request": multipleNodeDeleteRequest}).Debug("rpc call")

	_, err := s.client.MarkForDeleteMultipleNodesByID(ctx, multipleNodeDeleteRequest.NodeIds)

	return &response.ProcessMultipleNodeDeleteResponse{}, err
}

// ProcessNodeDelete send a delete action threw the action pipeline
func (s *ChefIngestServer) ProcessNodeDelete(ctx context.Context, delete *chef.Delete) (*response.ProcessNodeDeleteResponse, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")
	// Validate on presence of delete fields, add RecordedAt Time
	if delete.GetNodeId() != "" ||
		// Must have NodeId OR all three of the other three fields
		(delete.GetOrganizationName() != "" &&
			delete.GetServiceHostname() != "" &&
			delete.GetNodeName() != "") {
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

		return &response.ProcessNodeDeleteResponse{}, err
	}

	errMsg := "Unknown NodeName, OrganizationName and RemoteHostname, or NodeId"
	log.WithFields(log.Fields{
		"message_type": "delete",
		"error":        errMsg,
	}).Error("not processing")

	return &response.ProcessNodeDeleteResponse{}, status.Errorf(codes.InvalidArgument, "Missing one or more necessary fields. %s", errMsg)
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
