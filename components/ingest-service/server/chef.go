package server

import (
	"context"
	"encoding/json"
	"strings"
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
	"github.com/chef/automate/components/ingest-service/storage"
	"github.com/chef/automate/lib/version"
)

var skipIndices = map[string]bool{
	"security-auditlog":         true,
	".opendistro":               true,
	".plugins-ml-config":        true,
	".opensearch-observability": true,
}

type ChefIngestServer struct {
	chefRunPipeline    pipeline.ChefRunPipeline
	chefActionPipeline pipeline.ChefActionPipeline
	client             backend.Client
	authzClient        authz.ProjectsServiceClient
	nodeMgrClient      manager.NodeManagerServiceClient
	nodesClient        nodes.NodesServiceClient
	db                 *storage.DB // Added field for database access
}

// NewChefIngestServer creates a new server instance and it automatically
// initializes the ChefRun Pipeline by consuming the provided
// backend client
func NewChefIngestServer(client backend.Client, authzClient authz.ProjectsServiceClient,
	nodeMgrClient manager.NodeManagerServiceClient,
	nodesClient nodes.NodesServiceClient,
	actionPipeline pipeline.ChefActionPipeline,
	chefRunPipeline pipeline.ChefRunPipeline,
	db *storage.DB) *ChefIngestServer { // Added db parameter
	return &ChefIngestServer{
		chefRunPipeline:    chefRunPipeline,
		chefActionPipeline: actionPipeline,
		client:             client,
		authzClient:        authzClient,
		nodeMgrClient:      nodeMgrClient,
		nodesClient:        nodesClient,
		db:                 db, // Initialize db
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
		return &response.ProcessNodeDeleteResponse{}, nil
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

func (s *ChefIngestServer) GetIndicesEligableForReindexing(ctx context.Context) (map[string]backend.IndexSettingsVersion, error) {
	var eligableIndices = make(map[string]backend.IndexSettingsVersion)
	indices, err := s.client.GetIndices(ctx)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "failed to fetch indices: %s", err)
	}

OuterLoop:
	for _, index := range indices {
		for prefix := range skipIndices {
			if strings.HasPrefix(index.Index, prefix) {
				log.WithFields(log.Fields{"index": index.Index}).Info("Skipping index")
				continue OuterLoop
			}
		}

		versionSettings, err := s.client.GetIndexVersionSettings(index.Index)
		if err != nil {
			return nil, status.Errorf(codes.Internal, "failed to fetch settings for index %s: %s", index.Index, err)
		}

		// Is reindexing needed?
		if versionSettings.Settings.Index.Version.CreatedString == versionSettings.Settings.Index.Version.UpgradedString {
			log.WithFields(log.Fields{"index": index.Index}).Info("Skipping index as it is already up to date")
			continue
		}

		eligableIndices[index.Index] = *versionSettings
	}

	return eligableIndices, nil
}

func (s *ChefIngestServer) StartReindex(ctx context.Context, req *ingest.StartReindexRequest) (*ingest.StartReindexResponse, error) {
	log.Info("Received request to start reindexing")

	indices, err := s.GetIndicesEligableForReindexing(ctx)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "failed to fetch indices: %s", err)
	}

	if len(indices) == 0 {
		log.Info("No indices found that need reindexing")
		return &ingest.StartReindexResponse{
			Message: "No indices found that need reindexing",
		}, nil
	}

	requestID, err := s.db.InsertReindexRequest("running", time.Now())
	if err != nil {
		return nil, status.Errorf(codes.Internal, "failed to add reindex request: %s", err)
	}

	for key, value := range indices {

		if err := s.db.InsertReindexRequestDetailed(storage.ReindexRequestDetailed{
			RequestID:   requestID,
			Index:       key,
			FromVersion: value.Settings.Index.Version.CreatedString,
			ToVersion:   value.Settings.Index.Version.UpgradedString,
			OsTaskID:    "",
			Heartbeat:   time.Now(),
			HavingAlias: false,
			AliasList:   "",
		}, time.Now()); err != nil {
			return nil, status.Errorf(codes.Internal, "failed to add reindex request: %s", err)
		}

		err = s.GetAliases(ctx, indices, requestID)
		if err != nil {
			return nil, status.Errorf(codes.Internal, "failed to get aliases for indices: %s", err)
		}

		err = s.client.DeleteIndexAndUpdateStatus(ctx, s.db, requestID, key)
		if err != nil {
			return nil, status.Errorf(codes.Internal, "failed to delete index %s: %s", key, err)
		}
	}

	log.Info("Reindexing started successfully")
	return &ingest.StartReindexResponse{
		Message: "Reindexing started successfully",
	}, nil
}

func (s *ChefIngestServer) GetReindexStatus(ctx context.Context, req *ingest.GetReindexStatusRequest) (*ingest.GetReindexStatusResponse, error) {
	log.WithFields(log.Fields{"func": "GetReindexStatus"}).Debug("RPC call received")
	if s.db == nil {
		errMsg := "database connection is not initialized"
		log.WithFields(log.Fields{"error": errMsg}).Error("DB error")
		return nil, status.Errorf(codes.Internal, "%s", errMsg)
	}
	var requestID int
	var err error
	// If RequestId is missing (0), fetch the latest request ID
	if req == nil || req.RequestId == 0 {
		log.Debug("RequestId is missing, fetching the latest request ID")

		requestID, err = s.db.GetLatestReindexRequestID()
		if err != nil {
			log.WithFields(log.Fields{"error": err.Error()}).Error("Failed to fetch latest reindex request ID")
			return nil, status.Errorf(codes.Internal, "failed to fetch latest reindex request ID: %v", err)
		}
		log.WithFields(log.Fields{"requestID": requestID}).Debug("Fetched latest request ID successfully")
	} else {
		requestID = int(req.RequestId)
	}

	// Fetch reindex status from the database
	statusResponse, err := s.db.GetReindexStatus(requestID)
	if err != nil {
		log.WithFields(log.Fields{"error": err.Error()}).Error("Failed to fetch reindex status")
		return nil, status.Errorf(codes.Internal, "failed to fetch reindex status: %v", err)
	}

	statusJSON, err := json.Marshal(statusResponse)
	if err != nil {
		log.WithFields(log.Fields{"error": err.Error()}).Error("Failed to marshal status response")
		return nil, status.Errorf(codes.Internal, "failed to marshal status response: %v", err)
	}

	log.WithFields(log.Fields{"status": string(statusJSON)}).Debug("Reindex status fetched successfully")

	return &ingest.GetReindexStatusResponse{
		StatusJson: string(statusJSON),
	}, nil
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

// Get aliases for indexes
func (s *ChefIngestServer) GetAliases(ctx context.Context, indexes map[string]backend.IndexSettingsVersion, requestID int) error {
	log.Info("Fetching aliases for indexes")
	for index := range indexes {
		log.Info("Fetching aliases for index: ", index)
		alias, hasAlias, err := s.client.GetAliases(ctx, index)
		if err != nil {
			log.Info("Failed to fetch aliases for index: ", index)
			return err
		}
		err = s.db.UpdateAliasesForIndex(index, hasAlias, alias, requestID, time.Now())
		if err != nil {
			log.Info("Failed to update aliases for index: ", index)
			return err
		}
	}
	return nil
}
