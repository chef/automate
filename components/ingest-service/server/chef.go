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
	".tasks":                    true,
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

		isEligible, versionSettings, err := s.VersionComparision(index.Index)
		if err != nil {
			return nil, status.Errorf(codes.Internal, "failed to fetch settings for index %s: %s", index.Index, err)
		}

		if !isEligible {
			continue
		}

		eligableIndices[index.Index] = *versionSettings
	}

	return eligableIndices, nil
}

func (s *ChefIngestServer) VersionComparision(index string) (bool, *backend.IndexSettingsVersion, error) {
	versionSettings, err := s.client.GetIndexVersionSettings(index)
	if err != nil {
		return false, nil, status.Errorf(codes.Internal, "failed to fetch settings for index %s: %s", index, err)
	}

	// Is reindexing needed?
	if versionSettings.Settings.Index.Version.CreatedString == versionSettings.Settings.Index.Version.UpgradedString {
		return false, nil, nil
	}

	return true, versionSettings, nil
}

func (s *ChefIngestServer) StartReindex(ctx context.Context, req *ingest.StartReindexRequest) (*ingest.StartReindexResponse, error) {
	log.Info("Received request to start reindexing")

	// check if reindexing is already running
	//  ***** //

	// Fetch indices that need reindexing
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

	indexList := make([]string, 0, len(indices))
	for index := range indices {
		indexList = append(indexList, index)
	}

	// Add the request to the database with status 'running'
	requestID, err := s.db.InsertReindexRequest("running", time.Now())
	if err != nil {
		return nil, status.Errorf(codes.Internal, "failed to add reindex request: %s", err)
	}

	// Add the indices to reindex request detailed table
	for key, value := range indices {
		if err := s.db.InsertReindexRequestDetailed(storage.ReindexRequestDetailed{
			RequestID:   requestID,
			Index:       key,
			FromVersion: value.Settings.Index.Version.CreatedString,
			ToVersion:   value.Settings.Index.Version.UpgradedString,
			Stage:       []storage.StageDetail{},
			OsTaskID:    "",
			Heartbeat:   time.Now(),
			HavingAlias: false,
			AliasList:   "",
		}, time.Now()); err != nil {
			return nil, status.Errorf(codes.Internal, "failed to add reindex request: %s", err)
		}
	}

	backgroundCtx, cancel := context.WithTimeout(context.Background(), 24*time.Hour)

	errChan := make(chan error, 1)

	// Start the reindexing process in a background goroutine
	go func() {
		defer cancel() // Ensure context is canceled when goroutine completes

		// Run the reindexing process with the detached context
		err := s.runReindexingProcess(backgroundCtx, indexList, requestID, errChan)
		if err != nil {
			log.WithFields(log.Fields{"requestId": requestID}).WithError(err).Error("Reindexing process failed")
			errChan <- err
		}
	}()

	return &ingest.StartReindexResponse{
		Message: "Reindexing started successfully",
	}, nil
}

func (s *ChefIngestServer) runReindexingProcess(ctx context.Context, indexList []string, requestID int, errChan chan error) error {
	// Start a goroutine for heartbeat updates
	heartbeatDone := make(chan struct{})
	defer close(heartbeatDone)

	// Create a ticker for heartbeat updates - every 2 minutes
	go func() {
		ticker := time.NewTicker(2 * time.Minute)
		defer ticker.Stop()

		for {
			select {
			case <-ctx.Done():
				log.WithFields(log.Fields{"requestId": requestID}).Info("Heartbeat stopped due to context cancellation")
				return
			case <-heartbeatDone:
				log.WithFields(log.Fields{"requestId": requestID}).Info("Heartbeat stopped as process completed")
				return
			case <-ticker.C:
				// Log heartbeat for all indices in this request
				for _, index := range indexList {
					log.WithFields(log.Fields{
						"requestId": requestID,
						"index":     index,
					}).Info("Heartbeat tick")
				}
			}
		}
	}()

	var isFailed bool

	// Loop through the indices
	for _, index := range indexList {
		// Check if context is canceled
		select {
		case <-ctx.Done():
			log.WithFields(log.Fields{"requestId": requestID}).Info("Reindexing process canceled")
			return ctx.Err()
		default:
			// 1. Check if the index needs reindexing
			isEligible, _, _ := s.VersionComparision(index)
			if !isEligible {
				log.WithFields(log.Fields{"index": index}).Info("Index does not need reindexing")
				continue
			}

			// 2. Get the aliases and update the database
			alias, err := s.getAliases(ctx, index, requestID)
			if err != nil {
				isFailed = true
				log.WithFields(log.Fields{"index": index}).WithError(err).Error("Failed to fetch aliases for index: ", index)
				continue
			}
			tempIndex := index + "_temp"

			// Step 3: Create Temporary Index
			if err := s.createIndex(ctx, tempIndex, index, requestID, SRC_TO_TEMP, index); err != nil {
				isFailed = true
				continue
			}

			// Step 4: Reindex from Source to Temporary Index
			if err := s.processReindexing(ctx, tempIndex, index, requestID, REINDEX_SRC_TEMP, index); err != nil {
				isFailed = true
				log.WithFields(log.Fields{"index": index}).WithError(err).Error("Failed to reindex from source to temporary index: ", index)
				continue
			}

			// Step 6: Delete Source Index
			if err := s.deleteIndex(ctx, index, requestID, index, DELETE_SRC); err != nil {
				isFailed = true
				log.WithFields(log.Fields{"index": index}).WithError(err).Error("Failed to delete source index: ", index)
				continue
			}

			// Step 7: Recreate Source Index from Temporary Index
			if err := s.createIndex(ctx, index, tempIndex, requestID, TEMP_TO_SRC, index); err != nil {
				isFailed = true
				log.WithFields(log.Fields{"index": index}).WithError(err).Error("Failed to recreate source index from temporary index: ", index)
				continue
			}

			// Step 8: Reindex from Temporary to Source Index
			if err := s.processReindexing(ctx, index, tempIndex, requestID, REINDEX_TEMP_SRC, index); err != nil {
				isFailed = true
				log.WithFields(log.Fields{"index": index}).WithError(err).Error("Failed to reindex from temporary to source index: ", index)
				continue
			}

			// Step 10: Create Aliases for Source Index
			if err := s.createAliases(ctx, index, alias, requestID); err != nil {
				isFailed = true
				log.WithFields(log.Fields{"index": index}).WithError(err).Error("Failed to create aliases for source index: ", index)
				continue
			}

			// Step 11: Delete Temporary Index
			if err := s.deleteIndex(ctx, tempIndex, requestID, index, DELETE_TEMP); err != nil {
				isFailed = true
				log.WithFields(log.Fields{"index": index}).WithError(err).Error("Failed to delete temporary index: ", tempIndex)
				continue
			}

			log.WithFields(log.Fields{"index": index}).Info("Reindexing completed for index")
		}
	}

	if isFailed {
		finalStatus := STATUS_FAILED
		errChan <- status.Errorf(codes.Internal, "failed to reindex indices")
		if err := s.db.UpdateReindexRequest(requestID, finalStatus, time.Now()); err != nil {
			log.WithFields(log.Fields{"requestId": requestID}).WithError(err).Error("Failed to update overall status of the request")
		}
		return status.Errorf(codes.Internal, "failed to reindex indices")
	}

	finalStatus := STATUS_COMPLETED
	if err := s.db.UpdateReindexRequest(requestID, finalStatus, time.Now()); err != nil {
		log.WithFields(log.Fields{"requestId": requestID}).WithError(err).Error("Failed to update overall status of the request")
		return status.Errorf(codes.Internal, "failed to update overall status of the request")
	}

	log.WithFields(log.Fields{"requestId": requestID}).Info("Reindexing process completed successfully")
	return nil
}

func (s *ChefIngestServer) createIndex(ctx context.Context, targetIndex, sourceIndex string, requestID int, stage string, originalIndex string) error {
	log.WithFields(log.Fields{"targetIndex": targetIndex, "sourceIndex": sourceIndex}).Info("Creating index")

	err := s.db.CreateOrUpdateStageAndStatusForIndex(requestID, originalIndex, stage, STATUS_RUNNING, time.Now())
	if err != nil {
		log.Errorf("Failed to update the status for stage %s with status %s with error %v", stage, STATUS_RUNNING, err)
		return err
	}
	err = s.client.CreateIndex(targetIndex, sourceIndex)
	if err != nil {
		log.WithError(err).Errorf("Failed to create index: %s", targetIndex)
		err = s.db.CreateOrUpdateStageAndStatusForIndex(requestID, originalIndex, stage, STATUS_FAILED, time.Now())
		if err != nil {
			log.Errorf("Failed to update the status for stage %s with status %s with error %v", stage, STATUS_FAILED, err)
		}
		return status.Errorf(codes.Internal, "failed to create index %s: %s", targetIndex, err)
	}
	err = s.db.CreateOrUpdateStageAndStatusForIndex(requestID, originalIndex, stage, STATUS_COMPLETED, time.Now())
	if err != nil {
		log.Errorf("Failed to update the status for stage %s with status %s with error %v", stage, STATUS_COMPLETED, err)
		return err
	}

	return nil
}

func (s *ChefIngestServer) createAliases(ctx context.Context, srcIndex string, aliases []string, requestID int) error {
	log.WithFields(log.Fields{"srcIndex": srcIndex, "aliases": aliases}).Info("Creating/updating aliases for source index")

	// Update the status to indicate the alias creation process has started
	err := s.db.CreateOrUpdateStageAndStatusForIndex(requestID, srcIndex, CREATE_ALIASES, STATUS_RUNNING, time.Now())
	if err != nil {
		log.Errorf("Failed to update the status for stage %s with status %s with error %v", CREATE_ALIASES, STATUS_RUNNING, err)
		return err
	}

	// If no aliases are provided, log and return
	if len(aliases) == 0 {
		log.WithFields(log.Fields{"srcIndex": srcIndex}).Info("No aliases found for source index")
		return nil
	}

	// Iterate over the aliases and create/update them in OpenSearch
	for _, aliasName := range aliases {
		log.WithFields(log.Fields{"srcIndex": srcIndex, "alias": aliasName}).Info("Creating alias for source index")
		err := s.client.CreateAlias(ctx, aliasName, srcIndex)
		if err != nil {
			log.WithError(err).Errorf("Failed to create alias %s for index %s", aliasName, srcIndex)

			// Update the database with the failure status
			dbErr := s.db.CreateOrUpdateStageAndStatusForIndex(requestID, srcIndex, CREATE_ALIASES, STATUS_FAILED, time.Now())
			if dbErr != nil {
				log.Errorf("Failed to update the status for stage %s with status %s with error %v", CREATE_ALIASES, STATUS_FAILED, dbErr)
			}
			return err
		}
	}

	// Update the status to indicate the alias creation process has completed
	err = s.db.CreateOrUpdateStageAndStatusForIndex(requestID, srcIndex, CREATE_ALIASES, STATUS_COMPLETED, time.Now())
	if err != nil {
		log.Errorf("Failed to update the status for stage %s with status %s with error %v", CREATE_ALIASES, STATUS_COMPLETED, err)
		return err
	}
	return nil
}

func (s *ChefIngestServer) deleteIndex(ctx context.Context, index string, requestID int, originalIndex, stage string) error {

	log.WithFields(log.Fields{"index": index}).Info("Deleting index")

	err := s.db.CreateOrUpdateStageAndStatusForIndex(requestID, originalIndex, stage, STATUS_RUNNING, time.Now())
	if err != nil {
		log.Errorf("Failed to update the status for stage %s with status %s with error %v", stage, STATUS_RUNNING, err)
		return err
	}

	err = s.client.DeleteIndex(ctx, index)
	if err != nil {
		log.WithError(err).Errorf("Failed to delete index %s", index)
		err = s.db.CreateOrUpdateStageAndStatusForIndex(requestID, originalIndex, stage, STATUS_FAILED, time.Now())
		if err != nil {
			log.Errorf("Failed to update the status for stage %s with status %s with error %v", stage, STATUS_FAILED, err)
		}
		return err
	}
	err = s.db.CreateOrUpdateStageAndStatusForIndex(requestID, originalIndex, stage, STATUS_COMPLETED, time.Now())
	if err != nil {
		log.Errorf("Failed to update the status for stage %s with status %s with error %v", stage, STATUS_COMPLETED, err)
		return err
	}
	return nil
}

func (s *ChefIngestServer) processReindexing(ctx context.Context, destIndex, srcIndex string, requestID int, stage, originalIndex string) error {
	log.WithFields(log.Fields{"destIndex": destIndex, "srcIndex": srcIndex}).Info("Reindexing from source to destination index")

	err := s.db.CreateOrUpdateStageAndStatusForIndex(requestID, originalIndex, stage, STATUS_RUNNING, time.Now())
	if err != nil {
		return err
	}

	taskID, err := s.client.ReindexIndices(ctx, srcIndex, destIndex)
	if err != nil {
		log.WithError(err).Errorf("Failed to start reindexing for index %s", srcIndex)
		err = s.db.CreateOrUpdateStageAndStatusForIndex(requestID, originalIndex, stage, STATUS_FAILED, time.Now())
		if err != nil {
			log.Errorf("Failed to update the status for stage %s with status %s with error %v", stage, STATUS_FAILED, err)
		}
		return status.Errorf(codes.Internal, "failed to start reindexing for index %s: %s", srcIndex, err)
	}
	if err := s.db.UpdateTaskIDForReindexRequest(requestID, originalIndex, taskID, time.Now()); err != nil {
		log.WithError(err).Errorf("Failed to update task ID for index %s", srcIndex)
		err = s.db.CreateOrUpdateStageAndStatusForIndex(requestID, originalIndex, stage, STATUS_FAILED, time.Now())
		if err != nil {
			log.Errorf("Failed to update the status for stage %s with status %s with error %v", stage, STATUS_FAILED, err)
		}
		return status.Errorf(codes.Internal, "failed to update task ID for index %s: %s", srcIndex, err)
	} else {
		log.WithFields(log.Fields{
			"srcIndex": srcIndex,
			"taskID":   taskID,
		}).Info("Task ID updated in the database")
	}

	// Wait for the reindexing to complete
	taskCompleted := s.monitorReindexing(ctx, requestID, srcIndex, taskID) // Ensure monitoring is finished

	// Only update status to COMPLETED if the task was successful
	if taskCompleted {
		err = s.db.CreateOrUpdateStageAndStatusForIndex(requestID, originalIndex, stage, STATUS_COMPLETED, time.Now())
		if err != nil {
			log.Errorf("Failed to update status for stage %s to COMPLETED: %v", stage, err)
			return err
		}
	} else {
		log.Warnf("Reindexing task %s for index %s did not complete successfully. Updating status to FAILED.", taskID, srcIndex)
		if err := s.db.CreateOrUpdateStageAndStatusForIndex(requestID, originalIndex, stage, STATUS_FAILED, time.Now()); err != nil {
			log.Errorf("Failed to update status for stage %s to FAILED: %v", stage, err)
		}
	}
	return nil
}

func (s *ChefIngestServer) monitorReindexing(ctx context.Context, requestID int, index, taskID string) bool {
	// Set a timeout for how long to wait for the task to complete
	timeout := time.Now().Add(6 * time.Hour)

	for {
		// Check if we've exceeded the timeout
		if time.Now().After(timeout) {
			log.Warnf("Task %s for index %s timed out.", taskID, index)
			return false // Task timed out
		}

		// Check the task status
		jobStatus, err := s.client.JobStatus(ctx, taskID)
		if err != nil {
			log.WithError(err).Errorf("Error checking status for task %s", taskID)
		}

		// Update heartbeat only if the task is still running
		if !jobStatus.Completed {
			if err := s.db.UpdateReindexStatus(requestID, index, "heartbeat", time.Now()); err != nil {
				log.WithError(err).Errorf("Failed to update heartbeat for index %s", index)
			}
		}

		// If the job is completed, exit the loop
		if jobStatus.Completed {
			log.Infof("Reindexing task %s for index %s completed successfully.", taskID, index)
			return true // Task completed successfully
		}

		// Wait for 15 seconds before checking again
		time.Sleep(15 * time.Second)
	}
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

// GetAliases fetches the aliases for index and update the database
func (s *ChefIngestServer) getAliases(ctx context.Context, index string, requestID int) ([]string, error) {
	log.Info("Fetching aliases for index: ", index)
	err := s.db.CreateOrUpdateStageAndStatusForIndex(requestID, index, GET_ALIASES, STATUS_RUNNING, time.Now())
	if err != nil {
		log.Errorf("Failed to update the status for stage %s with status %s with error %v", GET_ALIASES, STATUS_RUNNING, err)
		return nil, err
	}
	alias, hasAlias, err := s.client.GetAliases(ctx, index)
	if err != nil {
		log.Errorf("Failed to fetch aliases for index: %s for error %v ", index, err)
		err := s.db.CreateOrUpdateStageAndStatusForIndex(requestID, index, GET_ALIASES, STATUS_FAILED, time.Now())
		if err != nil {
			log.Errorf("Failed to update the status for stage %s with status %s with error %v", GET_ALIASES, STATUS_FAILED, err)
		}

		return nil, err
	}
	err = s.db.UpdateAliasesForIndex(index, hasAlias, alias, requestID, time.Now())
	if err != nil {
		log.Errorf("Failed to fetch aliases for index: %s for error %v ", index, err)
		err := s.db.CreateOrUpdateStageAndStatusForIndex(requestID, index, GET_ALIASES, STATUS_FAILED, time.Now())
		if err != nil {
			log.Errorf("Failed to update the status for stage %s with status %s with error %v", GET_ALIASES, STATUS_FAILED, err)
		}
		return nil, err
	}

	err = s.db.CreateOrUpdateStageAndStatusForIndex(requestID, index, GET_ALIASES, STATUS_COMPLETED, time.Now())
	if err != nil {
		log.Errorf("Failed to update the status for stage %s with status %s with error %v", GET_ALIASES, STATUS_COMPLETED, err)
		return nil, err
	}

	return alias, nil
}

func (s *ChefIngestServer) GetEligilbeIndexes(ctx context.Context, re *ingest.GetEligilbeIndexesRequest) (*ingest.GetEligilbeIndexesResponse, error) {

	indices, err := s.GetIndicesEligableForReindexing(ctx)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "failed to fetch indices: %s", err)
	}

	if len(indices) == 0 {
		log.Info("No indices found that need reindexing")
		return &ingest.GetEligilbeIndexesResponse{
			Indexes: []string{},
		}, nil
	}

	indexList := make([]string, 0, len(indices))
	for index := range indices {
		indexList = append(indexList, index)
	}

	return &ingest.GetEligilbeIndexesResponse{
		Indexes: indexList,
	}, nil
}
