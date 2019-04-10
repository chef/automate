package server

// RPC functions for the job: 'missing_nodes_for_deletion'

import (
	"context"

	"github.com/chef/automate/api/interservice/ingest"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// MarkMissingNodesForDeletion - run the mark missing nodes for deletion task now
func (server *JobSchedulerServer) MarkMissingNodesForDeletion(ctx context.Context,
	empty *ingest.MarkMissingNodesForDeletionRequest) (*ingest.MarkMissingNodesForDeletionResponse, error) {
	jConfig := server.configManager.GetMissingNodesForDeletionSchedulerConfig()
	nodes4Deletion, err := server.client.MarkMissingNodesForDeletion(ctx, jConfig.Threshold)
	if err != nil {
		return &ingest.MarkMissingNodesForDeletionResponse{}, status.Error(codes.Internal, err.Error())
	}

	log.WithFields(log.Fields{
		"nodes_updated": nodes4Deletion,
		"exists":        false,
	}).Info("Node(s) marked for deletion")
	return &ingest.MarkMissingNodesForDeletionResponse{}, nil
}

// StartMissingNodesForDeletionScheduler - start the scheduled task of marking nodes for deletion
func (server *JobSchedulerServer) StartMissingNodesForDeletionScheduler(ctx context.Context,
	empty *ingest.StartMissingNodesForDeletionSchedulerRequest) (*ingest.StartMissingNodesForDeletionSchedulerResponse, error) {
	log.Info("StartMissingNodesForDeletionScheduler")

	jConfig := server.configManager.GetMissingNodesForDeletionSchedulerConfig()
	if !jConfig.Running {
		jConfig.Running = true
		err := server.configManager.UpdateMissingNodesForDeletionSchedulerConfig(jConfig)
		if err != nil {
			return &ingest.StartMissingNodesForDeletionSchedulerResponse{}, status.Error(codes.Internal, err.Error())
		}

		server.jobScheduler.StartJob(jConfig.JobName())
	}

	return &ingest.StartMissingNodesForDeletionSchedulerResponse{}, nil
}

// StopMissingNodesForDeletionScheduler - stop the scheduled marking nodes for deletion task from running
func (server *JobSchedulerServer) StopMissingNodesForDeletionScheduler(ctx context.Context,
	empty *ingest.StopMissingNodesForDeletionSchedulerRequest) (*ingest.StopMissingNodesForDeletionSchedulerResponse, error) {
	log.Info("StopMissingNodesForDeletionScheduler")

	jConfig := server.configManager.GetMissingNodesForDeletionSchedulerConfig()
	if jConfig.Running {
		jConfig.Running = false
		err := server.configManager.UpdateMissingNodesForDeletionSchedulerConfig(jConfig)
		if err != nil {
			return &ingest.StopMissingNodesForDeletionSchedulerResponse{}, status.Error(codes.Internal, err.Error())
		}

		server.jobScheduler.StopJob(jConfig.JobName())
	}

	return &ingest.StopMissingNodesForDeletionSchedulerResponse{}, nil
}

// ConfigureMissingNodesForDeletionScheduler rpc call to configure the MissingNodesForDeletion Job
func (server *JobSchedulerServer) ConfigureMissingNodesForDeletionScheduler(ctx context.Context,
	settings *ingest.JobSettings) (*ingest.ConfigureMissingNodesForDeletionSchedulerResponse, error) {

	jConfig := server.configManager.GetMissingNodesForDeletionSchedulerConfig()
	log.WithFields(log.Fields{
		"settings": settings.String(),
	}).Info("Incoming job")

	// apply job settings to the job config, then update the job if needed
	retrigger, err := jConfig.ApplyJobSettings(settings)
	if err != nil {
		return &ingest.ConfigureMissingNodesForDeletionSchedulerResponse{}, status.Error(codes.InvalidArgument, err.Error())
	}

	if retrigger {
		err := server.configManager.UpdateMissingNodesForDeletionSchedulerConfig(jConfig)
		if err != nil {
			return &ingest.ConfigureMissingNodesForDeletionSchedulerResponse{}, status.Error(codes.Internal, err.Error())
		}

		server.addUpdateJob(jConfig)
	}

	return &ingest.ConfigureMissingNodesForDeletionSchedulerResponse{}, nil
}
