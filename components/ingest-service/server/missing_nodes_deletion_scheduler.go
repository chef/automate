package server

// RPC functions for the job: 'missing_nodes_for_deletion'

import (
	"context"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/lib/cereal"
)

// MarkMissingNodesForDeletion - run the mark missing nodes for deletion task now
func (server *JobSchedulerServer) MarkMissingNodesForDeletion(ctx context.Context,
	empty *ingest.MarkMissingNodesForDeletionRequest) (*ingest.MarkMissingNodesForDeletionResponse, error) {

	sched, err := server.jobManager.GetWorkflowScheduleByName(ctx,
		MissingNodesForDeletionScheduleName, MissingNodesForDeletionJobName)
	if err != nil {
		return &ingest.MarkMissingNodesForDeletionResponse{}, status.Error(codes.Internal, err.Error())
	}

	var threshold string
	if err := sched.GetParameters(&threshold); err != nil {
		return &ingest.MarkMissingNodesForDeletionResponse{}, status.Error(codes.Internal, err.Error())
	}

	nodes4Deletion, err := server.client.MarkMissingNodesForDeletion(ctx, threshold)
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

	err := server.jobManager.UpdateWorkflowScheduleByName(
		ctx, MissingNodesForDeletionScheduleName, MissingNodesForDeletionJobName,
		cereal.UpdateEnabled(true))

	if err != nil {
		return &ingest.StartMissingNodesForDeletionSchedulerResponse{}, status.Error(codes.Internal, err.Error())
	}
	return &ingest.StartMissingNodesForDeletionSchedulerResponse{}, nil
}

// StopMissingNodesForDeletionScheduler - stop the scheduled marking nodes for deletion task from running
func (server *JobSchedulerServer) StopMissingNodesForDeletionScheduler(ctx context.Context,
	empty *ingest.StopMissingNodesForDeletionSchedulerRequest) (*ingest.StopMissingNodesForDeletionSchedulerResponse, error) {
	log.Info("StopMissingNodesForDeletionScheduler")

	err := server.jobManager.UpdateWorkflowScheduleByName(
		ctx, MissingNodesForDeletionScheduleName, MissingNodesForDeletionJobName,
		cereal.UpdateEnabled(false))
	if err != nil {
		return &ingest.StopMissingNodesForDeletionSchedulerResponse{}, status.Error(codes.Internal, err.Error())
	}
	return &ingest.StopMissingNodesForDeletionSchedulerResponse{}, nil
}

// ConfigureMissingNodesForDeletionScheduler rpc call to configure the MissingNodesForDeletion Job
func (server *JobSchedulerServer) ConfigureMissingNodesForDeletionScheduler(ctx context.Context,
	settings *ingest.JobSettings) (*ingest.ConfigureMissingNodesForDeletionSchedulerResponse, error) {
	log.WithFields(log.Fields{
		"settings": settings.String(),
	}).Info("Incoming job")

	oldSchedule, err := server.jobManager.GetWorkflowScheduleByName(
		ctx, MissingNodesForDeletionScheduleName, MissingNodesForDeletionJobName)
	if err != nil {
		return &ingest.ConfigureMissingNodesForDeletionSchedulerResponse{}, status.Error(codes.Internal, err.Error())
	}

	updateOpts, shouldRunNow, err := JobSettingsToUpdateOpts(settings, oldSchedule)
	if err != nil {
		return &ingest.ConfigureMissingNodesForDeletionSchedulerResponse{}, status.Error(codes.InvalidArgument, err.Error())
	}

	err = server.jobManager.UpdateWorkflowScheduleByName(
		ctx, MissingNodesForDeletionScheduleName, MissingNodesForDeletionJobName, updateOpts...)
	if err != nil {
		return &ingest.ConfigureMissingNodesForDeletionSchedulerResponse{}, status.Error(codes.Internal, err.Error())
	}
	if shouldRunNow {
		err = server.runJobNow(ctx, MissingNodesForDeletionJobName)
		if err != nil {
			return &ingest.ConfigureMissingNodesForDeletionSchedulerResponse{}, status.Error(codes.Internal, err.Error())
		}
	}

	return &ingest.ConfigureMissingNodesForDeletionSchedulerResponse{}, nil
}
