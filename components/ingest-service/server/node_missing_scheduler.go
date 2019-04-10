package server

// RPC functions for the job: 'missing_node'

import (
	"context"

	"github.com/chef/automate/api/interservice/ingest"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// MarkNodesMissing - run the mark nodes missing task now
func (server *JobSchedulerServer) MarkNodesMissing(ctx context.Context,
	empty *ingest.MarkNodesMissingRequest) (*ingest.MarkNodesMissingResponse, error) {
	jConfig := server.configManager.GetNodesMissingSchedulerConfig()
	nodesMissing, err := server.client.MarkNodesMissing(ctx, jConfig.Threshold)
	if err != nil {
		return &ingest.MarkNodesMissingResponse{}, status.Error(codes.Internal, err.Error())
	}

	log.WithFields(log.Fields{
		"nodes_updated": nodesMissing,
		"status":        "missing",
	}).Info("Marked nodes missing")
	return &ingest.MarkNodesMissingResponse{}, nil
}

// StartNodesMissingScheduler - start the scheduled task of deleting nodes
func (server *JobSchedulerServer) StartNodesMissingScheduler(ctx context.Context,
	empty *ingest.StartNodesMissingSchedulerRequest) (*ingest.StartNodesMissingSchedulerResponse, error) {
	log.Info("StartNodesMissingScheduler")

	jConfig := server.configManager.GetNodesMissingSchedulerConfig()
	if !jConfig.Running {
		jConfig.Running = true
		err := server.configManager.UpdateNodesMissingSchedulerConfig(jConfig)
		if err != nil {
			return &ingest.StartNodesMissingSchedulerResponse{}, status.Error(codes.Internal, err.Error())
		}

		server.jobScheduler.StartJob(jConfig.JobName())
	}

	return &ingest.StartNodesMissingSchedulerResponse{}, nil
}

// StopNodesMissingScheduler - stop the scheduled delete node task from running
func (server *JobSchedulerServer) StopNodesMissingScheduler(ctx context.Context,
	empty *ingest.StopNodesMissingSchedulerRequest) (*ingest.StopNodesMissingSchedulerResponse, error) {
	log.Info("StopNodesMissingScheduler")

	jConfig := server.configManager.GetNodesMissingSchedulerConfig()
	if jConfig.Running {
		jConfig.Running = false
		err := server.configManager.UpdateNodesMissingSchedulerConfig(jConfig)
		if err != nil {
			return &ingest.StopNodesMissingSchedulerResponse{}, status.Error(codes.Internal, err.Error())
		}

		server.jobScheduler.StopJob(jConfig.JobName())
	}

	return &ingest.StopNodesMissingSchedulerResponse{}, nil
}

// ConfigureNodesMissingScheduler rpc call to configure the NodesMissing Job
func (server *JobSchedulerServer) ConfigureNodesMissingScheduler(ctx context.Context,
	settings *ingest.JobSettings) (*ingest.ConfigureNodesMissingSchedulerResponse, error) {

	jConfig := server.configManager.GetNodesMissingSchedulerConfig()
	log.WithFields(log.Fields{
		"settings": settings.String(),
	}).Info("Incoming job")

	// apply job settings to the job config, then update the job if needed
	retrigger, err := jConfig.ApplyJobSettings(settings)
	if err != nil {
		return &ingest.ConfigureNodesMissingSchedulerResponse{}, status.Error(codes.InvalidArgument, err.Error())
	}

	if retrigger {
		err := server.configManager.UpdateNodesMissingSchedulerConfig(jConfig)
		if err != nil {
			return &ingest.ConfigureNodesMissingSchedulerResponse{}, status.Error(codes.Internal, err.Error())
		}

		server.addUpdateJob(jConfig)
	}

	return &ingest.ConfigureNodesMissingSchedulerResponse{}, nil
}
