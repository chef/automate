package server

// RPC functions for the job: 'delete_node'

import (
	"context"

	"github.com/chef/automate/api/interservice/ingest"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	log "github.com/sirupsen/logrus"
)

// DeleteMarkedNodes - run the delete node task now
func (server *JobSchedulerServer) DeleteMarkedNodes(ctx context.Context,
	empty *ingest.DeleteMarkedNodesRequest) (*ingest.DeleteMarkedNodesResponse, error) {
	jConfig := server.configManager.GetDeleteNodesSchedulerConfig()
	deletedNodes, err := server.client.DeleteMarkedNodes(ctx, jConfig.Threshold)
	if err != nil {
		return &ingest.DeleteMarkedNodesResponse{}, status.Error(codes.Internal, err.Error())
	}

	log.WithFields(log.Fields{
		"nodes_deleted": deletedNodes,
	}).Info("Nodes deleted")
	return &ingest.DeleteMarkedNodesResponse{}, nil
}

// StartDeleteNodesScheduler - start the scheduled task of deleting nodes
func (server *JobSchedulerServer) StartDeleteNodesScheduler(ctx context.Context,
	empty *ingest.StartDeleteNodesSchedulerRequest) (*ingest.StartDeleteNodesSchedulerResponse, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	jConfig := server.configManager.GetDeleteNodesSchedulerConfig()
	if !jConfig.Running {
		jConfig.Running = true
		err := server.configManager.UpdateDeleteNodesSchedulerConfig(jConfig)
		if err != nil {
			return &ingest.StartDeleteNodesSchedulerResponse{}, status.Error(codes.Internal, err.Error())
		}

		server.jobScheduler.StartJob(jConfig.JobName())
	}

	return &ingest.StartDeleteNodesSchedulerResponse{}, nil
}

// StopDeleteNodesScheduler - stop the scheduled delete node task from running
func (server *JobSchedulerServer) StopDeleteNodesScheduler(ctx context.Context,
	empty *ingest.StopDeleteNodesSchedulerRequest) (*ingest.StopDeleteNodesSchedulerResponse, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	jConfig := server.configManager.GetDeleteNodesSchedulerConfig()
	if jConfig.Running {
		jConfig.Running = false
		err := server.configManager.UpdateDeleteNodesSchedulerConfig(jConfig)
		if err != nil {
			return &ingest.StopDeleteNodesSchedulerResponse{}, status.Error(codes.Internal, err.Error())
		}

		server.jobScheduler.StopJob(jConfig.JobName())
	}

	return &ingest.StopDeleteNodesSchedulerResponse{}, nil
}

// ConfigureDeleteNodesScheduler changed the configurations for the delete node scheduler
func (server *JobSchedulerServer) ConfigureDeleteNodesScheduler(ctx context.Context,
	settings *ingest.JobSettings) (*ingest.ConfigureDeleteNodesSchedulerResponse, error) {

	jConfig := server.configManager.GetDeleteNodesSchedulerConfig()
	log.WithFields(log.Fields{
		"Freq":      settings.Every,
		"Running":   settings.Running,
		"Threshold": settings.Threshold,
	}).Info("ConfigureDeleteNodesScheduler")

	// apply job settings to the job config, then update the job if needed
	retrigger, err := jConfig.ApplyJobSettings(settings)
	if err != nil {
		return &ingest.ConfigureDeleteNodesSchedulerResponse{}, status.Error(codes.InvalidArgument, err.Error())
	}

	if retrigger {
		err := server.configManager.UpdateDeleteNodesSchedulerConfig(jConfig)
		if err != nil {
			return &ingest.ConfigureDeleteNodesSchedulerResponse{}, status.Error(codes.Internal, err.Error())
		}

		server.addUpdateJob(jConfig)
	}

	return &ingest.ConfigureDeleteNodesSchedulerResponse{}, nil
}
