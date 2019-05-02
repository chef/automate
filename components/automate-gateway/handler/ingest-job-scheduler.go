package handler

import (
	"context"

	log "github.com/sirupsen/logrus"

	req "github.com/chef/automate/api/external/ingest/request"
	res "github.com/chef/automate/api/external/ingest/response"
	"github.com/chef/automate/api/interservice/ingest"
)

type ChefIngestJobSchedulerServer struct {
	jobSchedulerClient ingest.JobSchedulerClient
}

func NewChefIngestJobSchedulerServer(jobSchedulerClient ingest.JobSchedulerClient) *ChefIngestJobSchedulerServer {
	return &ChefIngestJobSchedulerServer{jobSchedulerClient: jobSchedulerClient}
}

// GetStatusJobScheduler - collect and return the status of all the jobs in the Job Scheduler
func (server *ChefIngestJobSchedulerServer) GetStatusJobScheduler(ctx context.Context, empty *req.GetStatusJobScheduler) (*res.JobSchedulerStatus, error) {
	log.WithFields(log.Fields{
		"func": nameOfFunc(),
	}).Debug("rpc call")
	response, err := server.jobSchedulerClient.GetStatusJobScheduler(ctx, &ingest.JobSchedulerStatusRequest{})
	if err != nil {
		return &res.JobSchedulerStatus{}, err
	}
	return &res.JobSchedulerStatus{
		Running: response.Running,
		Jobs:    toGwJobs(response.Jobs)}, nil
}

// MarkNodesMissing - run the mark nodes missing task now
func (server *ChefIngestJobSchedulerServer) MarkNodesMissing(ctx context.Context, empty *req.MarkNodesMissing) (*res.MarkNodesMissing, error) {
	log.WithFields(log.Fields{
		"func": nameOfFunc(),
	}).Debug("rpc call")
	_, err := server.jobSchedulerClient.MarkNodesMissing(ctx, &ingest.MarkNodesMissingRequest{})

	return &res.MarkNodesMissing{}, err
}

// StartNodesMissingScheduler - start the scheduled task of deleting nodes
func (server *ChefIngestJobSchedulerServer) StartNodesMissingScheduler(ctx context.Context, empty *req.StartNodesMissingScheduler) (*res.StartNodesMissingScheduler, error) {
	log.WithFields(log.Fields{
		"func": nameOfFunc(),
	}).Debug("rpc call")
	_, err := server.jobSchedulerClient.StartNodesMissingScheduler(ctx, &ingest.StartNodesMissingSchedulerRequest{})

	return &res.StartNodesMissingScheduler{}, err
}

// StopNodesMissingScheduler - stop the scheduled delete node task from running
func (server *ChefIngestJobSchedulerServer) StopNodesMissingScheduler(ctx context.Context, empty *req.StopNodesMissingScheduler) (*res.StopNodesMissingScheduler, error) {
	log.WithFields(log.Fields{
		"func": nameOfFunc(),
	}).Debug("rpc call")
	_, err := server.jobSchedulerClient.StopNodesMissingScheduler(ctx, &ingest.StopNodesMissingSchedulerRequest{})

	return &res.StopNodesMissingScheduler{}, err
}

// ConfigureNodesMissingScheduler rpc call to configure the NodesMissing Job
func (server *ChefIngestJobSchedulerServer) ConfigureNodesMissingScheduler(ctx context.Context, parameters *req.SchedulerConfig) (*res.ConfigureNodesMissingScheduler, error) {
	log.WithFields(log.Fields{
		"request": parameters.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	config := &ingest.JobSettings{
		Every:     parameters.GetEvery(),
		Threshold: parameters.GetThreshold(),
		Running:   parameters.GetRunning(),
	}
	_, err := server.jobSchedulerClient.ConfigureNodesMissingScheduler(ctx, config)

	return &res.ConfigureNodesMissingScheduler{}, err
}

// DeleteMarkedNodes - run the delete node task now
func (server *ChefIngestJobSchedulerServer) DeleteMarkedNodes(ctx context.Context, empty *req.DeleteMarkedNodes) (*res.DeleteMarkedNodes, error) {
	log.WithFields(log.Fields{
		"func": nameOfFunc(),
	}).Debug("rpc call")
	_, err := server.jobSchedulerClient.DeleteMarkedNodes(ctx, &ingest.DeleteMarkedNodesRequest{})

	return &res.DeleteMarkedNodes{}, err
}

// StartDeleteNodesScheduler - start the scheduled task of deleting nodes
func (server *ChefIngestJobSchedulerServer) StartDeleteNodesScheduler(ctx context.Context, empty *req.StartDeleteNodesScheduler) (*res.StartDeleteNodesScheduler, error) {
	log.WithFields(log.Fields{
		"func": nameOfFunc(),
	}).Debug("rpc call")
	_, err := server.jobSchedulerClient.StartDeleteNodesScheduler(ctx, &ingest.StartDeleteNodesSchedulerRequest{})

	return &res.StartDeleteNodesScheduler{}, err
}

// StopDeleteNodesScheduler - stop the scheduled delete node task from running
func (server *ChefIngestJobSchedulerServer) StopDeleteNodesScheduler(ctx context.Context, empty *req.StopDeleteNodesScheduler) (*res.StopDeleteNodesScheduler, error) {
	log.WithFields(log.Fields{
		"func": nameOfFunc(),
	}).Debug("rpc call")
	_, err := server.jobSchedulerClient.StopDeleteNodesScheduler(ctx, &ingest.StopDeleteNodesSchedulerRequest{})

	return &res.StopDeleteNodesScheduler{}, err
}

// ConfigureDeleteNodesScheduler changed the configurations for the delete node scheduler
func (server *ChefIngestJobSchedulerServer) ConfigureDeleteNodesScheduler(ctx context.Context,
	config *req.SchedulerConfig) (*res.ConfigureDeleteNodesScheduler, error) {
	log.WithFields(log.Fields{
		"config": config.String(),
		"func":   nameOfFunc(),
	}).Debug("rpc call")

	parameters := &ingest.JobSettings{
		Every:     config.GetEvery(),
		Threshold: config.GetThreshold(),
		Running:   config.GetRunning(),
	}
	_, err := server.jobSchedulerClient.ConfigureDeleteNodesScheduler(ctx, parameters)

	return &res.ConfigureDeleteNodesScheduler{}, err
}

// MarkMissingNodesForDeletion - run the mark missing nodes for deletion task now
func (server *ChefIngestJobSchedulerServer) MarkMissingNodesForDeletion(ctx context.Context, empty *req.MarkMissingNodesForDeletion) (*res.MarkMissingNodesForDeletion, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	_, err := server.jobSchedulerClient.MarkMissingNodesForDeletion(ctx, &ingest.MarkMissingNodesForDeletionRequest{})
	return &res.MarkMissingNodesForDeletion{}, err
}

// StartMissingNodesForDeletionScheduler - start the scheduled task mark missing nodes for deletion
func (server *ChefIngestJobSchedulerServer) StartMissingNodesForDeletionScheduler(ctx context.Context, empty *req.StartMissingNodesForDeletionScheduler) (*res.StartMissingNodesForDeletionScheduler, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	_, err := server.jobSchedulerClient.StartMissingNodesForDeletionScheduler(ctx, &ingest.StartMissingNodesForDeletionSchedulerRequest{})
	return &res.StartMissingNodesForDeletionScheduler{}, err
}

// StopMissingNodesForDeletionScheduler - stop the scheduled mark missing nodes for deletion task from running
func (server *ChefIngestJobSchedulerServer) StopMissingNodesForDeletionScheduler(ctx context.Context, empty *req.StopMissingNodesForDeletionScheduler) (*res.StopMissingNodesForDeletionScheduler, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	_, err := server.jobSchedulerClient.StopMissingNodesForDeletionScheduler(ctx, &ingest.StopMissingNodesForDeletionSchedulerRequest{})
	return &res.StopMissingNodesForDeletionScheduler{}, err
}

// ConfigureMissingNodesForDeletionScheduler changed the configurations for mark missing nodes for deletion scheduler
func (server *ChefIngestJobSchedulerServer) ConfigureMissingNodesForDeletionScheduler(ctx context.Context,
	config *req.SchedulerConfig) (*res.ConfigureMissingNodesForDeletionScheduler, error) {
	log.WithFields(log.Fields{
		"config": config.String(),
		"func":   nameOfFunc(),
	}).Debug("rpc call")

	parameters := &ingest.JobSettings{
		Every:     config.GetEvery(),
		Threshold: config.GetThreshold(),
		Running:   config.GetRunning(),
	}
	_, err := server.jobSchedulerClient.ConfigureMissingNodesForDeletionScheduler(ctx, parameters)

	return &res.ConfigureMissingNodesForDeletionScheduler{}, err
}

func toGwJobs(ingestJobs []*ingest.Job) []*res.Job {
	gwJobs := make([]*res.Job, len(ingestJobs))

	for index, job := range ingestJobs {
		gwJobs[index] = &res.Job{
			Running:     job.Running,
			Name:        job.Name,
			Every:       job.Every,
			Threshold:   job.Threshold,
			LastRun:     job.LastRun,
			NextRun:     job.NextRun,
			LastElapsed: job.LastRun,
			StartedOn:   job.StartedOn,
		}
	}

	return gwJobs
}
