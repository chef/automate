package server

import (
	"context"

	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/config"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	log "github.com/sirupsen/logrus"
)

type JobSchedulerServer struct {
	client        backend.Client
	jobScheduler  *JobScheduler
	configManager *config.Manager
}

// NewJobSchedulerServer - create a new JobSchedulerServer
func NewJobSchedulerServer(client backend.Client, jobScheduler *JobScheduler, configManager *config.Manager) *JobSchedulerServer {
	server := &JobSchedulerServer{
		client:        client,
		configManager: configManager,
		jobScheduler:  jobScheduler,
	}

	server.initialize()

	return server
}

func (server *JobSchedulerServer) initialize() {
	jConfig := server.configManager.GetJobSchedulerConfig()
	if jConfig.Running {
		server.jobScheduler.Start()
	} else {
		server.jobScheduler.Stop()
	}

	// Add all configured jobs to the JobSchedulerServer
	for _, jobID := range server.configManager.GetJobsID() {
		server.addUpdateJob(server.configManager.GetJobConfig(jobID))
	}
}

// StartJobScheduler - Start the Job Scheduler
func (server *JobSchedulerServer) StartJobScheduler(ctx context.Context,
	empty *ingest.StartJobSchedulerRequest) (*ingest.StartJobSchedulerResponse, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	jConfig := server.configManager.GetJobSchedulerConfig()
	if !jConfig.Running {
		jConfig.Running = true
		err := server.configManager.UpdateJobSchedulerConfig(jConfig)
		if err != nil {
			return &ingest.StartJobSchedulerResponse{}, status.Error(codes.Internal, err.Error())
		}

		server.jobScheduler.Start()
	}

	return &ingest.StartJobSchedulerResponse{}, nil
}

// StopJobScheduler - Stop the Job Scheduler
func (server *JobSchedulerServer) StopJobScheduler(ctx context.Context,
	empty *ingest.StopJobSchedulerRequest) (*ingest.StopJobSchedulerResponse, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	jConfig := server.configManager.GetJobSchedulerConfig()
	if jConfig.Running {
		jConfig.Running = false
		err := server.configManager.UpdateJobSchedulerConfig(jConfig)
		if err != nil {
			return &ingest.StopJobSchedulerResponse{}, status.Error(codes.Internal, err.Error())
		}

		server.jobScheduler.Stop()
	}

	return &ingest.StopJobSchedulerResponse{}, nil
}

// GetStatusJobScheduler - collect and return the status of all the jobs in the Job Scheduler
func (server *JobSchedulerServer) GetStatusJobScheduler(ctx context.Context,
	empty *ingest.JobSchedulerStatusRequest) (*ingest.JobSchedulerStatus, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	jConfig := server.configManager.GetJobSchedulerConfig()
	jobs, err := server.jobScheduler.GetJobsStatus()
	if err != nil {
		return &ingest.JobSchedulerStatus{}, nil
	}

	status := ingest.JobSchedulerStatus{
		Running: jConfig.Running,
		Jobs:    jobs,
	}

	return &status, nil
}
