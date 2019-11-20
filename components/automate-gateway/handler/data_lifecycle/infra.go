package datalifecycle

import (
	"context"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/external/data_lifecycle"
	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/ingest"
)

// GetInfraStatus returns the infra job statuses by combining the nodes missing,
// nodes deleting, nodes missing deleting, and infra purge workflows details.
func (s *Server) GetInfraStatus(ctx context.Context, req *api.GetInfraStatusRequest) (*api.GetInfraStatusResponse, error) {
	res := &api.GetInfraStatusResponse{}

	ingestSchedStatus, err := s.ingestJobClient.GetStatusJobScheduler(ctx, &ingest.JobSchedulerStatusRequest{})
	if err != nil {
		return res, err
	}

	jobs := ingestSchedStatus.GetJobs()
	if jobs != nil {
		for _, job := range jobs {
			res.Jobs = append(res.Jobs, ingestJobToJobStatus(job))
		}
	}

	purge, err := s.ingestPurgeClient.Show(ctx, &data_lifecycle.ShowRequest{})
	if err != nil {
		return res, err
	}

	res.Jobs = append(res.Jobs, purgeShowToJobStatus(purge))

	return res, nil
}

// RunInfra runs all of the infra data lifecycle jobs
func (s *Server) RunInfra(ctx context.Context, req *api.RunInfraRequest) (*api.RunInfraResponse, error) {
	res := &api.RunInfraResponse{}

	_, err := s.ingestJobClient.MarkNodesMissing(ctx, &ingest.MarkNodesMissingRequest{})
	if err != nil {
		return res, err
	}

	_, err = s.ingestJobClient.MarkMissingNodesForDeletion(ctx, &ingest.MarkMissingNodesForDeletionRequest{})
	if err != nil {
		return res, err
	}

	_, err = s.ingestJobClient.DeleteMarkedNodes(ctx, &ingest.DeleteMarkedNodesRequest{})
	if err != nil {
		return res, err
	}

	_, err = s.ingestPurgeClient.Run(ctx, &data_lifecycle.RunRequest{})
	if err != nil {
		return res, err
	}

	return res, nil
}

// SetInfraConfig configures all data lifecycle jobs
func (s *Server) SetInfraConfig(ctx context.Context, req *api.SetInfraConfigRequest) (*api.SetInfraConfigResponse, error) {
	res := &api.SetInfraConfigResponse{}

	settings := req.GetJobSettings()
	if settings == nil || len(settings) == 0 {
		return res, nil
	}

	for _, setting := range settings {
		configure, ok := s.infraConfigSetters()[setting.Name]
		if !ok {
			return res, status.Errorf(codes.InvalidArgument, "%s is not a valid infra job name", setting.Name)
		}

		err := configure(ctx, s, setting)
		if err != nil {
			return res, err
		}
	}

	return res, nil
}

func ingestJobToJobStatus(job *ingest.Job) *api.JobStatus {
	jobStatus := &api.JobStatus{
		Name:           job.Name,
		Disabled:       !job.Running,
		Recurrence:     job.Recurrence,
		Threshold:      job.Threshold,
		NextDueAt:      maybeToTimestamp(job.NextRun),
		LastEnqueuedAt: maybeToTimestamp(job.LastRun),
		LastStartedAt:  maybeToTimestamp(job.StartedOn),
		LastElapsed:    maybeToDuration(job.LastElapsed),
	}
	jobStatus.LastEndedAt = maybeToLastEnded(jobStatus.LastStartedAt, jobStatus.LastElapsed)

	return jobStatus
}

func jobSettingsToIngestJobSettings(setting *api.JobSettings) *ingest.JobSettings {
	return &ingest.JobSettings{
		Running:    !setting.Disabled,
		Recurrence: setting.Recurrence,
		Threshold:  setting.Threshold,
	}
}

type configSetter func(context.Context, *Server, *api.JobSettings) error
type configSetters map[string]configSetter

func (s *Server) infraConfigSetters() configSetters {
	return configSetters{
		"delete_nodes":               configureDeleteNodes,
		"missing_nodes":              configureMissingNodes,
		"missing_nodes_for_deletion": configureMissingNodesForDeletion,
		"periodic_purge_timeseries":  configurePurgePeriodicTimeseries,
	}
}

func configureDeleteNodes(ctx context.Context, s *Server, setting *api.JobSettings) error {
	_, err := s.ingestJobClient.ConfigureDeleteNodesScheduler(ctx, jobSettingsToIngestJobSettings(setting))

	return err
}

func configureMissingNodes(ctx context.Context, s *Server, setting *api.JobSettings) error {
	_, err := s.ingestJobClient.ConfigureNodesMissingScheduler(ctx, jobSettingsToIngestJobSettings(setting))

	return err
}

func configureMissingNodesForDeletion(ctx context.Context, s *Server, setting *api.JobSettings) error {
	_, err := s.ingestJobClient.ConfigureMissingNodesForDeletionScheduler(ctx, jobSettingsToIngestJobSettings(setting))

	return err
}

func configurePurgePeriodicTimeseries(ctx context.Context, s *Server, setting *api.JobSettings) error {
	_, err := s.ingestPurgeClient.Configure(ctx, jobSettingsToPurgeConfigure(setting))

	return err
}
