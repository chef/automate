package server

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/lib/cereal"
)

type JobSchedulerServer struct {
	client     backend.Client
	jobManager *cereal.Manager
}

// NewJobSchedulerServer - create a new JobSchedulerServer
func NewJobSchedulerServer(client backend.Client, manager *cereal.Manager) *JobSchedulerServer {
	return &JobSchedulerServer{
		client:     client,
		jobManager: manager,
	}
}

// GetStatusJobScheduler - collect and return the status of all the jobs in the Job Scheduler
func (server *JobSchedulerServer) GetStatusJobScheduler(ctx context.Context,
	empty *ingest.JobSchedulerStatusRequest) (*ingest.JobSchedulerStatus, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	schedules, err := server.jobManager.ListWorkflowSchedules(ctx)
	if err != nil {
		return &ingest.JobSchedulerStatus{}, status.Error(codes.Internal, err.Error())
	}

	var (
		jobs      = []*ingest.Job{}
		jobStatus = &ingest.JobSchedulerStatus{}
	)
	for _, sched := range schedules {
		// Skip the purge workflow because it has it's own gRPC interface for
		// management
		if sched.WorkflowName == PurgeWorkflowName.String() {
			continue
		}

		// SingleJobWorkflow's
		var (
			threshold string
			job       *ingest.Job
		)

		if err = sched.GetParameters(&threshold); err != nil {
			return jobStatus, status.Error(codes.Internal, err.Error())
		}

		job, err = workflowToIngestJob(sched, sched.WorkflowName, threshold)
		if err != nil {
			return jobStatus, status.Error(codes.Internal, err.Error())
		}
		jobs = append(jobs, job)
	}

	jobStatus.Running = true
	jobStatus.Jobs = jobs

	return jobStatus, nil
}

func (server *JobSchedulerServer) runWorkflowNow(ctx context.Context, workflowName cereal.WorkflowName) error {
	sched, err := server.jobManager.GetWorkflowScheduleByName(ctx, workflowNameToInstanceName(workflowName), workflowName)
	if err != nil {
		return err
	}

	var threshold string
	if err := sched.GetParameters(&threshold); err != nil {
		return err
	}

	err = server.jobManager.EnqueueWorkflow(ctx, workflowName, workflowNameToInstanceName(workflowName), threshold)
	if err == cereal.ErrWorkflowInstanceExists {
		// TODO(ssd) 2019-06-28: It would be nice if there was
		// a way to queue this up rather than rejecting it.
		log.Warnf("workflow %q is already running, not running now", workflowName)
		return nil
	}
	return err
}

func workflowToIngestJob(sched *cereal.Schedule, name string, threshold string) (*ingest.Job, error) {
	every, err := rruleToEvery(sched.Recurrence)
	if err != nil {
		return nil, errors.Wrapf(
			err,
			"failed to parse recurrence for workflow %s",
			name,
		)
	}

	j := ingest.Job{
		Running:    sched.Enabled,
		Name:       name,
		Every:      every,
		Threshold:  threshold,
		Recurrence: sched.Recurrence,
		NextRun:    getTimeString(sched.NextDueAt),
	}

	if sched.LastStart != nil && sched.LastEnd != nil {
		j.LastRun = getTimeString(*sched.LastStart)
		j.LastElapsed = sched.LastEnd.Sub(*sched.LastStart).String()
	}

	return &j, nil
}

func getTimeString(dateTime time.Time) string {
	if dateTime.IsZero() {
		return ""
	}
	return dateTime.Format(time.RFC3339Nano)
}

func rruleToEvery(rruleStr string) (string, error) {
	r, err := rrule.StrToRRule(rruleStr)
	if err != nil {
		return "", err
	}

	switch r.OrigOptions.Freq {
	case rrule.DAILY:
		return fmt.Sprintf("%dd", r.OrigOptions.Interval), nil
	case rrule.HOURLY:
		return fmt.Sprintf("%dh", r.OrigOptions.Interval), nil
	case rrule.MINUTELY:
		return fmt.Sprintf("%dm", r.OrigOptions.Interval), nil
	case rrule.SECONDLY:
		// TODO(ssd) 2019-06-28: We should replace this with
		// something that remembers the user-provided
		// recurrence since our translation to an rrule will
		// necessarily lose the user's preferred description.
		if r.OrigOptions.Interval%3600 == 0 {
			return fmt.Sprintf("%dh", r.OrigOptions.Interval/3600), nil
		} else if r.OrigOptions.Interval%60 == 0 {
			return fmt.Sprintf("%dm", r.OrigOptions.Interval/60), nil
		}
		return fmt.Sprintf("%ds", r.OrigOptions.Interval), nil
	default:
		return "", errors.New("Unsupported rrule to duration conversion")
	}
}
