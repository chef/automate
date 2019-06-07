package scheduler

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/ingest/ingest"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/inspec-agent/resolver"
	"github.com/chef/automate/components/compliance-service/inspec-agent/runner"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/compliance-service/scanner"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/chef/automate/lib/errorutils"
	"github.com/chef/automate/lib/workflow"
)

type Scheduler struct {
	managerClient  manager.NodeManagerServiceClient
	nodesClient    nodes.NodesServiceClient
	db             *pgdb.DB
	scannerServer  *scanner.Scanner
	runnerServer   *runner.Runner
	resolverServer *resolver.Resolver
}

func New(managerClient manager.NodeManagerServiceClient, nodesClient nodes.NodesServiceClient, db *pgdb.DB,
	ingestClient ingest.ComplianceIngesterClient, secretsClient secrets.SecretsServiceClient, remoteInspecVer string, workflowManager *workflow.WorkflowManager) *Scheduler {
	logrus.Debugf("setting up the scheduler server with mgrclient %+v and nodesclient %+v and ingestclient %+v and secretsclient %+v", managerClient, nodesClient, ingestClient, secretsClient)

	// set up the server connections for resolver, runner, and scanner
	resolverServer := resolver.New(managerClient, nodesClient, db, secretsClient)
	scannerServer := scanner.New(managerClient, nodesClient, db)
	runnerServer := runner.New(scannerServer, remoteInspecVer, workflowManager)

	return &Scheduler{managerClient, nodesClient, db, scannerServer, runnerServer, resolverServer}
}

// Run a job. Schedule, resolve, distribute, and execute it.
func (a *Scheduler) Run(job *jobs.Job) error {
	// Here we initialize a new context since if we used a passed in context here,
	// the context eventually gets cancelled and the chain of calls is not completed.
	ctx := context.Background()

	logrus.Debugf("Processing job: %+v", job)
	// 1. Schedule
	jobToRun, err := a.scheduleJob(job)
	if err != nil {
		strErr := fmt.Sprintf("Failed to schedule job %s: %s", job.Id, err.Error())
		logrus.Error(strErr)
		return errors.New(strErr)
	}
	// 2. Resolve
	if jobToRun != nil {
		nodeJobs, err := a.resolverServer.ResolveJob(ctx, jobToRun)
		if err != nil {
			strErr := fmt.Sprintf("Failed to resolve job %s: %s", job.Id, err.Error())
			logrus.Error(strErr)
			return errors.New(strErr)
		}
		// 3. Distribute and Execute
		err = a.runNodeJobs(ctx, job, nodeJobs)
	} else {
		logrus.Debugf("No jobs provided, moving on...")
	}
	return nil
}
func (a *Scheduler) runNodeJobs(ctx context.Context, job *jobs.Job, nodeJobs []*types.InspecJob) error {
	if len(nodeJobs) == 0 {
		now := time.Now()
		status := types.StatusAborted
		nodeJob := &types.InspecJob{
			InspecBaseJob: types.InspecBaseJob{
				JobID: job.Id,
			},
			StartTime:  &now,
			EndTime:    &now,
			NodeStatus: &status,
		}
		a.scannerServer.UpdateJobStatus(nodeJob.JobID, "failed", nodeJob.StartTime, nodeJob.EndTime)
		a.scannerServer.UpdateResult(ctx, nodeJob, nil, &inspec.Error{Message: "no nodes found"}, "")
		return errors.New("no nodes found for job, aborting")
	}

	err := a.runnerServer.AddJobs(job.Id, nodeJobs)
	if err != nil {
		strErr := fmt.Sprintf("Unable to add jobs to inspec agent: %s", err.Error())
		logrus.Error(strErr)
		return errors.New(strErr)
	}
	return nil
}

func (a *Scheduler) scheduleJob(job *jobs.Job) (*jobs.Job, error) {
	// A non-recurrent job should run right away, no need to create child
	// Just return job so it can be resolved and executed
	if job.Recurrence == "" {
		return job, nil
	}

	// Ensure recurrence rule can be parsed
	_, err := rrule.StrToRRule(job.Recurrence)
	if err != nil {
		return nil, &errorutils.InvalidError{Msg: fmt.Sprintf("invalid job recurrence rule: %v", err)}
	}
	a.scannerServer.UpdateParentJobSchedule(job.Id, job.JobCount, job.Recurrence, job.ScheduledTime)
	return nil, nil
}

// PollForJobs loops every minute looking to create child jobs from recurring due jobs
func (a *Scheduler) PollForJobs(ctx context.Context) {
	for {
		time.Sleep(time.Minute)
		now := time.Now().UTC()
		go a.processDueJobs(ctx, now)
	}
}

// processDueJobs uses GetDueJobs to query the database for any recurring jobs that are due to be run
// For every due recurrent job, a child job is created and then the recurrent(parent) job updated
func (a *Scheduler) processDueJobs(ctx context.Context, nowTime time.Time) {
	dueJobs := a.scannerServer.GetDueJobs(nowTime)
	if len(dueJobs) > 0 {
		logrus.Debugf("processDueJobs, %d recurring jobs are due for running...", len(dueJobs))
	}
	for _, job := range dueJobs {
		job.JobCount++
		parentJobID := job.Id
		parentJobCount := job.JobCount
		parentRecurrence := job.Recurrence
		parentScheduledTime := job.ScheduledTime

		err := a.handleChildJob(ctx, job)
		if err != nil {
			logrus.Errorf("Error handling child job %q: %v", job.Id, err)
			return
		}
		a.scannerServer.UpdateParentJobSchedule(parentJobID, parentJobCount, parentRecurrence, parentScheduledTime)
	}
}

func (a *Scheduler) handleChildJob(ctx context.Context, job *jobs.Job) error {
	job.Name = fmt.Sprintf("%s - run %d", job.Name, job.JobCount)
	job.Recurrence = ""
	childJob, err := a.scannerServer.CreateChildJob(job)
	if err != nil {
		return errors.Wrap(err, "Failed to create child job")
	}
	nodeJobs, err := a.resolverServer.ResolveJob(ctx, childJob)
	if err != nil {
		return errors.Wrap(err, "Failed to resolve job")
	}
	err = a.runnerServer.AddJobs(childJob.Id, nodeJobs)
	if err != nil {
		return errors.Wrap(err, "Failed to hand jobs to workers")
	}
	return nil
}

func (a *Scheduler) RunHungJobs(ctx context.Context, scheduledJobsIds []string) {
	// send each of the jobs through
	for _, jobID := range scheduledJobsIds {
		job, err := a.db.GetJob(jobID)
		if err != nil {
			logrus.Errorf("RunHungJobs unable get job info: %v", err)
			continue
		}

		err = a.Run(job)
		if err != nil {
			logrus.Errorf("RunHungJobs unable to hand job over to inspec agent %v", err)
			continue
		}
	}
}
