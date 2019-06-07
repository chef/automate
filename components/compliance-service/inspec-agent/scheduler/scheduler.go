package scheduler

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"

	"github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/scanner"
	"github.com/chef/automate/lib/errorutils"
	"github.com/chef/automate/lib/workflow"
)

type Scheduler struct {
	db              *pgdb.DB
	scanner         *scanner.Scanner
	workflowManager *workflow.WorkflowManager
}

func New(db *pgdb.DB, scanner *scanner.Scanner, workflowManager *workflow.WorkflowManager) *Scheduler {
	return &Scheduler{db, scanner, workflowManager}
}

// Run a job. Schedule, resolve, distribute, and execute it.
func (a *Scheduler) Run(job *jobs.Job) error {
	logrus.Debugf("Processing job: %+v", job)
	// 1. Schedule
	jobToRun, err := a.scheduleJob(job)
	if err != nil {
		strErr := fmt.Sprintf("Failed to schedule job %s: %s", job.Id, err.Error())
		logrus.Error(strErr)
		return errors.New(strErr)
	}
	// 2. Trigger Workflow
	if jobToRun != nil {
		err := a.pushWorkflow(job)
		if err != nil {
			strErr := fmt.Sprintf("Unable to add jobs to inspec agent: %s", err.Error())
			logrus.Error(strErr)
			return errors.New(strErr)
		}
	} else {
		logrus.Debugf("No jobs provided, moving on...")
	}
	return nil
}

func (a *Scheduler) pushWorkflow(job *jobs.Job) error {
	logrus.Debugf("Calling EnqueueWorkflow for scan-job-workflow")
	return a.workflowManager.EnqueueWorkflow(context.TODO(), "scan-job-workflow", fmt.Sprintf("scan-job-%s", job.Id), job)
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
	a.scanner.UpdateParentJobSchedule(job.Id, job.JobCount, job.Recurrence, job.ScheduledTime)
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
	dueJobs := a.scanner.GetDueJobs(nowTime)
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
		a.scanner.UpdateParentJobSchedule(parentJobID, parentJobCount, parentRecurrence, parentScheduledTime)
	}
}

func (a *Scheduler) handleChildJob(ctx context.Context, job *jobs.Job) error {
	job.Name = fmt.Sprintf("%s - run %d", job.Name, job.JobCount)
	job.Recurrence = ""
	childJob, err := a.scanner.CreateChildJob(job)
	if err != nil {
		return errors.Wrap(err, "Failed to create child job")
	}
	err = a.pushWorkflow(childJob)
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
