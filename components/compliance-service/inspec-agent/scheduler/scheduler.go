package scheduler

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"

	"github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/scanner"
	"github.com/chef/automate/lib/errorutils"
	"github.com/chef/automate/lib/workflow"
)

type Scheduler struct {
	scanner         *scanner.Scanner
	workflowManager *workflow.WorkflowManager
}

func New(scanner *scanner.Scanner, workflowManager *workflow.WorkflowManager) *Scheduler {
	return &Scheduler{scanner, workflowManager}
}

// Run a job. Schedule, resolve, distribute, and execute it.
func (a *Scheduler) Run(job *jobs.Job) error {
	logrus.Debugf("Processing job: %+v", job)

	// If the job has a recurrence, we update the job schedule
	if job.Recurrence != "" {
		// Ensure recurrence rule can be parsed
		_, err := rrule.StrToRRule(job.Recurrence)
		if err != nil {
			return &errorutils.InvalidError{Msg: fmt.Sprintf("invalid job recurrence rule: %v", err)}
		}
		a.scanner.UpdateParentJobSchedule(job.Id, job.JobCount, job.Recurrence, job.ScheduledTime)
		return nil
	}

	err := a.pushWorkflow(job)
	if err != nil {
		strErr := fmt.Sprintf("Unable to add jobs to inspec agent: %s", err.Error())
		logrus.Error(strErr)
		return errors.New(strErr)
	}
	return nil
}

func (a *Scheduler) pushWorkflow(job *jobs.Job) error {
	logrus.Debugf("Calling EnqueueWorkflow for scan-job-workflow")
	return a.workflowManager.EnqueueWorkflow(context.TODO(), "scan-job-workflow", fmt.Sprintf("scan-job-%s", job.Id), job)
}

// PollForJobs loops every minute looking to create child jobs from recurring due jobs
func (a *Scheduler) PollForJobs(ctx context.Context) {
	for {
		time.Sleep(time.Minute)
		now := time.Now().UTC()
		go a.processDueJobs(ctx, now)
	}
}

// processDueJobs uses GetDueJobs to query the database for any
// recurring jobs that are due to be run. For every due recurrent job,
// we push that job onto the workflow queue.
func (a *Scheduler) processDueJobs(ctx context.Context, nowTime time.Time) {
	dueJobs := a.scanner.GetDueJobs(nowTime)
	if len(dueJobs) > 0 {
		logrus.Debugf("processDueJobs, %d recurring jobs are due for running...", len(dueJobs))
	}
	for _, job := range dueJobs {
		err := a.pushWorkflow(job)
		if err != nil {
			if err == workflow.ErrWorkflowInstanceExists {
				logrus.Infof("Job %q is still/already running", job.Id)
			} else {
				logrus.Errorf("Error handling job %q: %v", job.Id, err)
			}
		}
	}
}
