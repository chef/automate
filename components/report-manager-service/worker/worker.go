package worker

import (
	"context"
	"time"

	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

var (
	ReportWorkflowName = cereal.NewWorkflowName("report-workflow")
	ReportTaskName     = cereal.NewTaskName("report-task")
)

func InitCerealManager(cerealManager *cereal.Manager, workerCount int) error {
	err := cerealManager.RegisterWorkflowExecutor(ReportWorkflowName, &ReportWorkflow{})
	if err != nil {
		return err
	}

	return cerealManager.RegisterTaskExecutor(ReportTaskName, &GenerateReportTask{},
		cereal.TaskExecutorOpts{Workers: workerCount})
}

type ReportWorkflow struct{}
type ReportWorkflowParameters struct {
	JobID   string
	Retries int
}
type ReportWorkflowPayload struct {
	JobID       string
	Status      string
	RetriesLeft int
	StartTime   *time.Time
}

func (p *ReportWorkflow) OnStart(w cereal.WorkflowInstance,
	ev cereal.StartEvent) cereal.Decision {

	startTime := time.Now()
	workflowPayload := ReportWorkflowPayload{
		StartTime: &startTime,
	}

	workflowParams := ReportWorkflowParameters{}
	err := w.GetParameters(&workflowParams)
	if err != nil {
		err = errors.Wrap(err, "failed to unmarshal report-workflow parameters")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Infof("In On Start Method %w", workflowParams.JobID)

	workflowPayload.JobID = workflowParams.JobID
	workflowPayload.RetriesLeft = workflowParams.Retries
	workflowPayload.Status = "running"

	err = w.EnqueueTask(ReportTaskName, GenerateReportParameters{
		JobID: workflowParams.JobID,
	})
	if err != nil {
		err = errors.Wrap(err, "failed to enqueue the report-task")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	return w.Continue(&workflowPayload)
}

func (p *ReportWorkflow) OnTaskComplete(w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {

	var payload ReportWorkflowPayload

	if err := w.GetPayload(&payload); err != nil {
		err = errors.Wrap(err, "failed to unmarshal report-workflow payload")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Infof("Entered ReportWorkflow > OnTaskComplete with payload %+v", payload)

	if err := ev.Result.Err(); err != nil {
		//received error, if the retries are available enqueue the task
		if payload.RetriesLeft > 0 {
			logrus.Debugf("retring report-task %s", payload.JobID)
			w.EnqueueTask(ReportTaskName, GenerateReportParameters{
				JobID: payload.JobID,
			})
			payload.RetriesLeft--
			return w.Continue(&payload)
		}
		err = errors.Wrap(err, "failed to run report-task")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}
	logrus.Infof("successfully completed the task %s", payload.JobID)
	return w.Complete()
}

func (s *ReportWorkflow) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	logrus.Debugf("ReportWorkflow got OnCancel")
	return w.Complete()
}

type GenerateReportTask struct {
}

type GenerateReportParameters struct {
	JobID     string
	StartTime *time.Time
	EndTime   *time.Time
}

func (t *GenerateReportTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	var job GenerateReportParameters
	if err := task.GetParameters(&job); err != nil {
		return nil, errors.Wrap(err, "could not unmarshal GenerateReportParameters")
	}

	logrus.Infof("In TaskRun working on job %s", job.JobID)

	startTime := time.Now().UTC().Round(time.Second)
	job.StartTime = &startTime

	//TODO:: perform actual job
	var err error
	time.Sleep(1 * time.Minute)

	endTime := time.Now().UTC().Round(time.Second)
	job.EndTime = &endTime

	return &job, err
}
