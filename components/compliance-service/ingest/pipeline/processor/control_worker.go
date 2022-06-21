package processor

import (
	"context"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"time"
)

var (
	ReportWorkflowName = cereal.NewWorkflowName("control-workflow")
	ReportTaskName     = cereal.NewTaskName("control-task")
)

const (
	RunningStatus string = "running"
	FailedStatus  string = "failed"
	SuccessStatus string = "success"
)

func InitCerealManager(cerealManager *cereal.Manager, workerCount int, client *ingestic.ESClient) error {
	logrus.Info("Successfully starting control-workflow")
	err := cerealManager.RegisterWorkflowExecutor(ReportWorkflowName, &ControlWorkflow{})
	if err != nil {
		logrus.Info("Found error control-workflow")
		logrus.Info(err)
		return err
	}

	logrus.Info("Successfully registered control-workflow")
	return cerealManager.RegisterTaskExecutor(ReportTaskName, &GenerateControlTask{
		ESClient: client,
	}, cereal.TaskExecutorOpts{Workers: workerCount})

}

type ControlWorkflow struct {
}

type ControlWorkflowParameters struct {
	ReportUuid string
	Retries    int
}

type ControlWorkflowPayload struct {
	ReportUuid  string
	RetriesLeft int
	Status      string
	StartTime   *time.Time
}

func (s *ControlWorkflow) OnStart(w cereal.WorkflowInstance,
	ev cereal.StartEvent) cereal.Decision {

	logrus.Info("In control-workflow start method")

	startTime := time.Now()
	workflowPayload := ControlWorkflowPayload{
		StartTime: &startTime,
	}

	workflowParams := ControlWorkflowParameters{}
	err := w.GetParameters(&workflowParams)
	if err != nil {
		err = errors.Wrap(err, "failed to unmarshal control-workflow parameters")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Infof("In On Start Method %s", workflowParams.ReportUuid)

	workflowPayload.ReportUuid = workflowParams.ReportUuid
	workflowPayload.RetriesLeft = workflowParams.Retries
	workflowPayload.Status = RunningStatus

	err = w.EnqueueTask(ReportTaskName, GenerateControlParameters{
		ReportUuid: workflowParams.ReportUuid,
	})
	if err != nil {
		err = errors.Wrap(err, "failed to enqueue the control-task")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}
	return w.Continue(&workflowPayload)
}

func (s *ControlWorkflow) OnTaskComplete(w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {

	var payload ControlWorkflowPayload

	if err := w.GetPayload(&payload); err != nil {
		err = errors.Wrap(err, "failed to unmarshal control-workflow payload")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Infof("Entered ControlWorkflow > OnTaskComplete with payload %+v", payload)

	if err := ev.Result.Err(); err != nil {
		//received error, if the retries are available enqueue the task
		if payload.RetriesLeft > 0 {
			logrus.Debugf("retring control-task %s", payload.ReportUuid)

			workflowParams := ControlWorkflowParameters{}
			err := w.GetParameters(&workflowParams)
			if err != nil {
				err = errors.Wrap(err, "failed to unmarshal control-workflow parameters")
				logrus.WithError(err).Error()
				return w.Fail(err)
			}

			err = w.EnqueueTask(ReportTaskName, GenerateControlParameters{
				ReportUuid: payload.ReportUuid,
			})
			if err != nil {
				err = errors.Wrap(err, "failed to enqueue the control-task")
				logrus.WithError(err).Error()
				return w.Fail(err)
			}

			payload.RetriesLeft--
			return w.Continue(&payload)
		}
		err = errors.Wrap(err, "failed to run control-task")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	//get the results returned by task run
	var controlResult GenerateControlParameters
	err := ev.Result.Get(&controlResult)
	if err != nil {
		err = errors.Wrap(err, "failed to get the task run result in OnTaskComplete")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}
	logrus.Infof("successfully completed the task %s", payload.ReportUuid)
	return w.Complete()
}

func (s *ControlWorkflow) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	logrus.Debugf("ReportWorkflow got OnCancel")
	return w.Complete()
}

type GenerateControlTask struct {
	ESClient *ingestic.ESClient
}

type GenerateControlParameters struct {
	ReportUuid string
	StartTime  *time.Time
	EndTime    *time.Time
}

func (t *GenerateControlTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {

	logrus.Info("OnStartYashviTask inside")
	var job GenerateControlParameters
	if err := task.GetParameters(&job); err != nil {
		err = errors.Wrap(err, "could not unmarshal GenerateReportParameters")
		logrus.WithError(err).Error()
		return nil, err
	}

	logrus.Infof("In TaskRun working on job %s", job.ReportUuid)

	startTime := time.Now().UTC().Round(time.Second)
	job.StartTime = &startTime
	return &job, nil
}
