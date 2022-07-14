package migrations

import (
	"context"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

var (
	MigrationWorkflowName         = cereal.NewWorkflowName("migration-workflow")
	DayLatestMigrationTaskName    = cereal.NewTaskName("day-latest-task")
	ControlIndexMigrationTaskName = cereal.NewTaskName("control-index-task")
)

func InitCerealManager(cerealManager *cereal.Manager, workerCount int, client *ingestic.ESClient, upgradesDB *UpgradesDB) error {
	logrus.Info("Successfully starting control-workflow")
	err := cerealManager.RegisterWorkflowExecutor(MigrationWorkflowName, &MigrationWorkflow{})
	if err != nil {
		logrus.Info("Found error migration-workflow")
		logrus.Info(err)
		return err
	}

	logrus.Info("Successfully registered migration-workflow")
	return cerealManager.RegisterTaskExecutor(DayLatestMigrationTaskName, &GenerateDayLatestMigrationTask{
		ESClient:   client,
		UpgradesDB: upgradesDB,
	}, cereal.TaskExecutorOpts{Workers: workerCount})

}

type MigrationWorkflow struct {
}

type MigrationWorkflowParameters struct {
	DayLatestFlag    bool
	ControlIndexFlag bool
}

type MigrationWorkflowPayload struct {
	DayLatestFlag    bool
	ControlIndexFlag bool
}

func (s *MigrationWorkflow) OnStart(w cereal.WorkflowInstance,
	ev cereal.StartEvent) cereal.Decision {

	logrus.Debug("In control-workflow start method")

	workflowParams := MigrationWorkflowParameters{}
	err := w.GetParameters(&workflowParams)
	if err != nil {
		err = errors.Wrap(err, "failed to unmarshal control-workflow parameters")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Debugf("In On Start Method %s", workflowParams.DayLatestFlag)

	workflowPayload := MigrationWorkflowPayload{
		DayLatestFlag: workflowParams.DayLatestFlag,
	}

	err = w.EnqueueTask(DayLatestMigrationTaskName, GenerateDayLatestMigrationParameters{
		DayLatestFlag: workflowParams.DayLatestFlag,
	})
	if err != nil {
		err = errors.Wrap(err, "failed to enqueue the control-task")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}
	return w.Continue(&workflowPayload)
}

func (s *MigrationWorkflow) OnTaskComplete(w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {

	var payload MigrationWorkflowPayload
	if err := w.GetPayload(&payload); err != nil {
		err = errors.Wrap(err, "failed to unmarshal control-workflow payload")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Debugf("Entered ControlWorkflow > OnTaskComplete with payload %+v", payload)

	//get the results returned by task run
	var dayLatestParameters GenerateDayLatestMigrationParameters
	err := ev.Result.Get(&dayLatestParameters)
	if err != nil {
		err = errors.Wrap(err, "failed to get the task run result in OnTaskComplete")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	return w.Complete()
}

func (s *MigrationWorkflow) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	logrus.Debugf("ReportWorkflow got OnCancel")
	return w.Complete()
}

type GenerateDayLatestMigrationTask struct {
	ESClient   *ingestic.ESClient
	UpgradesDB *UpgradesDB
}

type GenerateDayLatestMigrationParameters struct {
	DayLatestFlag bool
}

func (t *GenerateDayLatestMigrationTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {

	var job GenerateDayLatestMigrationParameters
	if err := task.GetParameters(&job); err != nil {
		err = errors.Wrap(err, "could not unmarshal GenerateReportParameters")
		logrus.WithError(err).Error()
		return nil, err
	}

	logrus.Info("Inside the daily Latest Flag upgrades")
	err := t.ESClient.SetNodesDayLatestFalse(ctx)
	if err != nil {
		return nil, errors.Wrap(err, "could not update the day latest in rep data")
	}
	err = t.UpgradesDB.UpdateDayLatestFlagToTrue()
	if err != nil {
		return nil, errors.Wrap(err, "could not update flag the in upgrade database")
	}
	return &job, nil
}
