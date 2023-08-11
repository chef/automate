package licenseaudit

import (
	"context"
	"fmt"
	"time"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	log "github.com/sirupsen/logrus"
	"github.com/teambition/rrule-go"
)

var (
	WorkflowName         = cereal.NewWorkflowName("license-audit")
	LicenseAuditTaskName = cereal.NewTaskName("license-audit-task")
	ScheduleName         = "license-audit"
	Command              = "HAB_LICENSE=accept-no-persist hab pkg exec chef/license-audit "
)

func InitCerealManager(ctx context.Context, cerealManager *cereal.Manager, workerCount int) error {
	log.Info("Successfully starting license-audit-workflow")
	err := cerealManager.RegisterWorkflowExecutor(WorkflowName, &LicenseAuditWorkflow{})
	if err != nil {
		log.Errorf("Found error in Registering workflow for license-audit %v", err)
		return err
	}

	log.Info("Successfully registered migration-workflow")
	err = cerealManager.RegisterTaskExecutor(LicenseAuditTaskName, &LicenseAuditTask{ExecuteCommand: NewExecute(logger.NewLogrusStandardLogger()), Command: Command}, cereal.TaskExecutorOpts{Workers: workerCount})
	if err != nil {
		log.Errorf("Found error in RegisterTaskExecutor for license-audit %v", err)
		return err
	}

	rule, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.SECONDLY,
		Interval: 60,
		Dtstart:  time.Now(),
	})

	if err != nil {
		return errors.Wrapf(err, "Unable to create rule for schedule")
	}

	return createOrUpdateWorkflowSchedule(ctx, cerealManager, ScheduleName, WorkflowName, rule)

}

func createOrUpdateWorkflowSchedule(ctx context.Context, cerealManager *cereal.Manager, scheduleName string, auditWorkflowName cereal.WorkflowName, rule *rrule.RRule) error {
	err := cerealManager.CreateWorkflowSchedule(ctx, scheduleName, auditWorkflowName, AuditWorkflowParameters{Retries: 3}, true, rule)

	if err == nil {
		return nil
	}

	if err == cereal.ErrWorkflowScheduleExists {
		schedule, err := cerealManager.GetWorkflowScheduleByName(ctx, scheduleName, auditWorkflowName)
		if err != nil {
			return errors.Wrapf(err, "failed to get scheduled workflow %s from cereal manager", scheduleName)
		}
		scheduledRule, err := schedule.GetRRule()
		if err != nil {
			return errors.Wrapf(err, "unable to get rrule for scheduled workflow %s", scheduleName)
		}
		if scheduledRule != rule {
			err = cerealManager.UpdateWorkflowScheduleByName(ctx, scheduleName, auditWorkflowName, cereal.UpdateRecurrence(rule))
			if err != nil {
				return errors.Wrapf(err, "unable to update recurrence rule for scheduled workflow %s", scheduleName)
			}
		}

	} else {
		return errors.Wrapf(err, "could not continue creating license-audit workflow schedule %s", scheduleName)
	}
	return nil
}

type LicenseAuditWorkflow struct {
}

type LicenseAuditTask struct {
	Command        string
	ExecuteCommand ExecuteCommand
}

type AuditWorkflowParameters struct {
	Retries int
}

type AuditWorkflowPayload struct {
	Retries int
}

func (s *LicenseAuditWorkflow) OnStart(w cereal.WorkflowInstance,
	ev cereal.StartEvent) cereal.Decision {

	logrus.Info("Starting the workflow for audit")

	workflowParams, err := getWorkflowParametersAndEnqueTask(w)
	if err != nil {
		return w.Fail(err)
	}

	workflowPayload := &AuditWorkflowPayload{
		Retries: workflowParams.Retries,
	}

	return w.Continue(workflowPayload)
}

func (s *LicenseAuditWorkflow) OnTaskComplete(w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {

	var payload AuditWorkflowPayload
	if err := w.GetPayload(&payload); err != nil {
		err = errors.Wrap(err, "failed to unmarshal license-audit payload in OnComplete")
		log.WithError(err).Error()
		return w.Fail(err)
	}

	log.Debugf("Entered license-audit  > OnTaskComplete with payload %+v", payload)

	if err := ev.Result.Err(); err != nil {
		//received error, if the retries are available enqueue the task
		if payload.Retries > 0 {
			logrus.Debugf("Retrying license audit")
			_, err := getWorkflowParametersAndEnqueTask(w)
			if err != nil {
				return w.Fail(err)
			}
			payload.Retries--
			return w.Continue(&payload)
		}

		err = errors.Wrap(err, "failed to run license-audit")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	//get the results returned by task run
	var jobResult AuditWorkflowParameters
	err := ev.Result.Get(&jobResult)
	if err != nil {
		err = errors.Wrap(err, "failed to get the task run result in OnTaskComplete for license audit")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	log.Debugf("Exiting license-audit > OnTaskComplete with payload-------------- %+v", payload)

	return w.Complete()
}

func (s *LicenseAuditWorkflow) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	return w.Complete()
}

type AuditTaskParameters struct {
}

func (t *LicenseAuditTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	log.Info("Executing the task For Run method")
	var job AuditTaskParameters
	if err := task.GetParameters(&job); err != nil {
		err = errors.Wrap(err, "Unable to marshal Task Parameters for Audit task")
		log.WithError(err).Error()
		return nil, err
	}

	output, err := executeCommandforAudit(t.ExecuteCommand, getAppendedCommand(t.Command))
	if err != nil {
		log.Errorf("Failed to execute the command for audit")
		return nil, err
	}

	log.Debugf("License analytics output received is %s", output)

	return &job, nil
}

// executeCommandforAudit executs the command
func executeCommandforAudit(executeCommand ExecuteCommand, command string) (string, error) {
	return executeCommand.Execute(command)
}

// getAppendedCommand gets the appended command with date
func getAppendedCommand(commandToExecute string) string {
	yesterdayDate := time.Now().AddDate(0, 0, -1).UTC().Format("2006-01-02")
	return fmt.Sprintf(commandToExecute, yesterdayDate, yesterdayDate)
}

func getWorkflowParametersAndEnqueTask(w cereal.WorkflowInstance) (AuditWorkflowParameters, error) {
	workflowParams := AuditWorkflowParameters{}
	err := w.GetParameters(&workflowParams)
	if err != nil {
		err = errors.Wrap(err, "failed to unmarshal license-audit workflow parameters")
		logrus.WithError(err).Error()
		return workflowParams, err
	}

	err = w.EnqueueTask(LicenseAuditTaskName, AuditTaskParameters{})
	if err != nil {
		err = errors.Wrap(err, "failed to enqueue the license-audit task")
		logrus.WithError(err).Error()
		return workflowParams, err
	}

	return workflowParams, nil
}
