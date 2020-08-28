package purge

import (
	"context"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/teambition/rrule-go"

	es "github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/lib/cereal"
)

// There's no reason a purge should take longer than 10 mins to start
// Even if it does, we'll try again later
const failPurgeIfExecutedAfter = 10 * time.Minute

var errEnqueuedAtZero = errors.New("EnqueuedAt is zero")
var errExpired = errors.New("Purge did not start in time")
var taskName = cereal.NewTaskName("purge")

// ConfigureManager registers the purge workflow executor and task
// executor.
func ConfigureManager(man *cereal.Manager, workflowName cereal.WorkflowName, opts ...TaskOpt) error {
	task := &Task{}
	for _, o := range opts {
		o(task)
	}

	err := man.RegisterTaskExecutor(taskName, task, cereal.TaskExecutorOpts{})
	if err != nil {
		return err
	}
	err = man.RegisterWorkflowExecutor(workflowName, &Workflow{})
	if err != nil {
		return err
	}
	return nil
}

// CreateOrUpdatePurgeWorkflow creates a purge scheduled workflow. If a matching
// purge workflow exists it update the policies with any new default policies
// that don't exist. It also updates existing elasticsearch policies index
// names and purge fields in case they have changed.
func CreateOrUpdatePurgeWorkflow(
	ctx context.Context,
	man *cereal.Manager,
	scheduleName string,
	workflowName cereal.WorkflowName,
	defaultPolicies *Policies,
	enabled bool,
	recurrence *rrule.RRule) error {

	err := man.CreateWorkflowSchedule(
		ctx,
		scheduleName,
		workflowName,
		defaultPolicies,
		true,
		recurrence,
	)

	if err == cereal.ErrWorkflowScheduleExists {
		sched, err := man.GetWorkflowScheduleByName(ctx, scheduleName, workflowName)
		if err != nil {
			return errors.Wrap(err, "failed to get purge schedule from job manager")
		}

		currentPolicies := NewPolicies()
		if err = sched.GetParameters(&currentPolicies); err != nil {
			return errors.Wrap(err, "failed to get purge policies from job manager")
		}

		// Update existing policy indices or migrate a new default policy into
		// the policies.
		for name, dp := range defaultPolicies.Es {
			p, ok := currentPolicies.Es[name]
			if ok {
				p.IndexName = dp.IndexName
				p.CustomPurgeField = dp.CustomPurgeField
				currentPolicies.Es[name] = p
			} else {
				currentPolicies.Es[name] = dp
			}
		}

		// NOTE: this is a shim until pg support is finished
		for name, dp := range defaultPolicies.Pg {
			p, ok := currentPolicies.Pg[name]
			if ok {
				// Migrate things?
				currentPolicies.Pg[name] = p
			} else {
				currentPolicies.Pg[name] = dp
			}
		}

		// TODO: Add support for removing policies?

		err = man.UpdateWorkflowScheduleByName(
			context.Background(),
			scheduleName,
			workflowName,
			cereal.UpdateParameters(currentPolicies),
		)
		if err != nil {
			return errors.Wrap(err, "failed to update purge policy workflow schedule")
		}
	} else if err != nil {
		return errors.Wrap(err, "could not create purge scheduled workflow")
	}

	return nil
}

type TaskOpt func(*Task)

func WithTaskEsSidecarClient(client es.EsSidecarServiceClient) TaskOpt {
	return func(t *Task) {
		t.EsSidecarClient = client
	}
}

type Workflow struct{}

// There was a bug in a release of automate where purge workflows
// would be queued up, but would not run because nothing was listening for
// them. When we fixed this, people were confused by why automate was
// deleting data. More confusingly, the policy that would be used was the
// old one and not the current one as a workflow had been queued but not
// executed. We don't want these to run, so if each step of the purge does
// not happen within failPurgeIfExecutedAfter mins of when it was requested
// to happen, we fail.
func guardRun(enqueuedAt time.Time) error {
	if enqueuedAt.IsZero() {
		logrus.Errorf("Not purging because EnqueuedAt is zero. Will try again later")
		return errEnqueuedAtZero
	}
	now := time.Now()
	elapsed := now.Sub(enqueuedAt)
	logctx := logrus.WithFields(logrus.Fields{
		"enqueuedAt": enqueuedAt,
		"now":        now,
		"elapsed":    elapsed,
	})
	if elapsed > failPurgeIfExecutedAfter {
		logctx.Error("Not purging because task did not execute in time. Will try again later")
		return errExpired
	}
	return nil
}

func (s *Workflow) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	if err := guardRun(ev.EnqueuedAt); err != nil {
		return w.Fail(err)
	}

	policies := Policies{}
	err := w.GetParameters(&policies)
	if err != nil {
		return w.Fail(err)
	}

	err = w.EnqueueTask(taskName, policies)
	if err != nil {
		return w.Fail(err)
	}

	return w.Continue(nil)
}

func (s *Workflow) OnTaskComplete(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
	// TODO: Handle failures and retry?
	return w.Complete()
}

func (s *Workflow) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	return w.Fail(errors.New("cancelled"))
}

type Task struct {
	EsSidecarClient es.EsSidecarServiceClient
}

func (t *Task) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	if err := guardRun(task.GetMetadata().EnqueuedAt); err != nil {
		return nil, err
	}

	policies := Policies{}
	err := task.GetParameters(&policies)
	if err != nil {
		return nil, errors.Wrap(err, "could not get purge policy parameters")
	}

	for _, p := range policies.Es {
		err = p.Purge(ctx, t.EsSidecarClient)
		if err != nil {
			return nil, err
		}
	}

	for _, p := range policies.Pg {
		err = p.Purge(ctx)
		if err != nil {
			return nil, err
		}
	}

	return nil, nil
}
