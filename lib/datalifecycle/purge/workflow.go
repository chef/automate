package purge

import (
	"context"

	"github.com/pkg/errors"
	"github.com/teambition/rrule-go"

	es "github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/lib/cereal"
)

// ConfigureManager registers the purge workflow schedule and job with purge
// task and workflow executors.
func ConfigureManager(man *cereal.Manager, scheduleName string, jobName string, opts ...TaskOpt) error {
	task := &Task{}
	for _, o := range opts {
		o(task)
	}

	err := man.RegisterTaskExecutor(jobName, task, cereal.TaskExecutorOpts{})
	if err != nil {
		return err
	}
	err = man.RegisterWorkflowExecutor(scheduleName, &Workflow{})
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
	jobName string,
	defaultPolicies *Policies,
	enabled bool,
	recurrence *rrule.RRule) error {

	err := man.CreateWorkflowSchedule(
		ctx,
		scheduleName,
		jobName,
		defaultPolicies,
		true,
		recurrence,
	)

	if err == cereal.ErrWorkflowScheduleExists {
		sched, err := man.GetWorkflowScheduleByName(ctx, scheduleName, jobName)
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
			jobName,
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

func WithTaskEsSidecarClient(client es.EsSidecarClient) TaskOpt {
	return func(t *Task) {
		t.EsSidecarClient = client
	}
}

type Workflow struct{}

func (s *Workflow) OnStart(w cereal.WorkflowInstance, _ cereal.StartEvent) cereal.Decision {
	policies := Policies{}
	err := w.GetParameters(&policies)
	if err != nil {
		return w.Fail(err)
	}

	for _, policy := range policies.Es {
		err = w.EnqueueTask(policy.Name, policy)
		if err != nil {
			w.Fail(err)
		}
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
	EsSidecarClient es.EsSidecarClient
}

func (t *Task) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
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
