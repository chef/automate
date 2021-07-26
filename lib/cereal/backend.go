package cereal

import (
	"context"
	"time"
)

type Driver interface {
	TaskDequeuer

	EnqueueWorkflow(ctx context.Context, workflow *WorkflowInstanceData) error
	DequeueWorkflow(ctx context.Context, workflowNames []string) (*WorkflowEvent, WorkflowCompleter, error)
	CancelWorkflow(ctx context.Context, instanceName string, workflowName string) error
	KillWorkflow(ctx context.Context, instanceName string, workflowName string) error

	CreateWorkflowSchedule(ctx context.Context, instanceName string, workflowName string, parameters []byte, enabled bool, recurrence string, nextRunAt time.Time) error
	ListWorkflowSchedules(ctx context.Context) ([]*Schedule, error)

	GetWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string) (*Schedule, error)
	UpdateWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string, opts WorkflowScheduleUpdateOptions) error

	GetWorkflowInstanceByName(ctx context.Context, instanceName string, workflowName string) (*WorkflowInstanceData, error)
	ListWorkflowInstances(ctx context.Context, opts ListWorkflowOpts) ([]*WorkflowInstanceData, error)

	Init() error
	Close() error
}

type TaskDequeuer interface {
	DequeueTask(ctx context.Context, taskName string) (*TaskData, TaskCompleter, error)
}

type SchedulerDriver interface {
	GetDueScheduledWorkflow(ctx context.Context) (*Schedule, ScheduledWorkflowCompleter, error)
	GetNextScheduledWorkflow(ctx context.Context) (*Schedule, error)
}

// IntervalSuggester is an interface that backends can optionally
// implement to suggest a default TaskPollInterval and
// WorkflowPollInterval to the rest of the cereal library.
type IntervalSuggester interface {
	DefaultTaskPollInterval() time.Duration
	DefaultWorkflowPollInterval() time.Duration
}

type TaskCompleter interface {
	Context() context.Context
	Fail(err string) error
	Succeed(result []byte) error
}

type WorkflowCompleter interface {
	EnqueueTask(task *TaskData, opts TaskEnqueueOptions) error

	Continue(payload []byte) error
	Fail(err error) error
	Done(result []byte) error
	Close() error
}

type ScheduledWorkflowCompleter interface {
	EnqueueAndUpdateScheduledWorkflow(s *Schedule) error
	DisableSchedule(s *Schedule) error
	Close()
}
