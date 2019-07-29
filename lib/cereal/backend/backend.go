package backend

import (
	"context"
	"time"
)

type Driver interface {
	EnqueueWorkflow(ctx context.Context, workflow *WorkflowInstance) error
	DequeueWorkflow(ctx context.Context, workflowNames []string) (*WorkflowEvent, WorkflowCompleter, error)
	CancelWorkflow(ctx context.Context, instanceName string, workflowName string) error

	DequeueTask(ctx context.Context, taskName string) (*Task, TaskCompleter, error)

	CreateWorkflowSchedule(ctx context.Context, instanceName string, workflowName string, parameters []byte, enabled bool, recurrence string, nextRunAt time.Time) error
	ListWorkflowSchedules(ctx context.Context) ([]*Schedule, error)

	GetWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string) (*Schedule, error)
	UpdateWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string, opts WorkflowScheduleUpdateOpts) error

	GetWorkflowInstanceByName(ctx context.Context, instanceName string, workflowName string) (*WorkflowInstance, error)
	ListWorkflowInstances(ctx context.Context, opts ListWorkflowOpts) ([]*WorkflowInstance, error)

	Init() error
	Close() error
}

type SchedulerDriver interface {
	GetDueScheduledWorkflow(ctx context.Context) (*Schedule, ScheduledWorkflowCompleter, error)
	GetNextScheduledWorkflow(ctx context.Context) (*Schedule, error)
}

type ListWorkflowOpts struct {
	WorkflowName *string
	InstanceName *string
	IsRunning    *bool
}

type WorkflowInstanceStatus string

const (
	WorkflowInstanceStatusStarting  WorkflowInstanceStatus = "starting"
	WorkflowInstanceStatusRunning   WorkflowInstanceStatus = "running"
	WorkflowInstanceStatusCompleted WorkflowInstanceStatus = "completed"
)

type WorkflowInstance struct {
	InstanceName string
	WorkflowName string
	Status       WorkflowInstanceStatus
	Parameters   []byte
	Payload      []byte

	IsRunning bool
	Err       error
	Result    []byte
}

type WorkflowEventType string

const (
	WorkflowStart  WorkflowEventType = "start"
	TaskComplete   WorkflowEventType = "task_complete"
	WorkflowCancel WorkflowEventType = "cancel"
)

type TaskStatusType string

const (
	TaskStatusSuccess        TaskStatusType = "success"
	TaskStatusFailed         TaskStatusType = "failed"
	TaskStatusLost           TaskStatusType = "lost"
	TaskStatusUnusableResult TaskStatusType = "unusable_result"
)

type WorkflowEvent struct {
	Instance           WorkflowInstance
	Type               WorkflowEventType
	EnqueuedTaskCount  int
	CompletedTaskCount int

	TaskResult *TaskResult
}

type Task struct {
	Name       string
	Parameters []byte
}

type TaskResult struct {
	TaskName   string
	Parameters []byte

	Status    TaskStatusType
	ErrorText string
	Result    []byte
}

type TaskCompleter interface {
	Context() context.Context
	Fail(err string) error
	Succeed(result []byte) error
}

type TaskEnqueueOpts struct {
	StartAfter time.Time
}

type WorkflowCompleter interface {
	EnqueueTask(task *Task, opts TaskEnqueueOpts) error

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

type Schedule struct {
	// TODO(ssd) 2019-07-19: ID was originally placed on backends
	// because it was unclear whether (workflow_name,
	// instance_name) can actually be unique in the case of
	// scheduled workflows. We currently have a unique constraint
	// on them, so we could remove this, but since it was on this
	// struct it ended up in a few queries that would need to
	// change.
	ID             int64
	Enabled        bool
	InstanceName   string
	WorkflowName   string
	Parameters     []byte
	Recurrence     string
	NextDueAt      time.Time
	LastEnqueuedAt time.Time

	// These come from the latest result
	LastStart *time.Time
	LastEnd   *time.Time
}

type WorkflowScheduleUpdateOpts struct {
	UpdateEnabled bool
	Enabled       bool

	UpdateParameters bool
	Parameters       []byte

	UpdateRecurrence bool
	Recurrence       string
	NextRunAt        time.Time
}
