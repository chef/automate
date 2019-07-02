package backend

import (
	"context"
	"time"

	"github.com/chef/automate/lib/uuid4"
)

type Driver interface {
	EnqueueWorkflow(ctx context.Context, workflow *WorkflowInstance) error
	DequeueWorkflow(ctx context.Context, workflowNames []string) (*WorkflowEvent, WorkflowCompleter, error)

	DequeueTask(ctx context.Context, workerID uuid4.UUID, taskName string) (*Task, TaskCompleter, error)

	CreateWorkflowSchedule(ctx context.Context, instanceName string, workflowName string, parameters []byte, enabled bool, recurrence string, nextRunAt time.Time) error
	GetDueScheduledWorkflow(ctx context.Context) (*Schedule, ScheduledWorkflowCompleter, error)
	GetNextScheduledWorkflow(ctx context.Context) (*Schedule, error)
	UpdateWorkflowScheduleByID(ctx context.Context, id int64, opts WorkflowScheduleUpdateOpts) error
	UpdateWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string, opts WorkflowScheduleUpdateOpts) error

	GetWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string) (*Schedule, error)
	ListWorkflowSchedules(ctx context.Context) ([]*Schedule, error)

	GetWorkflowInstanceByName(ctx context.Context, instanceName string, workflowName string) (*WorkflowInstance, error)

	Init() error
	Close() error
}

type WorkflowInstanceStatus string

const (
	WorkflowInstanceStatusRunning   WorkflowInstanceStatus = "running"
	WorkflowInstanceStatusAbandoned WorkflowInstanceStatus = "abandoned"
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
	Cancel         WorkflowEventType = "cancel"
	TasksAbandoned WorkflowEventType = "tasks_abandoned"
)

type TaskStatusType string

const (
	TaskStatusSuccess    TaskStatusType = "success"
	TaskStatusFailed     TaskStatusType = "failed"
	TaskStatusWorkerLost TaskStatusType = "worker_lost"
)

type WorkflowEvent struct {
	InstanceID         int64
	Instance           WorkflowInstance
	Type               WorkflowEventType
	EnqueuedTaskCount  int
	CompletedTaskCount int

	TaskResult *TaskResult
}

type Task struct {
	Name               string
	WorkflowInstanceID int64
	Parameters         []byte
}

type TaskResult struct {
	TaskName   string
	Parameters []byte

	Status    TaskStatusType
	ErrorText string
	Result    []byte
}

type TaskCompleter interface {
	Fail(err string) error
	Succeed(result []byte) error
}

type TaskEnqueueOpts struct {
	StartAfter time.Time
}

type WorkflowCompleter interface {
	EnqueueTask(task *Task, opts TaskEnqueueOpts) error

	Continue(payload []byte) error
	Abandon() error
	Fail(err error) error
	Done(result []byte) error
	Close() error
}

type ScheduledWorkflowCompleter interface {
	EnqueueScheduledWorkflow(s *Schedule) error
	Close()
}

type Schedule struct {
	// NOTE(ssd) 2019-05-13: Since name and workflow-name are
	// user-controlled in the case of many scheduled workflows, we
	// need the ID to create unique workflow names.
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
