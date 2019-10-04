package backend

import (
	"time"
)

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

	Err    error
	Result []byte
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

type TaskResult struct {
	TaskName   string
	Parameters []byte

	Status    TaskStatusType
	ErrorText string
	Result    []byte
}

type TaskEnqueueOpts struct {
	StartAfter time.Time
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
