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

type WorkflowScheduleUpdateOpts struct {
	UpdateEnabled bool
	Enabled       bool

	UpdateParameters bool
	Parameters       []byte

	UpdateRecurrence bool
	Recurrence       string
	NextRunAt        time.Time
}
