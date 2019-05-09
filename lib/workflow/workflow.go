package workflow

import (
	"context"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type FTask struct {
}

type FWorkflowInstance interface {
	GetPayload(interface{}) error
	GetParameters(interface{}) error

	EnqueueTask(taskName string, parameters interface{})
	Complete() Decision
	Continue(payload interface{}) Decision
}

type workflowInstanceImpl struct {
	instanceID int64
	instance   WorkflowInstance
	tasks      []Task
}

func (w *workflowInstanceImpl) GetPayload(interface{}) error {
	return nil
}

func (w *workflowInstanceImpl) GetParameters(interface{}) error {
	return nil
}

func (w *workflowInstanceImpl) EnqueueTask(taskName string, parameters interface{}) {
	w.tasks = append(w.tasks, Task{
		WorkflowInstanceID: w.instanceID,
		Name:               taskName,
		Parameters:         parameters,
	})
}

func (w *workflowInstanceImpl) Complete() Decision {
	if len(w.tasks) > 0 {
		panic("cannot call EnqueueTask and Complete() in same workflow step!")
	}
	return Decision{
		complete: true,
	}
}

func (w *workflowInstanceImpl) Continue(payload interface{}) Decision {
	return Decision{
		tasks:      w.tasks,
		continuing: true,
		payload:    payload,
	}
}

type FTaskResult interface {
	GetParameters(interface{}) error
	Get(interface{}) error
	Err() error
}

type taskResultImpl struct {
	r *TaskResult
}

func (r *taskResultImpl) Err() error {
	if r.r.status == taskStatusFailed {
		return errors.New(r.r.errorText)
	}
	return nil

}

func (r *taskResultImpl) Get(interface{}) error {
	return nil
}

func (r *taskResultImpl) GetParameters(interface{}) error {
	return nil
}

type Decision struct {
	complete   bool
	continuing bool
	payload    interface{}
	tasks      []Task
}

type StartEvent struct{}
type TaskCompleteEvent struct {
	TaskName string
	Result   FTaskResult
}
type CancelEvent struct{}

type FWorkflowExecutor interface {
	OnStart(w FWorkflowInstance, ev StartEvent) Decision
	OnTaskComplete(w FWorkflowInstance, ev TaskCompleteEvent) Decision
	OnCancel(w FWorkflowInstance, ev CancelEvent) Decision
}

type FWorkflowManager struct {
	workflowExecutors map[string]FWorkflowExecutor
	backend           Backend
}

func NewManager(backend Backend) *FWorkflowManager {
	return &FWorkflowManager{backend: backend, workflowExecutors: make(map[string]FWorkflowExecutor)}
}
func (m *FWorkflowManager) RegisterWorkflowExecutor(workflowName string,
	workflowExecutor FWorkflowExecutor) error {
	m.workflowExecutors[workflowName] = workflowExecutor
	return nil
}
func (m *FWorkflowManager) Start(ctx context.Context) error {
	go m.run(ctx)
	return nil
}
func (m *FWorkflowManager) run(ctx context.Context) {
	workflowNames := make([]string, 0, len(m.workflowExecutors))
	for k := range m.workflowExecutors {
		workflowNames = append(workflowNames, k)
	}
	for {
		select {
		case <-ctx.Done():
			panic("DONE")
			return
		case <-time.After(2 * time.Second):
			for {
				m.processWorkflow(ctx, workflowNames)
			}
		}
	}
}

func (m *FWorkflowManager) processWorkflow(ctx context.Context, workflowNames []string) {
	wevt, completer, err := m.backend.DequeueWorkflow(ctx, workflowNames)
	if err != nil {
		if err != ErrNoWorkflowInstances {
			logrus.WithError(err).Error("failed to dequeue workflow!")
		}
		return
	}
	defer completer.Close()
	w := &workflowInstanceImpl{
		instanceID: wevt.InstanceID,
		instance:   wevt.Instance,
	}
	executor, ok := m.workflowExecutors[wevt.Instance.WorkflowName]
	if !ok {
		logrus.Errorf("No workflow executor for %s", wevt.Instance.WorkflowName)
		return
	}

	decision := Decision{}
	switch wevt.Type {
	case WorkflowStart:
		decision = executor.OnStart(w, StartEvent{})
	case TaskComplete:
		decision = executor.OnTaskComplete(w, TaskCompleteEvent{
			Result: &taskResultImpl{r: wevt.TaskResult},
		})
	case Cancel:
		decision = executor.OnCancel(w, CancelEvent{})
	default:
		panic("WTF")
	}

	if decision.complete {
		err := completer.Done()
		if err != nil {
			logrus.WithError(err).Error("failed to complete workflow")
		}
	} else if decision.continuing {
		for _, t := range decision.tasks {
			completer.EnqueueTask(&t)
		}
		completer.Continue(nil)
	}
}
