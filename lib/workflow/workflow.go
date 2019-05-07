package workflow

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"
)

type TaskQuerier interface {
	GetParameters(interface{}) error
}

type TaskResultQuerier interface {
	GetParameters(interface{}) error
	Get(interface{}) error
	Err() error
}

type WorkflowInstanceHandler interface {
	GetPayload(interface{}) error
	GetParameters(interface{}) error

	EnqueueTask(taskName string, parameters interface{}) error
	Complete() Decision
	Continue(payload interface{}) Decision

	TotalEnqueuedTasks() int
	TotalCompletedTasks() int
}

type workflowInstanceImpl struct {
	instanceID int64
	instance   WorkflowInstance
	tasks      []Task
	wevt       *WorkflowEvent
}

func (w *workflowInstanceImpl) GetPayload(obj interface{}) error {
	if w.instance.Payload != nil && len(w.instance.Payload) > 0 {
		return json.Unmarshal(w.instance.Payload, obj)
	}
	return nil
}

func (w *workflowInstanceImpl) GetParameters(obj interface{}) error {
	if w.instance.Parameters != nil {
		return json.Unmarshal(w.instance.Parameters, obj)
	}
	return nil
}

func (w *workflowInstanceImpl) TotalEnqueuedTasks() int {
	return len(w.tasks) + w.wevt.EnqueuedTaskCount
}

func (w *workflowInstanceImpl) TotalCompletedTasks() int {
	return w.wevt.CompletedTaskCount
}

func (w *workflowInstanceImpl) EnqueueTask(taskName string, parameters interface{}) error {
	paramsData, err := jsonify(parameters)
	if err != nil {
		return err
	}
	w.tasks = append(w.tasks, Task{
		WorkflowInstanceID: w.instanceID,
		Name:               taskName,
		Parameters:         paramsData,
	})
	return nil
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

type Decision struct {
	complete   bool
	continuing bool
	payload    interface{}
	tasks      []Task
}

type StartEvent struct{}
type TaskCompleteEvent struct {
	TaskName string
	Result   TaskResultQuerier
}
type CancelEvent struct{}

type WorkflowExecutor interface {
	OnStart(w WorkflowInstanceHandler, ev StartEvent) Decision
	OnTaskComplete(w WorkflowInstanceHandler, ev TaskCompleteEvent) Decision
	OnCancel(w WorkflowInstanceHandler, ev CancelEvent) Decision
}

// TODO(ssd) 2019-05-10: How do we want to handle cancellation?
type TaskExecutor interface {
	Run(context.Context, TaskQuerier) (interface{}, error)
}

type WorkflowManager struct {
	workflowExecutors map[string]WorkflowExecutor
	taskExecutors     map[string]registeredExecutor
	workflowScheduler *workflowScheduler
	backend           Backend
}

func NewManager(backend Backend) *WorkflowManager {
	return &WorkflowManager{
		backend:           backend,
		workflowExecutors: make(map[string]WorkflowExecutor),
		taskExecutors:     make(map[string]registeredExecutor),
		workflowScheduler: &workflowScheduler{backend},
	}
}

func (m *WorkflowManager) CreateWorkflowSchedule(
	scheduleName string,
	workflowName string,
	parameters interface{},
	enabled bool,
	recurrence string,
) error {
	recurRule, err := rrule.StrToRRule(recurrence)
	if err != nil {
		return errors.Wrap(err, "invalid recurrence rule")
	}

	nextRunAt := recurRule.After(time.Now().UTC(), true).UTC()
	return m.backend.CreateWorkflowSchedule(context.TODO(), scheduleName, workflowName, parameters, enabled, recurrence, nextRunAt)
}

func (m *WorkflowManager) RegisterWorkflowExecutor(workflowName string,
	workflowExecutor WorkflowExecutor) error {
	m.workflowExecutors[workflowName] = workflowExecutor
	return nil
}

type TaskExecutorOpts struct {
	Timeout time.Duration
	Workers int
}

type registeredExecutor struct {
	executor TaskExecutor
	opts     TaskExecutorOpts
}

func (m *WorkflowManager) RegisterTaskExecutor(taskName string, executor TaskExecutor, opts TaskExecutorOpts) error {
	m.taskExecutors[taskName] = registeredExecutor{
		executor: executor,
		opts:     opts,
	}
	return nil
}

func (m *WorkflowManager) EnqueueWorkflow(ctx context.Context, workflowName string,
	instanceName string, parameters interface{}) error {
	paramsData, err := jsonify(parameters)
	if err != nil {
		return err
	}
	err = m.backend.EnqueueWorkflow(ctx, &WorkflowInstance{
		WorkflowName: workflowName,
		InstanceName: instanceName,
		Parameters:   paramsData,
	})
	return err
}

func (m *WorkflowManager) Start(ctx context.Context) error {
	go m.startTaskExecutors(ctx)
	go m.workflowScheduler.run(ctx)
	go m.run(ctx)
	return nil
}

func (m *WorkflowManager) startTaskExecutors(ctx context.Context) {
	for taskName, exec := range m.taskExecutors {
		workerCount := exec.opts.Workers
		if workerCount == 0 {
			workerCount = 1
		}

		for i := 0; i < workerCount; i++ {
			go m.RunTaskExecutor(ctx, taskName, i, exec.opts.Timeout, exec.executor)
		}
	}
}

// TODO(ssd) 2019-05-10: Why does Task need the WorkflowInstanceID?
func (m *WorkflowManager) RunTaskExecutor(ctx context.Context, taskName string, workerID int, timeout time.Duration, exec TaskExecutor) {
	workerName := fmt.Sprintf("%s/%d", taskName, workerID)
	logrus.Infof("starting task executor %s", workerName)

	for {
		select {
		case <-ctx.Done():
			logrus.Infof("exiting task executor %s", workerName)
		default:
		}

		t, taskCompleter, err := m.backend.DequeueTask(ctx, taskName)
		if err != nil {
			if err == ErrNoTasks {
				// TODO(ssd) 2019-05-10: Once we have notifications we can probably sleep longer
				time.Sleep(1 * time.Second)
			} else {
				logrus.WithError(err).Error("failed to dequeue task")
			}
			continue
		}
		logrus.Infof("Dequeued task %s", t.Name)

		var runCtx context.Context
		var cancel context.CancelFunc
		if timeout > 0 {
			runCtx, cancel = context.WithTimeout(ctx, timeout)
		} else {
			runCtx, cancel = context.WithCancel(ctx)
		}

		result, err := exec.Run(runCtx, t)
		if err != nil {
			err := taskCompleter.Fail(err.Error())
			if err != nil {
				logrus.WithError(err).Error("failed to mark task as failed")
			}
		} else {
			err := taskCompleter.Succeed(result)
			if err != nil {
				logrus.WithError(err).Error("failed to mark task as successful")
			}
		}

		cancel()
	}
}

func (m *WorkflowManager) run(ctx context.Context) {
	workflowNames := make([]string, 0, len(m.workflowExecutors))
	for k := range m.workflowExecutors {
		workflowNames = append(workflowNames, k)
	}
	for {
		select {
		case <-ctx.Done():
			panic("DONE")
		case <-time.After(2 * time.Second):
			for {
				if m.processWorkflow(ctx, workflowNames) {
					break
				}
			}
		}
	}
}

func (m *WorkflowManager) processWorkflow(ctx context.Context, workflowNames []string) bool {
	wevt, completer, err := m.backend.DequeueWorkflow(ctx, workflowNames)
	if err != nil {
		if err != ErrNoWorkflowInstances {
			logrus.WithError(err).Error("failed to dequeue workflow!")
		}
		return true
	}
	defer completer.Close() // nolint: errcheck

	logrus.WithFields(logrus.Fields{
		"status": wevt.Instance.Status,
	}).Info("Dequeued Workflow")

	w := &workflowInstanceImpl{
		instanceID: wevt.InstanceID,
		instance:   wevt.Instance,
		wevt:       wevt,
	}
	if wevt.Instance.Status == WorkflowInstanceStatusAbandoned {
		logrus.Info("Got abandoned workflow")
		if wevt.CompletedTaskCount > wevt.EnqueuedTaskCount {
			// we should never get here.
			// TODO: just delete the workflow instance
			panic("Invalid task count")
		}
		if wevt.CompletedTaskCount == wevt.EnqueuedTaskCount {
			logrus.Info("Completing abandoned workflow")
			if err := completer.Done(); err != nil {
				logrus.WithError(err).Error("failed to complete with abandoned workflow")
				return true
			}
		} else {
			logrus.Info("Continuing abandoned workflow")
			// jaym: Depending on how we support retries, this logic may still wait for
			// an unlocked task to complete when we could have removed it
			if err := completer.Continue(nil); err != nil {
				logrus.WithError(err).Error("failed to continue with abandoned workflow")
				return true
			}
		}

		return false
	}
	executor, ok := m.workflowExecutors[wevt.Instance.WorkflowName]
	if !ok {
		logrus.Errorf("No workflow executor for %s", wevt.Instance.WorkflowName)
		return true
	}

	decision := Decision{}
	switch wevt.Type {
	case WorkflowStart:
		decision = executor.OnStart(w, StartEvent{})
	case TaskComplete:
		decision = executor.OnTaskComplete(w, TaskCompleteEvent{
			TaskName: wevt.TaskResult.taskName,
			Result:   wevt.TaskResult,
		})
	case Cancel:
		decision = executor.OnCancel(w, CancelEvent{})
	default:
		panic("WTF")
	}

	if decision.complete {
		if wevt.CompletedTaskCount != wevt.EnqueuedTaskCount {
			logrus.WithFields(logrus.Fields{
				"enqueued":  wevt.EnqueuedTaskCount,
				"completed": wevt.CompletedTaskCount,
			}).Info("Abandoning workflow")
			if err := completer.Abandon(); err != nil {
				logrus.WithError(err).Error("failed to abandon workflow")
			}
		} else {
			logrus.WithFields(logrus.Fields{
				"enqueued":  wevt.EnqueuedTaskCount,
				"completed": wevt.CompletedTaskCount,
			}).Info("Completing workflow")
			err := completer.Done()
			if err != nil {
				logrus.WithError(err).Error("failed to complete workflow")
			}
		}
	} else if decision.continuing {
		for _, t := range decision.tasks {
			err := completer.EnqueueTask(&t)
			if err != nil {
				// TODO(ssd) 2019-05-15: What do we
				// want to do here? I think we need to
				// rollback and have the workflow run
				// again.
				logrus.WithError(err).Error("failed to enqueue task!")
			}

		}
		err := completer.Continue(decision.payload)
		if err != nil {
			logrus.WithError(err).Error("failed to continue workflow")
		}
	}
	return false
}
