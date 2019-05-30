package workflow

import (
	"context"
	"encoding/json"
	"fmt"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"

	"github.com/chef/automate/lib/workflow/backend"
)

var (
	ErrNoTasks                = errors.New("no tasks in queue")
	ErrNoWorkflowInstances    = errors.New("no workflow instances in queue")
	ErrWorkflowScheduleExists = errors.New("workflow schedule already exists")
	ErrWorkflowInstanceExists = errors.New("workflow instance already exists")
	ErrNoDueWorkflows         = errors.New("no due workflows")
	ErrNoScheduledWorkflows   = errors.New("no workflows are scheduled")
	ErrInvalidSchedule        = errors.New("workflow schedule is not valid")
)

// Schedule represents a recurring workflow.
// TODO(jaym): we should wrap this in the workflow package and provide a getter
// for the parameters
type Schedule backend.Schedule

// Task is an interface to an object representing a running Task. This will be
// provided to TaskExecutor implementations when the Run method is called.
type Task interface {
	// GetParameters unmarshals the parameters the task was started with into
	// the value pointed at by obj.
	GetParameters(obj interface{}) error
}

type task struct {
	backendTask *backend.Task
}

func (r *task) GetParameters(obj interface{}) error {
	if r.backendTask.Parameters != nil {
		return json.Unmarshal(r.backendTask.Parameters, obj)
	}
	return nil
}

// TaskResult is an interface to an object representing a completed Task. This
// will be provided to the OnTaskComplete callback method of WorkflowExecutor
// implementations.
type TaskResult interface {
	// GetParameters unmarshals the parameters the task was started with into
	// the value pointed at by obj.

	GetParameters(obj interface{}) error

	// Get unmarshals the result returned by the task into the value pointed at
	// by obj.
	Get(obj interface{}) error

	// Err returns an error if the task returned an error upon completion,
	// otherwise nil. The exact error types will not be preserved.
	Err() error
}

type taskResult struct {
	backendResult *backend.TaskResult
}

func (r *taskResult) Get(obj interface{}) error {
	if r.backendResult.Result != nil {
		return json.Unmarshal(r.backendResult.Result, obj)
	}
	return nil
}

func (r *taskResult) GetParameters(obj interface{}) error {
	if r.backendResult.Parameters != nil {
		return json.Unmarshal(r.backendResult.Parameters, obj)
	}
	return nil
}

func (r *taskResult) Err() error {
	if r.backendResult.Status == backend.TaskStatusFailed {
		return errors.New(r.backendResult.ErrorText)
	}
	return nil
}

// WithRetries sets the number of automatic retries allowed for a task
func WithRetries(numRetries int) EnqueueOpts {
	return func(o *backend.TaskEnqueueOpts) {
		o.TryRemaining = numRetries + 1
	}
}

// StartAfter indicates when the task should start running
func StartAfter(startAfter time.Time) EnqueueOpts {
	return func(o *backend.TaskEnqueueOpts) {
		o.StartAfter = startAfter
	}
}

// EnqueueOpts are optional parameters for enqueuing a task
type EnqueueOpts func(*backend.TaskEnqueueOpts)

// WorkflowInstance is an interface to an object representing a running
// workflow instance. A workflow instance keeps state of a currently
// running workflow, and decides what decisions need to be made
// based and that state. This will be passed to the callback methods
// of WorkflowExecutor. Only once instance of a WorkflowInstance can
// execute at a time.
type WorkflowInstance interface {
	// GetPayload unmarshals the payload of the workflow instance into the
	// value pointed at by obj. This payload is any state the user wishes
	// to keep.
	GetPayload(obj interface{}) error

	// GetParameters unmarshals the parameters the workflow instance was
	// started with into the value pointed at by obj.
	GetParameters(obj interface{}) error

	// EnqueueTask requests that a task of the type taskName be started
	// with the given parameters. Any enqueued tasks will be started
	// after the currently running callback of the WorkflowExecutor
	// returns.
	// TODO (jaym): Allow passing enqueue options
	EnqueueTask(taskName string, parameters interface{}) error

	// Complete returns a decision to end execution of the workflow for
	// the running workflow instance. If there are any uncompleted tasks,
	// the workflow instance will be considered abandoned. Abandoned
	// workflows will not complete until all running tasks have been
	// completed. All tasks which are queued will be dequeued.
	Complete() Decision

	// Continue returns a decision to continue execution of the workflow for
	// the running workflow instance. The provided payload will available when
	// this workflow instance is processed by a WorkflowExecutor next.
	Continue(payload interface{}) Decision

	// InstanceName returns the workflow instance name
	InstanceName() string

	// TotalEnqueuedTasks returns the total number of tasks that been enqueued
	// for the lifetime of the running workflow instance.
	TotalEnqueuedTasks() int

	// TotalCompletedTasks returns the total number of tasks that have finished
	// execution and been seen by the workflow instance.
	TotalCompletedTasks() int
}

type workflowInstanceImpl struct {
	instanceID int64
	instance   backend.WorkflowInstance
	tasks      []backend.Task
	wevt       *backend.WorkflowEvent
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

func (w *workflowInstanceImpl) InstanceName() string {
	return w.instance.InstanceName
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
	w.tasks = append(w.tasks, backend.Task{
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

// Decision indicates how the execution of a workflow instance is to proceed.
// This struct should not be created by the user. Instead, use the methods
// that return Decision on the WorkflowInstance.
type Decision struct {
	complete   bool
	continuing bool
	payload    interface{}
	tasks      []backend.Task
}

// StartEvent is passed to the OnStart callback of the WorkflowExecutor when
// a workflow instance is signaled to be started.
type StartEvent struct{}

// TaskCompleteEvent is passed to the OnTaskComplete callback of the
// WorkflowExecutor when a task for a workflow instance completes.
type TaskCompleteEvent struct {
	// TaskName is the type of the task that completed
	TaskName string

	// Result contains information representing the completion of the
	// task such as if it errored or returned a value.
	Result TaskResult
}

// CancelEvent is passed to the OnCancel callback of the WorkflowExecutor
// when the workflow is signaled for cancelation.
type CancelEvent struct{}

// WorkflowExecutor is the interface implemented by objects that can process
// a workflow of a certain type.
type WorkflowExecutor interface {
	// OnStart is called when a StartEvent for this type of workflow is to
	// be processed.
	OnStart(w WorkflowInstance, ev StartEvent) Decision

	// OnTaskComplete is called when a TaskCompleteEvent for this type of
	// workflow is to be processed. This event will never be received before
	// OnStart for the given WorkflowInstance.
	OnTaskComplete(w WorkflowInstance, ev TaskCompleteEvent) Decision

	// OnCancel is called when a workflow instance is to be canceled.
	//
	// BUG(jaym): It's currently possible to receive a CancelEvent before
	// OnStart for the postgres implementation.
	OnCancel(w WorkflowInstance, ev CancelEvent) Decision
}

// TaskExecutor is the interface implemented by objects that can run tasks
// of a certain type.
// TODO(ssd) 2019-05-10: How do we want to handle cancellation?
type TaskExecutor interface {
	// Run implements the logic for running the task. The returned result/err
	// will be provided to the workflow instance that started this task on
	// completion. This method should strive to be retryable / idempotent
	// as there is a possibility that it can run multiple times.
	Run(ctx context.Context, task Task) (result interface{}, err error)
}

// WorkflowManager is responsible for for calling WorkflowExecutors and
// TaskExecutors when they need to be processed, along with managing
// the scheduling of workflows.
type WorkflowManager struct {
	workflowExecutors map[string]WorkflowExecutor
	taskExecutors     map[string]registeredExecutor
	workflowScheduler *workflowScheduler
	backend           backend.Driver
	cancel            context.CancelFunc
	wg                sync.WaitGroup
}

// NewManager creates a new WorkflowManager with the given Driver. If
// the driver fails to initialize, an error is returned.
func NewManager(backend backend.Driver) (*WorkflowManager, error) {
	err := backend.Init()
	if err != nil {
		return nil, err
	}
	return &WorkflowManager{
		backend:           backend,
		workflowExecutors: make(map[string]WorkflowExecutor),
		taskExecutors:     make(map[string]registeredExecutor),
		workflowScheduler: &workflowScheduler{backend},
	}, nil
}

// RegisterWorkflowExecutor registers a WorkflowExecutor to execute workflows
// of type workflowName. This is not safe to call concurrently and should be
// done from only one thread of the process. This must be called before Start.
func (m *WorkflowManager) RegisterWorkflowExecutor(workflowName string,
	workflowExecutor WorkflowExecutor) error {
	m.workflowExecutors[workflowName] = workflowExecutor
	return nil
}

// TaskExecutorOpts are options that describe how a TaskExecutor should run tasks.
type TaskExecutorOpts struct {
	// Timeout is how long to wait before canceling a running task.
	Timeout time.Duration
	// Workers specifies the max concurrently executing tasks the WorkflowManager
	// will launch for the registered TaskExecutor.
	Workers int
}

type registeredExecutor struct {
	executor TaskExecutor
	opts     TaskExecutorOpts
}

// RegisterTaskExecutor registers a TaskExecutor to execute tasks of type taskName.
// This is not safe to call concurrently and should be done from only one thread
// of the process. This must be called before Start.
func (m *WorkflowManager) RegisterTaskExecutor(taskName string, executor TaskExecutor, opts TaskExecutorOpts) error {
	m.taskExecutors[taskName] = registeredExecutor{
		executor: executor,
		opts:     opts,
	}
	return nil
}

// Start starts the WorkflowManager. No workflows, tasks, or schedules will be
// processed before Start is called. This should only be called once.
func (m *WorkflowManager) Start(ctx context.Context) error {
	ctx, cancel := context.WithCancel(ctx)
	m.cancel = cancel
	m.startTaskExecutors(ctx)
	go m.workflowScheduler.run(ctx)
	go m.runWorkflowExecutor(ctx)
	return nil
}

func (m *WorkflowManager) Stop() error {
	m.cancel()
	m.wg.Wait()
	return nil
}

// CreateWorkflowSchedule creates a recurring workflow based on the recurrence
// rule provided. The first run will happen during when the recurrence is first
// due from Now.
func (m *WorkflowManager) CreateWorkflowSchedule(
	instanceName string,
	workflowName string,
	parameters interface{},
	enabled bool,
	recurRule *rrule.RRule,
) error {
	nextRunAt := recurRule.After(time.Now().UTC(), true).UTC()
	if nextRunAt.IsZero() {
		return ErrInvalidSchedule
	}
	jsonData, err := jsonify(parameters)
	if err != nil {
		return err
	}
	return m.backend.CreateWorkflowSchedule(context.TODO(), instanceName, workflowName,
		jsonData, enabled, recurRule.String(), nextRunAt)
}

// WorkflowScheduleUpdateOpts represents changes that can be made to a scheduled
// workflow. The changes will be committed atomically.
type WorkflowScheduleUpdateOpts func(*backend.WorkflowScheduleUpdateOpts) error

// UpdateEnabled allows enabling or disabling a scheduled workflow.
func UpdateEnabled(enabled bool) WorkflowScheduleUpdateOpts {
	return func(o *backend.WorkflowScheduleUpdateOpts) error {
		o.Enabled = enabled
		o.UpdateEnabled = true
		return nil
	}
}

// UpdateParameters allows changing the parameters a workflow will be started
// with.
func UpdateParameters(parameters interface{}) WorkflowScheduleUpdateOpts {
	return func(o *backend.WorkflowScheduleUpdateOpts) error {
		paramsData, err := jsonify(parameters)
		o.UpdateParameters = true
		o.Parameters = paramsData
		return err
	}
}

// UpdateRecurrence changes the recurrence rule for the scheduled workflow. The
// next run will happen when the recurrence is first due from Now.
func UpdateRecurrence(recurRule *rrule.RRule) WorkflowScheduleUpdateOpts {
	return func(o *backend.WorkflowScheduleUpdateOpts) error {
		o.UpdateRecurrence = true
		o.Recurrence = recurRule.String()
		o.NextRunAt = recurRule.After(time.Now().UTC(), true).UTC()
		return nil
	}
}

// UpdateWorkflowScheduleByName updates the scheduled workflow identified by
// (instanceName, workflowName).
func (m *WorkflowManager) UpdateWorkflowScheduleByName(ctx context.Context,
	instanceName string, workflowName string, opts ...WorkflowScheduleUpdateOpts) error {

	o := backend.WorkflowScheduleUpdateOpts{}
	for _, opt := range opts {
		err := opt(&o)
		if err != nil {
			return err
		}
	}
	return m.backend.UpdateWorkflowScheduleByName(ctx, instanceName, workflowName, o)
}

// ListWorkflowSchedules list all the scheduled workflows.
func (m *WorkflowManager) ListWorkflowSchedules(ctx context.Context) ([]*Schedule, error) {
	backendScheds, err := m.backend.ListWorkflowSchedules(ctx)
	if err != nil {
		return nil, err
	}
	ret := make([]*Schedule, len(backendScheds))
	for i, s := range backendScheds {
		ret[i] = (*Schedule)(s)
	}
	return ret, nil
}

// GetScheduledWorkflowParameters returns the parameters that the scheduled workflow
// identified by (instanceName, workflowName) will be started with.
func (m *WorkflowManager) GetScheduledWorkflowParameters(ctx context.Context, instanceName string, workflowName string, out interface{}) error {
	data, err := m.backend.GetScheduledWorkflowParameters(ctx, instanceName, workflowName)
	if err != nil {
		return errors.Wrap(err, "could not retrieve parameters for workflow")
	}

	if data != nil {
		return json.Unmarshal(data, out)
	}

	return nil
}

// GetScheduledWorkflowRecurrence returns the recurrence rule for the scheduled workflow
// identified by (instanceName, workflowName)
func (m *WorkflowManager) GetScheduledWorkflowRecurrence(ctx context.Context, instanceName string, workflowName string) (*rrule.RRule, error) {
	ruleStr, err := m.backend.GetScheduledWorkflowRecurrence(ctx, instanceName, workflowName)
	if err != nil {
		return nil, errors.Wrap(err, "could not retrieve parameters for workflow")
	}

	if ruleStr == "" {
		return nil, errors.New("no recurrence data")
	}

	return rrule.StrToRRule(ruleStr)
}

// EnqueueWorkflow enqueues a workflow of type workflowName. Only one instance of
// (workflowName, instanceName) can be running at a time.
func (m *WorkflowManager) EnqueueWorkflow(ctx context.Context, workflowName string,
	instanceName string, parameters interface{}) error {
	paramsData, err := jsonify(parameters)
	if err != nil {
		return err
	}
	err = m.backend.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		WorkflowName: workflowName,
		InstanceName: instanceName,
		Parameters:   paramsData,
	})
	return err
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
// TODO(jaym): should this be private?
func (m *WorkflowManager) RunTaskExecutor(ctx context.Context, taskName string, workerID int, timeout time.Duration, exec TaskExecutor) {
	workerName := fmt.Sprintf("%s/%d", taskName, workerID)
	logrus.Infof("starting task executor %s", workerName)

	m.wg.Add(1)

LOOP:
	for {
		select {
		case <-ctx.Done():
			logrus.Infof("exiting task executor %s", workerName)
			break LOOP
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
		logrus.Debugf("Dequeued task %s", t.Name)

		var runCtx context.Context
		var cancel context.CancelFunc
		if timeout > 0 {
			runCtx, cancel = context.WithTimeout(ctx, timeout)
		} else {
			runCtx, cancel = context.WithCancel(ctx)
		}

		result, err := exec.Run(runCtx, &task{backendTask: t})
		if err != nil {
			err := taskCompleter.Fail(err.Error())
			if err != nil {
				logrus.WithError(err).Error("failed to mark task as failed")
			}
		} else {
			jsonResults, err := jsonify(result)
			if err != nil {
				logrus.WithError(err).Error("could not convert returned results to JSON")
				if err := taskCompleter.Fail(err.Error()); err != nil {
					logrus.WithError(err).Error("Failed to fail task completer")
				}
			}
			err = taskCompleter.Succeed(jsonResults)
			if err != nil {
				logrus.WithError(err).Error("failed to mark task as successful")
			}
		}

		cancel()
	}
	m.wg.Done()
}

func (m *WorkflowManager) runWorkflowExecutor(ctx context.Context) {
	workflowNames := make([]string, 0, len(m.workflowExecutors))
	for k := range m.workflowExecutors {
		workflowNames = append(workflowNames, k)
	}
	m.wg.Add(1)
LOOP:
	for {
		select {
		case <-ctx.Done():
			logrus.Info("exiting workflow executor")
			break LOOP
		case <-time.After(2 * time.Second):
			for {
				if m.processWorkflow(ctx, workflowNames) {
					break
				}
			}
		}
	}
	m.wg.Done()
}

func (m *WorkflowManager) processWorkflow(ctx context.Context, workflowNames []string) bool {
	m.wg.Add(1)
	defer m.wg.Done()

	s := newStatsInfo()
	s.Begin("dequeue")
	wevt, completer, err := m.backend.DequeueWorkflow(ctx, workflowNames)
	if err != nil {
		if err != ErrNoWorkflowInstances {
			logrus.WithError(err).Error("failed to dequeue workflow!")
		}
		return true
	}
	s.End("dequeue")
	defer completer.Close() // nolint: errcheck

	logrus.WithFields(logrus.Fields{
		"status": wevt.Instance.Status,
	}).Debug("Dequeued Workflow")

	w := &workflowInstanceImpl{
		instanceID: wevt.InstanceID,
		instance:   wevt.Instance,
		wevt:       wevt,
	}
	if wevt.Instance.Status == backend.WorkflowInstanceStatusAbandoned {
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
	s.Begin("user")
	executor, ok := m.workflowExecutors[wevt.Instance.WorkflowName]
	if !ok {
		logrus.Errorf("No workflow executor for %s", wevt.Instance.WorkflowName)
		return true
	}

	decision := Decision{}
	switch wevt.Type {
	case backend.WorkflowStart:
		decision = executor.OnStart(w, StartEvent{})
	case backend.TaskComplete:
		decision = executor.OnTaskComplete(w, TaskCompleteEvent{
			TaskName: wevt.TaskResult.TaskName,
			Result: &taskResult{
				backendResult: wevt.TaskResult,
			},
		})
	case backend.Cancel:
		decision = executor.OnCancel(w, CancelEvent{})
	default:
		panic("WTF")
	}
	s.End("user")

	if decision.complete {
		s.Begin("complete")
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
		s.End("complete")
	} else if decision.continuing {
		s.Begin("enqueue_task")
		for _, t := range decision.tasks {
			err := completer.EnqueueTask(&t, backend.TaskEnqueueOpts{})
			if err != nil {
				// TODO(ssd) 2019-05-15: What do we
				// want to do here? I think we need to
				// rollback and have the workflow run
				// again.
				logrus.WithError(err).Error("failed to enqueue task!")
			}
		}
		s.End("enqueue_task")
		s.Begin("continue")
		jsonPayload, err := jsonify(decision.payload)
		if err != nil {
			logrus.WithError(err).Error("could not marshal payload to JSON, completing workflow")
			// TODO: We need to fail workflows
			err := completer.Done()
			if err != nil {
				logrus.WithError(err).Error("failed to complete workflow after JSON marshal failure")
			}
		} else {
			err := completer.Continue(jsonPayload)
			if err != nil {
				logrus.WithError(err).Error("failed to continue workflow")
			}
		}
		s.End("continue")
	}
	logrus.Debugf("Processed Workflow: %s", s)

	return false
}

// TODO(ssd) 2019-05-17: Replace me with prometheus
func newStatsInfo() *statsInfo {
	return &statsInfo{
		start:  make(map[string]time.Time),
		totals: make(map[string]time.Duration),
	}
}

type statsInfo struct {
	start  map[string]time.Time
	totals map[string]time.Duration
}

func (s *statsInfo) Begin(label string) {
	_, ok := s.start[label]
	if ok {
		logrus.Warn("statsInfo.Begin on currently running label, ignoring")
		return
	}

	s.start[label] = time.Now()
}

func (s *statsInfo) End(label string) {
	start, ok := s.start[label]
	if !ok {
		logrus.Warn("statsInfo.End on label with no start, ignoring")
		return
	}

	delete(s.start, label)
	t, ok := s.totals[label]
	if ok {
		s.totals[label] = t + time.Since(start)
	} else {
		s.totals[label] = time.Since(start)
	}
}

func (s *statsInfo) String() string {
	keys := make([]string, 0, len(s.totals))
	for k := range s.totals {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	str := &strings.Builder{}
	for _, label := range keys {
		fmt.Fprintf(str, "%s: %f ms; ", label, s.totals[label].Seconds()*1000)
	}
	return str.String()
}

func jsonify(data interface{}) ([]byte, error) {
	if data == nil {
		return nil, nil
	}
	return json.Marshal(data)
}
