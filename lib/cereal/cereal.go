package cereal

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

	"github.com/chef/automate/lib/cereal/backend"
)

var (
	ErrNoTasks                  = errors.New("no tasks in queue")
	ErrNoWorkflowInstances      = errors.New("no workflow instances in queue")
	ErrWorkflowScheduleExists   = errors.New("workflow schedule already exists")
	ErrWorkflowInstanceExists   = errors.New("workflow instance already exists")
	ErrNoDueWorkflows           = errors.New("no due workflows")
	ErrNoScheduledWorkflows     = errors.New("no workflows are scheduled")
	ErrInvalidSchedule          = errors.New("workflow schedule is not valid")
	ErrWorkflowInstanceNotFound = errors.New("workflow instance not found")
	ErrWorkflowScheduleNotFound = errors.New("workflow schedule not found")
	ErrWorkflowNotComplete      = errors.New("workflow instance is still running")
	ErrTaskLost                 = errors.New("task lost before reporting its status")
)

// Schedule represents a recurring workflow.
// TODO(jaym): we should wrap this in the workflow package and provide a getter
// for the parameters
type Schedule backend.Schedule

func (s *Schedule) GetParameters(out interface{}) error {
	if s.Parameters != nil {
		return json.Unmarshal(s.Parameters, out)
	}
	return nil
}

func (s *Schedule) GetRRule() (*rrule.RRule, error) {
	if s.Recurrence == "" {
		return nil, errors.New("no recurrence data")
	}

	return rrule.StrToRRule(s.Recurrence)
}

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
	switch r.backendResult.Status {
	case backend.TaskStatusFailed:
		return errors.New(r.backendResult.ErrorText)
	case backend.TaskStatusLost:
		return ErrTaskLost
	default:
		return nil
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

type CompleteOpts func(*Decision)

func WithResult(obj interface{}) CompleteOpts {
	return func(d *Decision) {
		d.result = obj
	}
}

// WorkflowInstance is an interface to an object representing a running
// workflow instance. A workflow instance keeps state of a currently
// running workflow, and decides what decisions need to be made
// based and that state. This will be passed to the callback methods
// of WorkflowExecutor. Only one instance of a WorkflowInstance can
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
	// the running workflow instance.
	Complete(...CompleteOpts) Decision

	// Continue returns a decision to continue execution of the workflow for
	// the running workflow instance. The provided payload will available when
	// this workflow instance is processed by a WorkflowExecutor next.
	Continue(payload interface{}) Decision

	// Fail returns a decision to end the execution of the
	// workflow because of an error that occurred when processing
	// the workflow event.
	Fail(error) Decision

	// InstanceName returns the workflow instance name
	InstanceName() string

	// TotalEnqueuedTasks returns the total number of tasks that
	// have been enqueued for the lifetime of the running workflow
	// instance.
	TotalEnqueuedTasks() int

	// TotalCompletedTasks returns the total number of tasks that have finished
	// execution and been seen by the workflow instance.
	TotalCompletedTasks() int
}

type workflowInstanceImpl struct {
	instance backend.WorkflowInstance
	tasks    []backend.Task
	wevt     *backend.WorkflowEvent
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
		Name:       taskName,
		Parameters: paramsData,
	})
	return nil
}

func (w *workflowInstanceImpl) Complete(opts ...CompleteOpts) Decision {
	if len(w.tasks) > 0 {
		logrus.Errorf("cannot call EnqueueTask and Complete in same workflow step! failing workflow")
		return Decision{failed: true, err: errors.New("EnqueueTask and Complete called in same workflow step")}
	}

	d := Decision{
		complete: true,
	}

	for _, o := range opts {
		o(&d)
	}

	return d
}

func (w *workflowInstanceImpl) Continue(payload interface{}) Decision {
	return Decision{
		tasks:      w.tasks,
		continuing: true,
		payload:    payload,
	}
}

func (w *workflowInstanceImpl) Fail(err error) Decision {
	return Decision{
		failed: true,
		err:    err,
	}
}

type ImmutableWorkflowInstance interface {
	// GetPayload unmarshals the payload of the workflow instance into the
	// value pointed at by obj. This payload is any state the user wishes
	// to keep.
	GetPayload(obj interface{}) error

	// GetParameters unmarshals the parameters the workflow instance was
	// started with into the value pointed at by obj.
	GetParameters(obj interface{}) error

	IsRunning() bool

	GetResult(obj interface{}) error

	Err() error
}

type immutableWorkflowInstanceImpl struct {
	instance *backend.WorkflowInstance
}

func (w *immutableWorkflowInstanceImpl) GetPayload(obj interface{}) error {
	if w.instance.Payload != nil && len(w.instance.Payload) > 0 {
		return json.Unmarshal(w.instance.Payload, obj)
	}
	return nil
}

func (w *immutableWorkflowInstanceImpl) GetParameters(obj interface{}) error {
	if w.instance.Parameters != nil && len(w.instance.Parameters) > 0 {
		return json.Unmarshal(w.instance.Parameters, obj)
	}
	return nil
}

func (w *immutableWorkflowInstanceImpl) GetResult(obj interface{}) error {
	if w.IsRunning() {
		return ErrWorkflowNotComplete
	}
	if w.instance.Result != nil && len(w.instance.Result) > 0 {
		return json.Unmarshal(w.instance.Result, obj)
	}
	return nil
}

func (w *immutableWorkflowInstanceImpl) Err() error {
	return w.instance.Err
}

func (w *immutableWorkflowInstanceImpl) IsRunning() bool {
	return w.instance.Status != backend.WorkflowInstanceStatusCompleted
}

// Decision indicates how the execution of a workflow instance is to proceed.
// This struct should not be created by the user. Instead, use the methods
// that return Decision on the WorkflowInstance.
type Decision struct {
	complete   bool
	continuing bool
	failed     bool
	payload    interface{}
	result     interface{}
	err        error
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

// Manager is responsible for for calling WorkflowExecutors and
// TaskExecutors when they need to be processed, along with managing
// the scheduling of workflows.
type Manager struct {
	workflowExecutors map[string]WorkflowExecutor
	taskExecutors     map[string]registeredExecutor
	workflowScheduler *workflowScheduler
	backend           backend.Driver
	cancel            context.CancelFunc
	wg                sync.WaitGroup
}

// NewManager creates a new Manager with the given Driver. If
// the driver fails to initialize, an error is returned.
func NewManager(backend backend.Driver) (*Manager, error) {
	err := backend.Init()
	if err != nil {
		return nil, err
	}
	return &Manager{
		backend:           backend,
		workflowExecutors: make(map[string]WorkflowExecutor),
		taskExecutors:     make(map[string]registeredExecutor),
		workflowScheduler: &workflowScheduler{backend},
	}, nil
}

// RegisterWorkflowExecutor registers a WorkflowExecutor to execute workflows
// of type workflowName. This is not safe to call concurrently and should be
// done from only one thread of the process. This must be called before Start.
func (m *Manager) RegisterWorkflowExecutor(workflowName string,
	workflowExecutor WorkflowExecutor) error {
	m.workflowExecutors[workflowName] = workflowExecutor
	return nil
}

// TaskExecutorOpts are options that describe how a TaskExecutor should run tasks.
type TaskExecutorOpts struct {
	// Timeout is how long to wait before canceling a running task.
	Timeout time.Duration
	// Workers specifies the max concurrently executing tasks the Manager
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
func (m *Manager) RegisterTaskExecutor(taskName string, executor TaskExecutor, opts TaskExecutorOpts) error {
	m.taskExecutors[taskName] = registeredExecutor{
		executor: executor,
		opts:     opts,
	}
	return nil
}

// Start starts the Manager. No workflows, tasks, or schedules will be
// processed before Start is called. This should only be called once.
func (m *Manager) Start(ctx context.Context) error {
	ctx, cancel := context.WithCancel(ctx)
	m.cancel = cancel
	err := m.startTaskExecutors(ctx)
	if err != nil {
		return err
	}

	go m.workflowScheduler.run(ctx)
	go m.runWorkflowExecutor(ctx)
	return nil
}

func (m *Manager) Stop() error {
	if m.cancel != nil {
		m.cancel()
	}

	m.wg.Wait()

	var err error
	if m.backend != nil {
		err = m.backend.Close()
	}

	return err
}

// CreateWorkflowSchedule creates a recurring workflow based on the recurrence
// rule provided. The first run will happen during when the recurrence is first
// due from Now.
func (m *Manager) CreateWorkflowSchedule(
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
func (m *Manager) UpdateWorkflowScheduleByName(ctx context.Context,
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
func (m *Manager) ListWorkflowSchedules(ctx context.Context) ([]*Schedule, error) {
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

func (m *Manager) GetWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string) (*Schedule, error) {
	backendSched, err := m.backend.GetWorkflowScheduleByName(ctx, instanceName, workflowName)
	if err != nil {
		return nil, err
	}

	return (*Schedule)(backendSched), nil
}

func (m *Manager) GetWorkflowInstanceByName(ctx context.Context, instanceName string, workflowName string) (ImmutableWorkflowInstance, error) {
	workflowInstance, err := m.backend.GetWorkflowInstanceByName(ctx, instanceName, workflowName)
	if err != nil {
		return nil, err
	}

	return &immutableWorkflowInstanceImpl{
		instance: workflowInstance,
	}, nil
}

// EnqueueWorkflow enqueues a workflow of type workflowName. Only one instance of
// (workflowName, instanceName) can be running at a time.
func (m *Manager) EnqueueWorkflow(ctx context.Context, workflowName string,
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

func (m *Manager) startTaskExecutors(ctx context.Context) error {
	for taskName, exec := range m.taskExecutors {
		workerCount := exec.opts.Workers
		if workerCount == 0 {
			workerCount = 1
		}

		for i := 0; i < workerCount; i++ {
			go m.runTaskExecutor(ctx, taskName, i, exec.opts.Timeout, exec.executor)
		}
	}
	return nil
}

func (m *Manager) runTaskExecutor(ctx context.Context, taskName string, workerIdx int, timeout time.Duration, exec TaskExecutor) {
	logctx := logrus.WithFields(logrus.Fields{
		"worker_name": fmt.Sprintf("%s/%d", taskName, workerIdx),
	})
	logctx.Info("Starting task executor")
	m.wg.Add(1)

LOOP:
	for {
		select {
		case <-ctx.Done():
			logctx.Info("exiting task executor")
			break LOOP
		default:
		}

		t, taskCompleter, err := m.backend.DequeueTask(ctx, taskName)
		if err != nil {
			if err == ErrNoTasks {
				// TODO(ssd) 2019-05-10: Once we have notifications we can probably sleep longer
				time.Sleep(1 * time.Second)
			} else {
				logctx.WithError(err).Error("failed to dequeue task")
			}
			continue
		}
		logctx.Debugf("Dequeued task %s", t.Name)

		runCtx := taskCompleter.Context()
		var cancel context.CancelFunc
		if timeout > 0 {
			runCtx, cancel = context.WithTimeout(runCtx, timeout)
		} else {
			runCtx, cancel = context.WithCancel(runCtx)
		}
		runTask(runCtx, logctx, exec, t, taskCompleter) // nolint: errcheck
		cancel()
	}
	m.wg.Done()
}

func runTask(ctx context.Context, logctx logrus.FieldLogger, exec TaskExecutor, t *backend.Task, taskCompleter backend.TaskCompleter) error {
	result, err := exec.Run(ctx, &task{backendTask: t})
	if err != nil {
		err := taskCompleter.Fail(err.Error())
		if err != nil {
			logctx.WithError(err).Error("failed to mark task as failed")
			return err
		}
	} else {
		jsonResults, err := jsonify(result)
		if err != nil {
			logctx.WithError(err).Error("could not convert returned results to JSON")
			if err := taskCompleter.Fail(err.Error()); err != nil {
				logrus.WithError(err).Error("Failed to fail task completer")
				return err
			}
		}
		err = taskCompleter.Succeed(jsonResults)
		if err != nil {
			logrus.WithError(err).Error("failed to mark task as successful")
			return err
		}
	}
	return nil
}

func (m *Manager) runWorkflowExecutor(ctx context.Context) {
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

func (m *Manager) processWorkflow(ctx context.Context, workflowNames []string) bool {
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
		instance: wevt.Instance,
		wevt:     wevt,
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
	case backend.WorkflowCancel:
		decision = executor.OnCancel(w, CancelEvent{})
	default:
		panic("WTF")
	}
	s.End("user")
	logctx := logrus.WithFields(logrus.Fields{
		"workflow_name":   wevt.Instance.WorkflowName,
		"instance_name":   wevt.Instance.InstanceName,
		"enqueued_tasks":  wevt.EnqueuedTaskCount,
		"completed_tasks": wevt.CompletedTaskCount,
	})
	if decision.failed {
		s.Begin("failed")
		if wevt.CompletedTaskCount != wevt.EnqueuedTaskCount {
			logctx.WithError(decision.err).Warn("Workflow failed with pending tasks")
		} else {
			logctx.WithError(decision.err).Info("Workflow complete with error")
		}

		err = completer.Fail(decision.err)
		if err != nil {
			logctx.WithError(err).Error("failed to complete workflow")
		}

		s.End("failed")
	} else if decision.complete {
		s.Begin("complete")
		if wevt.CompletedTaskCount != wevt.EnqueuedTaskCount {
			logctx.Warn("Workflow complete with pending tasks")
		} else {
			logctx.Info("Completing workflow")
		}
		jsonResult, err := jsonify(decision.result)
		if err != nil {
			logctx.WithError(err).Error("failed to jsonify workflow result")
			jsonResult = nil
		}

		err = completer.Done(jsonResult)
		if err != nil {
			logctx.WithError(err).Error("failed to complete workflow")
		}
		s.End("complete")
	} else if decision.continuing {
		s.Begin("enqueue_task")
		for _, t := range decision.tasks {
			err := completer.EnqueueTask(&t, backend.TaskEnqueueOpts{})
			if err != nil {
				logrus.WithError(err).Error("failed to enqueue task!")
				return true
			}
		}
		s.End("enqueue_task")
		s.Begin("continue")
		jsonPayload, err := jsonify(decision.payload)
		if err != nil {
			logrus.WithError(err).Error("could not marshal payload to JSON, failing workflow")
			err := completer.Fail(err)
			if err != nil {
				logrus.WithError(err).Error("failed to fail workflow after JSON marshal failure")
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

func (m *Manager) CancelWorkflow(ctx context.Context, workflowName string,
	instanceName string) error {
	return m.backend.CancelWorkflow(ctx, instanceName, workflowName)
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
