package cereal

import (
	"context"
	"encoding/json"
	"fmt"
	"math/rand"
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

const (
	defaultTaskPollInterval     = 10 * time.Second
	defaultTaskPollMaxJitter    = 1 * time.Second
	defaultWorkflowPollInterval = 10 * time.Second
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
	if r.Err() != nil {
		return r.Err()
	}
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
	case backend.TaskStatusUnusableResult:
		return errors.New(r.backendResult.ErrorText)
	default:
		return nil
	}
}

// StartAfter indicates when the task should start running
func StartAfter(startAfter time.Time) TaskEnqueueOpts {
	return func(o *backend.TaskEnqueueOpts) {
		o.StartAfter = startAfter
	}
}

// EnqueueOpts are optional parameters for enqueuing a task
type TaskEnqueueOpts func(*backend.TaskEnqueueOpts)

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
	EnqueueTask(taskName string, parameters interface{}, opts ...TaskEnqueueOpts) error

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

type enqueueTaskRequest struct {
	backendTask backend.Task
	opts        backend.TaskEnqueueOpts
}
type workflowInstanceImpl struct {
	instance backend.WorkflowInstance
	tasks    []enqueueTaskRequest
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

func (w *workflowInstanceImpl) EnqueueTask(taskName string, parameters interface{}, opts ...TaskEnqueueOpts) error {
	paramsData, err := jsonify(parameters)
	if err != nil {
		return err
	}

	req := enqueueTaskRequest{
		backendTask: backend.Task{
			Name:       taskName,
			Parameters: paramsData,
		},
		opts: backend.TaskEnqueueOpts{},
	}
	for _, o := range opts {
		o(&req.opts)
	}
	w.tasks = append(w.tasks, req)
	return nil
}

func (w *workflowInstanceImpl) Complete(opts ...CompleteOpts) Decision {
	if len(w.tasks) > 0 {
		logrus.Error("Cannot call EnqueueTask and Complete in same workflow step! failing workflow")
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
	if w.instance.Err != nil {
		return w.instance.Err
	}
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
	if w.instance.Err != nil {
		return w.instance.Err
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
	tasks      []enqueueTaskRequest
}

func NewCompleteDecision(result interface{}) Decision {
	return Decision{
		complete: true,
		result:   result,
	}
}

func NewContinueDecision(payload interface{}) Decision {
	return Decision{
		continuing: true,
		payload:    payload,
	}
}

func NewFailDecision(err error) Decision {
	return Decision{
		failed: true,
		err:    err,
	}
}

func (d *Decision) IsComplete() bool {
	return d.complete
}
func (d *Decision) IsContinuing() bool {
	return d.continuing
}
func (d *Decision) IsFailed() bool {
	return d.failed
}
func (d *Decision) Payload() interface{} {
	return d.payload
}
func (d *Decision) Result() interface{} {
	return d.result
}
func (d *Decision) Err() error {
	return d.err
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
// when the workflow is signaled for cancellation.
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

// waywardWorkflowList is a map of wayward workflows to the time they
// entered the map. A wayward workflow is a workflow that has
// experienced some fundamental processing error in the past. To avoid
// stalling the processing of other workflows, we temporarily skip
// events for any wayward workflows.
type waywardWorkflowList map[string]time.Time

// waywardWorkflowTimeout is amount of time we will keep a given
// workflow in our wayward workflow list.
var waywardWorkflowTimeout = 60 * time.Second

func (w waywardWorkflowList) Add(workflowName string) {
	if w == nil {
		w = make(waywardWorkflowList)
	}
	if workflowName != "" {
		logrus.Warnf("Ignoring workflow %q for the next %s", workflowName, waywardWorkflowTimeout)
		w[workflowName] = time.Now()
	}
}

// Filter takes list of workflows and returns a list of all
// non-wayward workflows from the original list.
func (w waywardWorkflowList) Filter(workflowNames []string) []string {
	out := make([]string, 0, len(workflowNames))
	for _, name := range workflowNames {
		if waywardTime, isWayward := w[name]; isWayward {
			if time.Since(waywardTime) > waywardWorkflowTimeout {
				// Carry on my wayward son...
				delete(w, name)
				out = append(out, name)
			}
		} else {
			out = append(out, name)
		}
	}
	return out
}

// A OnWorkflowCompleteCallback is a function that can be called at
// the completion of workflows for debugging and testing purposes. The
// function should not be used for application logic.
type OnWorkflowCompleteCallback func(*backend.WorkflowEvent)

// Manager is responsible for calling WorkflowExecutors and
// TaskExecutors when they need to be processed, along with managing
// the scheduling of workflows.
type Manager struct {
	workflowExecutors map[string]WorkflowExecutor
	taskExecutors     map[string]*registeredExecutor
	waywardWorkflows  waywardWorkflowList
	workflowScheduler *WorkflowScheduler
	backend           backend.Driver
	cancel            context.CancelFunc
	wg                sync.WaitGroup

	taskDequeueWorkers int
	taskDequeuePool    *taskDequeuePool

	workflowWakeupChan   chan struct{}
	taskPollInterval     time.Duration
	workflowPollInterval time.Duration
	taskPollMaxJitter    time.Duration

	onWorkflowCompleteCallback OnWorkflowCompleteCallback
}

// ManagerOpt is an option that can be passed to NewManager.
type ManagerOpt func(*Manager)

// WithTaskPollInterval sets the polling interval for all TaskExecutor
// workers. Each worker will poll the database at least every interval
// for new jobs.
func WithTaskPollInterval(interval time.Duration) ManagerOpt {
	return func(m *Manager) { m.taskPollInterval = interval }
}

// WithWorkflowPollInterval sets the polling interval for the main
// workflow processing loop. The loop will wake up at least once every
// interval to check for new workflow events.
func WithWorkflowPollInterval(interval time.Duration) ManagerOpt {
	return func(m *Manager) { m.workflowPollInterval = interval }
}

// WithTaskPollIntervalMaxJitter is the maximum amount of time before
// the configured interval that we can wake up to prevent TaskPollers
// from always waking up at the same time.
func WithTaskPollIntervalMaxJitter(jitter time.Duration) ManagerOpt {
	return func(m *Manager) { m.taskPollMaxJitter = jitter }
}

// WithTaskDequeueWorkers sets the number of workers that will be used
// to dequeue task. This limits the number of concurrent calls being
// made to the backend.
func WithTaskDequeueWorkers(count int) ManagerOpt {
	return func(m *Manager) { m.taskDequeueWorkers = count }
}

// WithOnWorkflowCompleteCallback sets a OnWOrkflowComplete callback
// that will be called whenever a workflow is finished (i.e. the
// workflow returns a failure or completion decision).  This is
// intended for testing and debugging purposes ONLY.
func WithOnWorkflowCompleteCallback(c OnWorkflowCompleteCallback) ManagerOpt {
	return func(m *Manager) { m.onWorkflowCompleteCallback = c }
}

// NewManager creates a new Manager with the given Driver. If
// the driver fails to initialize, an error is returned.
func NewManager(b backend.Driver, opts ...ManagerOpt) (*Manager, error) {
	err := b.Init()
	if err != nil {
		return nil, err
	}

	workflowWakeupChan := make(chan struct{}, 5) // 5 is arbitrary
	m := &Manager{
		backend:           b,
		waywardWorkflows:  make(waywardWorkflowList),
		workflowExecutors: make(map[string]WorkflowExecutor),
		taskExecutors:     make(map[string]*registeredExecutor),

		taskDequeueWorkers: 2,

		workflowPollInterval: defaultWorkflowPollInterval,
		taskPollInterval:     defaultTaskPollInterval,
		taskPollMaxJitter:    defaultTaskPollMaxJitter,
		workflowWakeupChan:   workflowWakeupChan,
	}

	if v, ok := b.(backend.SchedulerDriver); ok {
		m.workflowScheduler = NewWorkflowScheduler(v, m.WakeupWorkflowExecutor)
	}

	if intervalSuggester, ok := b.(backend.IntervalSuggester); ok {
		m.taskPollInterval = intervalSuggester.DefaultTaskPollInterval()
		m.workflowPollInterval = intervalSuggester.DefaultWorkflowPollInterval()
	}

	for _, o := range opts {
		o(m)
	}

	m.taskDequeuePool = newTaskDequeuePool(m.taskDequeueWorkers, b)

	return m, nil
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

// registeredExecutor is responsible for polling for and executing
// tasks using the given task executor.
//
// Tasks are polled for based on
//
// - a time-based interval, and
// - in-process notifications that are sent when we think there
// might be a task available.
//
// When either of these tiggers occur, we spawn a new goroutine (the
// "task worker") provided that we are under the maximum configured
// workers for this task executor.
//
// The task worker will repeatedly dequeue tasks until none are
// available. To ensure our queue grows in response to load, if a task
// is found in the queue, another task worker will be spawned.
//
type registeredExecutor struct {
	name     string
	executor TaskExecutor
	opts     TaskExecutorOpts

	wakeupChan chan struct{}

	maxWorkers    int
	activeWorkers int
	sync.Mutex    // guards activeWorkers

	wg sync.WaitGroup
}

// GetActiveWorkers returns the worker count. Do not use the returned
// count for decision making. This is provided for logging only.
func (r *registeredExecutor) GetActiveWorkers() int {
	r.Lock()
	defer r.Unlock()
	return r.activeWorkers
}

// DecActiveWorkers decrements the worker count, clamping it at zero.
func (r *registeredExecutor) DecActiveWorkers() {
	r.Lock()
	defer r.Unlock()

	if r.activeWorkers > 0 {
		r.activeWorkers--
	}
}

// IncActiveWorkers increments the active worker count if another
// worker is allowed. It returns true if the count was incremented and
// false otherwise. The caller should not start a worker if false is
// returned.
func (r *registeredExecutor) IncActiveWorkers() bool {
	r.Lock()
	defer r.Unlock()

	if r.activeWorkers < r.maxWorkers {
		r.activeWorkers++
		return true
	}

	return false
}

// WakeupPoller will wake up the poller for this
// registeredExecutor. This is called when a workflow executor
// enqueues tasks.
func (r *registeredExecutor) WakeupPoller() {
	select {
	case r.wakeupChan <- struct{}{}:
	default:
	}
}

// A pollIntervalProvider provides repeated time.Durations that can be
// used by wait/retry loops.
type pollIntervalProvider interface {
	Next() time.Duration
	String() string
}

// A jitterDownIntervalProvider is a pollIntervalProvider that will
// return an interval no larger than the base and no smaller than
// base-jitter.
type jitterDownIntervalProvider struct {
	base      time.Duration
	maxJitter time.Duration
}

func newJitterDownIntervalProvider(base time.Duration, maxJitter time.Duration) *jitterDownIntervalProvider {
	return &jitterDownIntervalProvider{
		base:      base,
		maxJitter: maxJitter,
	}
}

func (j *jitterDownIntervalProvider) Next() time.Duration {
	jitter := time.Duration(float64(j.maxJitter.Nanoseconds()) * rand.Float64())
	return j.base - jitter
}

func (j *jitterDownIntervalProvider) String() string {
	return fmt.Sprintf("%s with up to %s of jitter", j.base, j.maxJitter)
}

// StartPoller runs until the given context is cancelled, starting a
// TaskWorker every interval (or sooner if we've been notified of a
// change).
func (r *registeredExecutor) StartPoller(ctx context.Context, d backend.TaskDequeuer, p pollIntervalProvider, workflowWakeupFun func()) {
	logctx := logrus.WithFields(logrus.Fields{
		"task_name":     r.name,
		"poll_interval": p.String(),
	})
	logctx.Info("Starting task poller")
	timer := time.NewTimer(p.Next())
	for {
		select {
		case <-ctx.Done():
			logctx.WithField("active_workers", r.GetActiveWorkers()).Info("Waiting for all task workers to exit")
			r.wg.Wait()
			logctx.Info("Exiting task poller")
			return
		case <-timer.C:
			break
		case <-r.wakeupChan:
			if !timer.Stop() {
				<-timer.C
			}
			break
		}
		r.startTaskWorker(ctx, d, workflowWakeupFun)
		timer.Reset(p.Next())
	}
}

// startTaskWorker starts a goroutine that will dequeue new tasks and
// execute them, exiting when no tasks are available in the queue. If
// the task poller initially finds a job, it will also start the next
// task worker.
func (r *registeredExecutor) startTaskWorker(ctx context.Context, d backend.TaskDequeuer, workflowWakeupFun func()) {
	logctx := logrus.WithField("task_name", r.name)
	if !r.IncActiveWorkers() {
		if r.maxWorkers > 1 {
			logctx.WithFields(logrus.Fields{
				"max_workers": r.maxWorkers,
			}).Warn("Maximum task workers already started")
		}
		return
	}

	r.wg.Add(1)
	go func() {
		logctx.Debug("Worker starting")
		startedNext := false
		for {
			t, taskCompleter, err := d.DequeueTask(ctx, r.name)
			if err != nil {
				if err != ErrNoTasks {
					logctx.WithError(err).Error("Failed to dequeue task")
				}
				logctx.Debug("Worker shutting down")
				r.DecActiveWorkers()
				r.wg.Done()
				return
			}
			logctx.Debugf("Dequeued task %s", t.Name)

			if !startedNext {
				r.startTaskWorker(ctx, d, workflowWakeupFun)
				startedNext = true
			}

			runCtx := taskCompleter.Context()
			var cancel context.CancelFunc
			if r.opts.Timeout > 0 {
				runCtx, cancel = context.WithTimeout(runCtx, r.opts.Timeout)
			} else {
				runCtx, cancel = context.WithCancel(runCtx)
			}

			r.runTask(runCtx, t, taskCompleter) // nolint: errcheck
			cancel()
			workflowWakeupFun()
		}
	}()
}

func (r *registeredExecutor) runTask(ctx context.Context, t *backend.Task, taskCompleter backend.TaskCompleter) error {
	startTime := time.Now()
	result, err := r.executor.Run(ctx, &task{backendTask: t})
	endTime := time.Now()
	logctx := logrus.WithFields(logrus.Fields{
		"task_name":  r.name,
		"start_time": startTime,
		"end_time":   endTime,
		"duration":   endTime.Sub(startTime),
	})
	if err != nil {
		err := taskCompleter.Fail(err.Error())
		if err != nil {
			logctx.WithError(err).Error("Failed to mark task as failed")
			return err
		}
	} else {
		jsonResults, err := jsonify(result)
		if err != nil {
			logctx.WithError(err).Error("Could not convert returned results to JSON")
			if err := taskCompleter.Fail(err.Error()); err != nil {
				logctx.WithError(err).Error("Failed to fail task completer")
				return err
			}
		}
		err = taskCompleter.Succeed(jsonResults)
		if err != nil {
			logctx.WithError(err).Error("Failed to mark task as successful")
			return err
		}
	}
	logctx.Debug("Task completed successfully")
	return nil
}

// RegisterTaskExecutor registers a TaskExecutor to execute tasks of type taskName.
// This is not safe to call concurrently and should be done from only one thread
// of the process. This must be called before Start.
func (m *Manager) RegisterTaskExecutor(taskName string, executor TaskExecutor, opts TaskExecutorOpts) error {
	r := &registeredExecutor{
		name:       taskName,
		wakeupChan: make(chan struct{}, 10), // 10 is arbitrary
		executor:   executor,
		opts:       opts,
	}
	maxWorkers := opts.Workers
	if maxWorkers == 0 {
		maxWorkers = 1
	}
	r.maxWorkers = maxWorkers
	m.taskExecutors[taskName] = r
	return nil
}

// Start starts the Manager. No workflows, tasks, or schedules will be
// processed before Start is called. This should only be called once.
func (m *Manager) Start(ctx context.Context) error {
	ctx, cancel := context.WithCancel(ctx)
	m.cancel = cancel
	err := m.startTaskPollers(ctx)
	if err != nil {
		return err
	}

	m.taskDequeuePool.Start(ctx)
	if m.workflowScheduler != nil {
		go m.workflowScheduler.Run(ctx)
	}

	go m.runWorkflowExecutor(ctx)
	return nil
}

func (m *Manager) Stop() error {
	if m.cancel != nil {
		m.cancel()
	}

	logrus.Info("Waiting for goroutines to stop")
	m.taskDequeuePool.Stop()
	m.wg.Wait()
	logrus.Info("Goroutines stopped")

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
	err = m.backend.CreateWorkflowSchedule(context.TODO(), instanceName, workflowName,
		jsonData, enabled, recurRule.String(), nextRunAt)
	if err == nil {
		m.workflowScheduler.Trigger()
	}
	return err
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
	err := m.backend.UpdateWorkflowScheduleByName(ctx, instanceName, workflowName, o)
	if err == nil {
		m.workflowScheduler.Trigger()
	}
	return err
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
	if err != nil {
		return err
	}

	m.WakeupWorkflowExecutor()
	return nil
}

func (m *Manager) WakeupWorkflowExecutor() {
	select {
	case m.workflowWakeupChan <- struct{}{}:
	default:
		// We don't log here because if we finish 1000 tasks
		// locally, we don't want the log noise.
	}
}

func (m *Manager) WakeupTaskPollerByTaskName(taskName string) {
	if e, ok := m.taskExecutors[taskName]; ok {
		e.WakeupPoller()
	}
}

func (m *Manager) wakeupTaskPollersByRequests(taskRequests []enqueueTaskRequest) {
	uniqueTaskNames := []string{}
	markMap := make(map[string]struct{})
	for _, tr := range taskRequests {
		name := tr.backendTask.Name
		if _, ok := markMap[name]; !ok {
			markMap[name] = struct{}{}
			uniqueTaskNames = append(uniqueTaskNames, name)
		}
	}

	for _, name := range uniqueTaskNames {
		m.WakeupTaskPollerByTaskName(name)
	}
}

func (m *Manager) startTaskPollers(ctx context.Context) error {
	p := newJitterDownIntervalProvider(m.taskPollInterval, m.taskPollMaxJitter)
	for _, exec := range m.taskExecutors {
		e := exec
		m.wg.Add(1)
		go func() {
			e.StartPoller(ctx, m.taskDequeuePool, p, m.WakeupWorkflowExecutor)
			m.wg.Done()
		}()
	}
	return nil
}

func (m *Manager) runWorkflowExecutor(ctx context.Context) {
	workflowNames := make([]string, 0, len(m.workflowExecutors))
	for k := range m.workflowExecutors {
		workflowNames = append(workflowNames, k)
	}

	timer := time.NewTimer(m.workflowPollInterval)
	m.wg.Add(1)
LOOP:
	for {
		select {
		case <-ctx.Done():
			logrus.Info("Exiting workflow executor")
			break LOOP
		case <-m.workflowWakeupChan:
			if !timer.Stop() {
				<-timer.C
			}
			break
		case <-timer.C:
			break
		}
		for !m.processWorkflow(ctx, workflowNames) {
		}
		timer.Reset(m.workflowPollInterval)
	}
	m.wg.Done()
}

func (m *Manager) callOnWorkflowCompleteCallback(w *backend.WorkflowEvent) {
	if m.onWorkflowCompleteCallback != nil {
		m.onWorkflowCompleteCallback(w)
	}
}

func (m *Manager) processWorkflow(ctx context.Context, workflowNames []string) bool {
	m.wg.Add(1)
	defer m.wg.Done()

	s := newStatsInfo()
	s.Begin("dequeue")
	workflowsToProcess := m.waywardWorkflows.Filter(workflowNames)
	wevt, completer, err := m.backend.DequeueWorkflow(ctx, workflowsToProcess)
	if err != nil {
		if err != ErrNoWorkflowInstances {
			logrus.WithError(err).Error("Failed to dequeue workflow!")
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
		m.waywardWorkflows.Add(wevt.Instance.WorkflowName)
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
		logrus.Warnf("Unknown workflow event %q for workflow %q", wevt.Type, wevt.Instance.WorkflowName)
		m.waywardWorkflows.Add(wevt.Instance.WorkflowName)
		return true
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
			logctx.WithError(decision.err).Debug("Workflow complete with error")
		}

		err = completer.Fail(decision.err)
		if err != nil {
			logctx.WithError(err).Error("Failed to complete workflow")
		}
		s.End("failed")
		m.callOnWorkflowCompleteCallback(wevt)
	} else if decision.complete {
		s.Begin("complete")
		if wevt.CompletedTaskCount != wevt.EnqueuedTaskCount {
			logctx.Warn("Workflow complete with pending tasks")
		} else {
			logctx.Debug("Completing workflow")
		}
		jsonResult, err := jsonify(decision.result)
		if err != nil {
			logctx.WithError(err).Error("Failed to jsonify workflow result")
			jsonResult = nil
		}

		err = completer.Done(jsonResult)
		if err != nil {
			logctx.WithError(err).Error("Failed to complete workflow")
		}
		s.End("complete")
		m.callOnWorkflowCompleteCallback(wevt)
	} else if decision.continuing {
		s.Begin("enqueue_task")
		for _, t := range decision.tasks {
			err := completer.EnqueueTask(&t.backendTask, t.opts)
			if err != nil {
				logrus.WithError(err).Error("Failed to enqueue task!")
				return true
			}
		}
		s.End("enqueue_task")
		s.Begin("continue")
		jsonPayload, err := jsonify(decision.payload)
		if err != nil {
			logrus.WithError(err).Error("Could not marshal payload to JSON, failing workflow")
			err := completer.Fail(err)
			if err != nil {
				logrus.WithError(err).Error("Failed to fail workflow after JSON marshal failure")
			}
			m.callOnWorkflowCompleteCallback(wevt)
		} else {
			err := completer.Continue(jsonPayload)
			if err != nil {
				logrus.WithError(err).Error("Failed to continue workflow")
			}
			m.wakeupTaskPollersByRequests(decision.tasks)
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
		logrus.Warn("Called statsInfo.Begin on currently running label, ignoring")
		return
	}

	s.start[label] = time.Now()
}

func (s *statsInfo) End(label string) {
	start, ok := s.start[label]
	if !ok {
		logrus.Warn("Called statsInfo.End on label with no start, ignoring")
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
	total := 0.0
	for _, label := range keys {
		ms := s.totals[label].Seconds() * 1000
		total = total + ms
		fmt.Fprintf(str, "%s: %f ms; ", label, ms)
	}
	fmt.Fprintf(str, "%s: %f ms", "total", total)
	return str.String()
}

func jsonify(data interface{}) ([]byte, error) {
	if data == nil {
		return nil, nil
	}
	return json.Marshal(data)
}
