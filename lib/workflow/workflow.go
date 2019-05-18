package workflow

import (
	"context"
	"encoding/json"
	"fmt"
	"sort"
	"strings"
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
	ErrNoDueWorkflows         = errors.New("No due workflows")
	ErrNoScheduledWorkflows   = errors.New("No workflows are scheduled")
)

type Schedule backend.Schedule

type Task interface {
	GetParameters(interface{}) error
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

type TaskResult interface {
	GetParameters(interface{}) error
	Get(interface{}) error
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

type WorkflowInstance interface {
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

type Decision struct {
	complete   bool
	continuing bool
	payload    interface{}
	tasks      []backend.Task
}

type StartEvent struct{}
type TaskCompleteEvent struct {
	TaskName string
	Result   TaskResult
}
type CancelEvent struct{}

type WorkflowExecutor interface {
	OnStart(w WorkflowInstance, ev StartEvent) Decision
	OnTaskComplete(w WorkflowInstance, ev TaskCompleteEvent) Decision
	OnCancel(w WorkflowInstance, ev CancelEvent) Decision
}

// TODO(ssd) 2019-05-10: How do we want to handle cancellation?
type TaskExecutor interface {
	Run(context.Context, Task) (interface{}, error)
}

type WorkflowManager struct {
	workflowExecutors map[string]WorkflowExecutor
	taskExecutors     map[string]registeredExecutor
	workflowScheduler *workflowScheduler
	backend           backend.Driver
}

func WithRetries(numRetries int) EnqueueOpts {
	return func(o *backend.TaskEnqueueOpts) {
		o.TryRemaining = numRetries + 1
	}
}

func StartAfter(startAfter time.Time) EnqueueOpts {
	return func(o *backend.TaskEnqueueOpts) {
		o.StartAfter = startAfter
	}
}

type EnqueueOpts func(*backend.TaskEnqueueOpts)

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

func (m *WorkflowManager) CreateWorkflowSchedule(
	scheduleName string,
	workflowName string,
	parameters interface{},
	enabled bool,
	recurRule *rrule.RRule,
) error {
	nextRunAt := recurRule.After(time.Now().UTC(), true).UTC()
	jsonData, err := jsonify(parameters)
	if err != nil {
		return err
	}
	return m.backend.CreateWorkflowSchedule(context.TODO(), scheduleName, workflowName,
		jsonData, enabled, recurRule.String(), nextRunAt)
}

type WorkflowScheduleUpdateOpts func(*backend.WorkflowScheduleUpdateOpts) error

func UpdateEnabled(enabled bool) WorkflowScheduleUpdateOpts {
	return func(o *backend.WorkflowScheduleUpdateOpts) error {
		o.Enabled = enabled
		o.UpdateEnabled = true
		return nil
	}
}

func UpdateParameters(parameters interface{}) WorkflowScheduleUpdateOpts {
	return func(o *backend.WorkflowScheduleUpdateOpts) error {
		paramsData, err := jsonify(parameters)
		o.UpdateParameters = true
		o.Parameters = paramsData
		return err
	}
}

func UpdateRecurrence(recurRule *rrule.RRule) WorkflowScheduleUpdateOpts {
	return func(o *backend.WorkflowScheduleUpdateOpts) error {
		o.UpdateRecurrence = true
		o.Recurrence = recurRule.String()
		o.NextRunAt = recurRule.After(time.Now().UTC(), true).UTC()
		return nil
	}
}

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

func (m *WorkflowManager) UpdateWorkflowScheduleByName(ctx context.Context,
	scheduleName string, workflowName string, opts ...WorkflowScheduleUpdateOpts) error {

	o := backend.WorkflowScheduleUpdateOpts{}
	for _, opt := range opts {
		err := opt(&o)
		if err != nil {
			return err
		}
	}
	return m.backend.UpdateWorkflowScheduleByName(ctx, scheduleName, workflowName, o)
}

func (m *WorkflowManager) GetScheduledWorkflowParameters(ctx context.Context, scheduleName string, workflowName string, out interface{}) error {
	data, err := m.backend.GetScheduledWorkflowParameters(ctx, scheduleName, workflowName)
	if err != nil {
		return errors.Wrap(err, "could not retrieve parameters for workflow")
	}

	if data != nil {
		return json.Unmarshal(data, out)
	}

	return nil
}

func (m *WorkflowManager) GetScheduledWorkflowRecurrence(ctx context.Context, scheduleName string, workflowName string) (*rrule.RRule, error) {
	ruleStr, err := m.backend.GetScheduledWorkflowRecurrence(ctx, scheduleName, workflowName)
	if err != nil {
		return nil, errors.Wrap(err, "could not retrieve parameters for workflow")
	}

	if ruleStr == "" {
		return nil, errors.New("no recurrence data")
	}

	return rrule.StrToRRule(ruleStr)
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
	err = m.backend.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
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
				taskCompleter.Fail(err.Error())
			}
			err = taskCompleter.Succeed(jsonResults)
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
