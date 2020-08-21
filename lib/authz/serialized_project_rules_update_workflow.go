package authz

import (
	"context"
	"fmt"
	"sort"
	"strings"
	"time"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

var errCanceled = errors.New("Canceled")

// SerializedProjectUpdateWorkflowExecutor is a workflow that allows running 1
// task at a time across many services.
// Given a list of services, it first asks each service to list what tasks it
// would like to run. For example, a service would return a task to run for
// each elasticsearch index to update.
// After the tasks have been gathered for all services, the workflow executes them
// 1 at a time. Once a task is started, it is monitored until completion. The
// next task does not start until the completion of the previous one. If a task
// fails, the workflow does not continue.
type SerializedProjectUpdateWorkflowExecutor struct {
	PollInterval time.Duration
}

type SerializedProjectUpdateTask struct {
	Priority int64
	Params   map[string]string
}

type SerializedProjectUpdateTaskID string

const SerializedProjectUpdateTaskIDUnknown SerializedProjectUpdateTaskID = ""

type SerializedProjectUpdateTaskState string

const (
	SerializedProjectUpdateTaskRunning SerializedProjectUpdateTaskState = "running"
	SerializedProjectUpdateTaskFailed  SerializedProjectUpdateTaskState = "failed"
	SerializedProjectUpdateTaskSuccess SerializedProjectUpdateTaskState = "success"
)

type SerializedProjectUpdateTaskStatus struct {
	State              SerializedProjectUpdateTaskState
	PercentageComplete float32
	Metadata           map[string]string
	Error              string
}

var NoStatus = SerializedProjectUpdateTaskStatus{}

type SerializedProjectUpdate interface {
	ListProjectUpdateTasks(ctx context.Context) ([]SerializedProjectUpdateTask, error)
	RunProjectUpdateTask(ctx context.Context, projectUpdateID string, params map[string]string, projectTaggingRules map[string]*authz.ProjectRules) (SerializedProjectUpdateTaskID, SerializedProjectUpdateTaskStatus, error)
	MonitorProjectUpdateTask(ctx context.Context, projectUpdateID string, id SerializedProjectUpdateTaskID) (SerializedProjectUpdateTaskStatus, error)
	CancelProjectUpdateTask(ctx context.Context, projectUpdateID string, id SerializedProjectUpdateTaskID) error
}

type SerializedProjectUpdateWorkflowParams struct {
	ProjectUpdateID string
	DomainServices  []string
}
type SerializedProjectUpdateWorkflowPhase string

const (
	SerializedProjectUpdateWorkflowPhaseStarting  SerializedProjectUpdateWorkflowPhase = "starting"
	SerializedProjectUpdateWorkflowPhaseRunning   SerializedProjectUpdateWorkflowPhase = "running"
	SerializedProjectUpdateWorkflowPhaseComplete  SerializedProjectUpdateWorkflowPhase = "complete"
	SerializedProjectUpdateWorkflowPhaseCanceling SerializedProjectUpdateWorkflowPhase = "canceling"
)

type serializedProjectUpdateWorkflowPhaseStartingData struct {
	ListTasksResults map[string]SerializedProjectUpdateListTasksTaskResult
}

type serializedProjectUpdateTask struct {
	SerializedProjectUpdateTask `json:"t"`
	DomainService               string `json:"svc"`
}

type serializedProjectUpdateRunningTask struct {
	ID         SerializedProjectUpdateTaskID
	Task       serializedProjectUpdateTask
	LastStatus SerializedProjectUpdateTaskStatus
}

type serializedProjectUpdateWorkflowPhaseRunningData struct {
	RunningTask    serializedProjectUpdateRunningTask
	RemainingTasks []serializedProjectUpdateTask
	CompletedTasks int
	TotalTasks     int
	StartTime      time.Time
	LastUpdated    time.Time
}

type SerializedProjectUpdateWorkflowState struct {
	ProjectUpdateID string
	DomainServices  []string
	Phase           SerializedProjectUpdateWorkflowPhase
	Starting        serializedProjectUpdateWorkflowPhaseStartingData
	Running         serializedProjectUpdateWorkflowPhaseRunningData
	MonitorFailures int
}

func (s *SerializedProjectUpdateWorkflowState) isStartPhaseDone() bool {
	if s.Phase != SerializedProjectUpdateWorkflowPhaseStarting {
		return true
	}

	if s.Starting.ListTasksResults == nil {
		return false
	}

	for _, svc := range s.DomainServices {
		if _, ok := s.Starting.ListTasksResults[svc]; !ok {
			return false
		}
	}

	return true
}

type SerializedProjectUpdateListTasksTaskParams struct{}
type SerializedProjectUpdateListTasksTaskResult struct {
	Tasks []SerializedProjectUpdateTask
}

type SerializedProjectUpdateRunTaskTaskParams struct {
	ProjectUpdateID string
	Params          map[string]string
}
type SerializedProjectUpdateRunTaskTaskResult struct {
	ID     SerializedProjectUpdateTaskID
	Status SerializedProjectUpdateTaskStatus
}

type SerializedProjectUpdateMonitorTaskTaskParams struct {
	ProjectUpdateID string
	ID              SerializedProjectUpdateTaskID
}
type SerializedProjectUpdateMonitorTaskTaskResult struct {
	Status SerializedProjectUpdateTaskStatus
}

type SerializedProjectUpdateCancelTaskTaskParams struct {
	ProjectUpdateID string
	ID              SerializedProjectUpdateTaskID
}
type SerializedProjectUpdateCancelTaskTaskResult struct {
}

func (m *SerializedProjectUpdateWorkflowExecutor) OnStart(
	w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {

	params := SerializedProjectUpdateWorkflowParams{}
	if err := w.GetParameters(&params); err != nil {
		return w.Fail(err)
	}

	if len(params.DomainServices) == 0 {
		return w.Fail(errors.New("no services specified"))
	}

	logrus.Infof("Started SerializedProjectUpdateWorkflow for %s",
		params.ProjectUpdateID)

	for _, svc := range params.DomainServices {
		if err := w.EnqueueTask(listTasksTaskNameForService(svc), nil); err != nil {
			return w.Fail(err)
		}
	}

	return w.Continue(SerializedProjectUpdateWorkflowState{
		ProjectUpdateID: params.ProjectUpdateID,
		DomainServices:  params.DomainServices,
		Phase:           SerializedProjectUpdateWorkflowPhaseStarting,
	})
}

func (m *SerializedProjectUpdateWorkflowExecutor) OnTaskComplete(
	w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {

	state := SerializedProjectUpdateWorkflowState{}
	if err := w.GetPayload(&state); err != nil {
		logrus.WithError(err).Error("Failed to deserialize payload")
		return w.Fail(err)
	}
	taskType, svc, err := parseTaskName(ev.TaskName)
	if err != nil {
		return w.Fail(err)
	}

	state.Running.LastUpdated = time.Now().UTC()
	switch taskType {
	case SerializedProjectUpdateListTaskType:
		return m.handleListTasksComplete(&state, svc, w, ev)
	case SerializedProjectUpdateRunTaskType:
		return m.handleRunTaskComplete(&state, w, ev)
	case SerializedProjectUpdateMonitorTaskType:
		return m.handleMonitorTaskComplete(&state, w, ev)
	case SerializedProjectUpdateCancelTaskType:
		return m.handleCancelTaskComplete(&state, w, ev)
	}

	return w.Fail(errors.Errorf("Could not handle task completion %q", ev.TaskName))
}

func (m *SerializedProjectUpdateWorkflowExecutor) OnCancel(
	w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	state := SerializedProjectUpdateWorkflowState{}
	if err := w.GetPayload(&state); err != nil {
		logrus.WithError(err).Error("Failed to deserialize payload")
		return w.Fail(err)
	}
	if state.Phase == SerializedProjectUpdateWorkflowPhaseStarting {
		return w.Fail(errCanceled)
	}
	if state.Phase == SerializedProjectUpdateWorkflowPhaseCanceling {
		return w.Continue(state)
	}
	return m.cancelRunningTask(&state, w)
}

func (m *SerializedProjectUpdateWorkflowExecutor) cancelRunningTask(
	state *SerializedProjectUpdateWorkflowState, w cereal.WorkflowInstance) cereal.Decision {
	state.Phase = SerializedProjectUpdateWorkflowPhaseCanceling
	if state.Running.RunningTask.ID != SerializedProjectUpdateTaskIDUnknown {
		w.EnqueueTask( // nolint: errcheck
			cancelTaskTaskNameForService(state.Running.RunningTask.Task.DomainService),
			SerializedProjectUpdateCancelTaskTaskParams{
				ProjectUpdateID: state.ProjectUpdateID,
				ID:              state.Running.RunningTask.ID,
			},
		)
	}
	return w.Continue(state)
}

func (m *SerializedProjectUpdateWorkflowExecutor) handleListTasksComplete(
	state *SerializedProjectUpdateWorkflowState, svc string, w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {
	if ev.Result.Err() != nil {
		return w.Fail(ev.Result.Err())
	}

	var res SerializedProjectUpdateListTasksTaskResult
	if err := ev.Result.Get(&res); err != nil {
		return w.Fail(err)
	}

	if state.Starting.ListTasksResults == nil {
		state.Starting.ListTasksResults = make(map[string]SerializedProjectUpdateListTasksTaskResult)
	}

	state.Starting.ListTasksResults[svc] = res

	if state.isStartPhaseDone() {
		tasks := []serializedProjectUpdateTask{}
		for svc, resForSvc := range state.Starting.ListTasksResults {
			for _, t := range resForSvc.Tasks {
				tasks = append(tasks, serializedProjectUpdateTask{
					DomainService:               svc,
					SerializedProjectUpdateTask: t,
				})
			}
		}

		if len(tasks) == 0 {
			state.Phase = SerializedProjectUpdateWorkflowPhaseComplete
			return w.Complete(cereal.WithResult(state))
		}

		sort.Slice(tasks, func(i, j int) bool {
			return tasks[i].Priority > tasks[j].Priority
		})

		state.Phase = SerializedProjectUpdateWorkflowPhaseRunning
		state.Running.TotalTasks = len(tasks)
		state.Running.StartTime = time.Now().UTC()
		state.Running.RunningTask = serializedProjectUpdateRunningTask{
			ID:   SerializedProjectUpdateTaskIDUnknown,
			Task: tasks[0],
		}
		if err := w.EnqueueTask(runTaskTaskNameForService(tasks[0].DomainService),
			SerializedProjectUpdateRunTaskTaskParams{
				ProjectUpdateID: state.ProjectUpdateID,
				Params:          tasks[0].Params,
			}); err != nil {
			return w.Fail(err)
		}
		state.Running.RemainingTasks = tasks[1:]
	}

	return w.Continue(state)
}

func (m *SerializedProjectUpdateWorkflowExecutor) handleRunTaskComplete(
	state *SerializedProjectUpdateWorkflowState, w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {

	if ev.Result.Err() != nil {
		return w.Fail(ev.Result.Err())
	}

	var res SerializedProjectUpdateRunTaskTaskResult
	if err := ev.Result.Get(&res); err != nil {
		return w.Fail(err)
	}

	switch res.Status.State {
	case SerializedProjectUpdateTaskSuccess:
		state.Running.CompletedTasks++
		return m.startNextTask(state, w)
	case SerializedProjectUpdateTaskFailed:
		return w.Fail(errors.New(res.Status.Error))
	case SerializedProjectUpdateTaskRunning:
		state.Running.RunningTask.ID = res.ID
		state.Running.RunningTask.LastStatus = res.Status
	default:
		return w.Fail(errors.New("Unknown task state"))
	}

	if state.Phase == SerializedProjectUpdateWorkflowPhaseCanceling {
		return m.cancelRunningTask(state, w)
	}

	// Start task monitor
	return m.startMonitorTask(state, w, true)
}

const maxMonitorFailures = 10

func (m *SerializedProjectUpdateWorkflowExecutor) handleMonitorTaskComplete(
	state *SerializedProjectUpdateWorkflowState, w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {

	var params SerializedProjectUpdateMonitorTaskTaskResult
	if err := ev.Result.GetParameters(&params); err != nil {
		return w.Fail(err)
	}

	var res SerializedProjectUpdateMonitorTaskTaskResult
	if err := ev.Result.Get(&res); err != nil {
		state.MonitorFailures++
		if state.MonitorFailures > maxMonitorFailures {
			return w.Fail(err)
		}
		return m.startMonitorTask(state, w, false)
	} else {
		state.MonitorFailures = 0
		switch res.Status.State {
		case SerializedProjectUpdateTaskSuccess:
			state.Running.CompletedTasks++
			return m.startNextTask(state, w)
		case SerializedProjectUpdateTaskFailed:
			return w.Fail(errors.New(res.Status.Error))
		case SerializedProjectUpdateTaskRunning:
			state.Running.RunningTask.LastStatus = res.Status
			return m.startMonitorTask(state, w, false)
		default:
			return w.Fail(errors.New("Unknown task state"))
		}
	}
}

func (m *SerializedProjectUpdateWorkflowExecutor) handleCancelTaskComplete(
	state *SerializedProjectUpdateWorkflowState, w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {
	if err := ev.Result.Err(); err != nil {
		return w.Fail(err)
	}
	return w.Fail(errCanceled)
}

const maxFailureBackoffDuration = 2 * time.Minute

func (m *SerializedProjectUpdateWorkflowExecutor) startMonitorTask(
	state *SerializedProjectUpdateWorkflowState, w cereal.WorkflowInstance, first bool) cereal.Decision {

	if state.Phase == SerializedProjectUpdateWorkflowPhaseRunning {
		failureBackoff := time.Duration(0)
		if state.MonitorFailures > 0 {
			failureBackoff = (1 << state.MonitorFailures) * time.Second
			if failureBackoff > maxFailureBackoffDuration {
				failureBackoff = maxFailureBackoffDuration
			}
		}

		nextRun := time.Now()
		if !first {
			nextRun.Add(m.PollInterval).Add(failureBackoff)
		}

		w.EnqueueTask(monitorTaskTaskNameForService(state.Running.RunningTask.Task.DomainService), // nolint: errcheck
			SerializedProjectUpdateMonitorTaskTaskParams{
				ProjectUpdateID: state.ProjectUpdateID,
				ID:              state.Running.RunningTask.ID,
			}, cereal.StartAfter(nextRun))
	}

	return w.Continue(state)
}

func (m *SerializedProjectUpdateWorkflowExecutor) startNextTask(
	state *SerializedProjectUpdateWorkflowState, w cereal.WorkflowInstance) cereal.Decision {

	if len(state.Running.RemainingTasks) == 0 {
		state.Phase = SerializedProjectUpdateWorkflowPhaseComplete
		return w.Complete(cereal.WithResult(state))
	}

	if state.Phase == SerializedProjectUpdateWorkflowPhaseCanceling {
		return w.Fail(errCanceled)
	}

	state.Phase = SerializedProjectUpdateWorkflowPhaseRunning
	state.Running.RunningTask = serializedProjectUpdateRunningTask{
		ID:   SerializedProjectUpdateTaskIDUnknown,
		Task: state.Running.RemainingTasks[0],
	}
	if err := w.EnqueueTask(runTaskTaskNameForService(state.Running.RunningTask.Task.DomainService),
		SerializedProjectUpdateRunTaskTaskParams{
			ProjectUpdateID: state.ProjectUpdateID,
			Params:          state.Running.RunningTask.Task.Params,
		}); err != nil {
		return w.Fail(err)
	}
	state.Running.RemainingTasks = state.Running.RemainingTasks[1:]

	return w.Continue(state)
}

const (
	SerializedProjectUpdateListTaskType    = "project-update-list-tasks"
	SerializedProjectUpdateRunTaskType     = "project-update-run-task"
	SerializedProjectUpdateMonitorTaskType = "project-update-monitor-task"
	SerializedProjectUpdateCancelTaskType  = "project-update-cancel-task"
)

func parseTaskName(taskName cereal.TaskName) (taskType string, svcName string, err error) {
	parts := strings.Split(taskName.String(), "/")
	if len(parts) != 2 {
		return "", "", errors.New("Failed to parse task name for service")
	}
	return parts[0], parts[1], nil
}

func listTasksTaskNameForService(svc string) cereal.TaskName {
	return cereal.NewTaskName(fmt.Sprintf("%s/%s", SerializedProjectUpdateListTaskType, svc))
}

func runTaskTaskNameForService(svc string) cereal.TaskName {
	return cereal.NewTaskName(fmt.Sprintf("%s/%s", SerializedProjectUpdateRunTaskType, svc))
}

func monitorTaskTaskNameForService(svc string) cereal.TaskName {
	return cereal.NewTaskName(fmt.Sprintf("%s/%s", SerializedProjectUpdateMonitorTaskType, svc))
}

func cancelTaskTaskNameForService(svc string) cereal.TaskName {
	return cereal.NewTaskName(fmt.Sprintf("%s/%s", SerializedProjectUpdateCancelTaskType, svc))
}

type serializedProjectUpdateListTasksTask struct {
	client SerializedProjectUpdate
	svc    string
}

func (s *serializedProjectUpdateListTasksTask) Run(
	ctx context.Context, task cereal.Task) (interface{}, error) {

	logrus.WithFields(logrus.Fields{
		"svc": s.svc,
	}).Info("Listing project update tasks")

	tasks, err := s.client.ListProjectUpdateTasks(ctx)
	if err != nil {
		return nil, err
	}

	return &SerializedProjectUpdateListTasksTaskResult{
		Tasks: tasks,
	}, nil
}

type serializedProjectUpdateRunTaskTask struct {
	client                     SerializedProjectUpdate
	authzProjectsServiceClient authz.ProjectsServiceClient
	svc                        string
}

func (s *serializedProjectUpdateRunTaskTask) Run(
	ctx context.Context, task cereal.Task) (interface{}, error) {

	var params SerializedProjectUpdateRunTaskTaskParams
	if err := task.GetParameters(&params); err != nil {
		return nil, err
	}

	logrus.WithFields(logrus.Fields{
		"svc":               s.svc,
		"project_update_id": params.ProjectUpdateID,
		"params":            params.Params,
	}).Info("Running project update task")

	projectCollectionRulesResp, err := s.authzProjectsServiceClient.ListRulesForAllProjects(ctx,
		&authz.ListRulesForAllProjectsReq{})
	if err != nil {
		return []string{}, errors.Wrap(err, "Failed to get authz project rules")
	}

	id, status, err := s.client.RunProjectUpdateTask(ctx, params.ProjectUpdateID, params.Params,
		projectCollectionRulesResp.ProjectRules)
	if err != nil {
		return nil, err
	}

	return &SerializedProjectUpdateRunTaskTaskResult{
		ID:     id,
		Status: status,
	}, nil
}

type serializedProjectUpdateMonitorTaskTask struct {
	client SerializedProjectUpdate
	svc    string
}

func (s *serializedProjectUpdateMonitorTaskTask) Run(
	ctx context.Context, task cereal.Task) (interface{}, error) {

	var params SerializedProjectUpdateMonitorTaskTaskParams
	if err := task.GetParameters(&params); err != nil {
		return nil, err
	}

	logrus.WithFields(logrus.Fields{
		"svc":               s.svc,
		"project_update_id": params.ProjectUpdateID,
		"task_id":           params.ID,
	}).Info("Monitoring project update task")

	status, err := s.client.MonitorProjectUpdateTask(ctx, params.ProjectUpdateID, params.ID)
	if err != nil {
		return nil, err
	}

	return &SerializedProjectUpdateMonitorTaskTaskResult{
		Status: status,
	}, nil
}

type serializedProjectUpdateCancelTaskTask struct {
	client SerializedProjectUpdate
	svc    string
}

func (s *serializedProjectUpdateCancelTaskTask) Run(
	ctx context.Context, task cereal.Task) (interface{}, error) {

	var params SerializedProjectUpdateCancelTaskTaskParams
	if err := task.GetParameters(&params); err != nil {
		return nil, err
	}

	logrus.WithFields(logrus.Fields{
		"svc":               s.svc,
		"project_update_id": params.ProjectUpdateID,
		"task_id":           params.ID,
	}).Info("Canceling project update task")

	err := s.client.CancelProjectUpdateTask(ctx, params.ProjectUpdateID, params.ID)
	if err != nil {
		return nil, err
	}

	return &SerializedProjectUpdateCancelTaskTaskResult{}, nil
}

func RegisterSerialTaskExecutors(manager *cereal.Manager, svc string, client SerializedProjectUpdate,
	authzProjectsServiceClient authz.ProjectsServiceClient) error {

	taskExecutorOpts := cereal.TaskExecutorOpts{
		Workers: 1,
	}

	listTaskExecutor := &serializedProjectUpdateListTasksTask{
		client: client,
		svc:    svc,
	}

	if err := manager.RegisterTaskExecutor(listTasksTaskNameForService(svc), listTaskExecutor,
		taskExecutorOpts); err != nil {
		return err
	}

	runTaskExecutor := &serializedProjectUpdateRunTaskTask{
		client:                     client,
		authzProjectsServiceClient: authzProjectsServiceClient,
		svc:                        svc,
	}

	if err := manager.RegisterTaskExecutor(runTaskTaskNameForService(svc), runTaskExecutor,
		taskExecutorOpts); err != nil {
		return err
	}

	monitorTask := &serializedProjectUpdateMonitorTaskTask{
		client: client,
		svc:    svc,
	}
	if err := manager.RegisterTaskExecutor(monitorTaskTaskNameForService(svc), monitorTask,
		taskExecutorOpts); err != nil {
		return err
	}

	cancelTaskExecutor := &serializedProjectUpdateCancelTaskTask{
		client: client,
		svc:    svc,
	}
	if err := manager.RegisterTaskExecutor(cancelTaskTaskNameForService(svc), cancelTaskExecutor,
		taskExecutorOpts); err != nil {
		return err
	}

	return nil
}

func NewSerializedWorkflowExecutor() *SerializedProjectUpdateWorkflowExecutor {
	return &SerializedProjectUpdateWorkflowExecutor{
		PollInterval: 10 * time.Second,
	}
}
