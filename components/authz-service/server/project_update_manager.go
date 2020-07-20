package server

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/lib/stringutils"
	uuid "github.com/chef/automate/lib/uuid4"

	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/logger"

	project_update_tags "github.com/chef/automate/lib/authz"
)

type ProjectUpdateStage string
type ProjectUpdateState string

const (
	ProjectUpdateRunningState    ProjectUpdateState = "running"
	ProjectUpdateNotRunningState ProjectUpdateState = "not_running"

	ProjectUpdateStageApplyStagedRules     ProjectUpdateStage = "apply_staged_rules"
	ProjectUpdateStageUpdateDomainServices ProjectUpdateStage = "update_domain_services"
	ProjectUpdateStageUpdateDone           ProjectUpdateStage = "done"
	ProjectUpdateStageUpdateNone           ProjectUpdateStage = "none"
)

var (
	ProjectUpdateWorkflowName = cereal.NewWorkflowName("ProjectUpdate")
	ProjectUpdateInstanceName = "SingletonV1"
	ApplyStagedRulesTaskName  = cereal.NewTaskName("authz/ApplyStagedRules")

	serialSubworkflowName = "serialized"
)

var ParallelProjectUpdateDomainServices = []string{}

var SerializedProjectUpdateDomainServices = []string{
	"compliance",
	"ingest",
	"nodemanager",
	"feed",
}

type ProjectUpdateStatus interface {
	Failed() bool
	Cancelled() bool
	FailureMessage() string
	PercentageComplete() float64
	EstimatedTimeComplete() time.Time
	State() ProjectUpdateState
	Stage() ProjectUpdateStage
}

type ProjectUpdateMgr interface {
	Cancel() error
	Start() error
	Status() (ProjectUpdateStatus, error)
}

func createProjectUpdateID() string {
	uuid, err := uuid.NewV4()
	if err != nil {
		return "project-update-id"
	}

	return uuid.String()
}

func NewWorkflowExecutor() (*patterns.ChainWorkflowExecutor, error) {
	// This function creates a WorkflowExecutor out of multiple smaller workflow executors.
	// First, we want the ApplyStagedRules database commit stuff to run. Then, we want
	// the domain services to update.
	// So, that's a chain of [applyStagedRulesExecutor, parallelWorkflowExecutor]
	// parallelWorkflowExecutor is itself a bunch of smaller workflows that run in parallel.
	// There is a DomainServiceExecutor for nodemanager, and a SerializedWorkflowExecutor
	// for any services that need to update elasticsearch. SerializedWorkflowExecutor will
	// ensure that only one index is updated at a time and prevent elasticsearch from falling
	// over.
	// applyStagedRulesExecutor is simply an executor that runs ApplyStagedRulesTaskExecutor
	// An ascii art below describes that it all looks like when it's tied together
	/*
	   +--------------------------------------------------+
	   |                                                  |
	   |  +------------+       +------------------------+ |    +-----------------+
	   |  |            |       |                        | |    |ListTasks        |
	   |  | Apply      | Chain | +--------------------+ + +    |RunTask          |
	   |  | Staged     +------>+ |SerializedWorkflow  | cereal |MonitorTask      |
	   |  | Rules      |       | |                    <-+-+---->CancelTask       |
	   |  | Workflow   |       | |                    | | |    |       compliance|
	   |  +------------+      P| |                    | | |    +-----------------+
	   |                      a| |                    | | |    +-----------------+
	   |                      r| |                    | + +    |ListTasks        |
	   |                      a| |                    | cereal |RunTask          |
	   |                      l| |                    <-+-+---->MonitorTask      |
	   |                      l| |                    | | |    |CancelTask       |
	   |                      e| |                    | | |    |           ingest|
	   |                      l| +--------------------+ | |    +-----------------+
	   |                       | +--------------------+ + +    +-----------------+
	   |                       | |Domain Service      | cereal |Start Update     |
	   |                       | |Update Workflow     <-+-+---->Update Status    |
	   |                       | |nodemanager         | | |    |      nodemanager|
	   |                       | +--------------------+ | |    +-----------------+
	   |                       +------------------------+ |
	   |                                     authz-service|
	   +--------------------------------------------------+

	*/
	workflowMap := make(map[string]cereal.WorkflowExecutor)
	lock := sync.Mutex{}

	applyStagedRulesExecutor := patterns.NewSingleTaskWorkflowExecutor(ApplyStagedRulesTaskName, true)
	// The code below allows us to easily add to the ProjectUpdateDomainServices list.
	// By not preconfiguring our workflow with an exact set of subworkflow executors,
	// we can generate the workflow executor for the correct domain service easily.
	// The logic only depends on the name. This means that an instance of authz that
	// didn't know about a domain service can still drive its workflow.
	parallelWorkflowExecutor := patterns.NewParallelWorkflowExecutor(func(key string) (cereal.WorkflowExecutor, bool) {
		lock.Lock()
		defer lock.Unlock()
		if workflowExecutor, ok := workflowMap[key]; ok {
			return workflowExecutor, true
		}

		var workflowExecutor cereal.WorkflowExecutor
		if key == serialSubworkflowName {
			workflowExecutor = project_update_tags.NewSerializedWorkflowExecutor()
		} else if stringutils.SliceContains(ParallelProjectUpdateDomainServices, key) {
			workflowExecutor = project_update_tags.NewWorkflowExecutorForDomainService(key)
		} else {
			logrus.Errorf("invalid parallel subworkflow %q", key)
			return nil, false
		}

		logrus.Infof("creating workflow executor for %q", key)
		workflowMap[key] = workflowExecutor
		return workflowExecutor, true
	})
	return patterns.NewChainWorkflowExecutor(applyStagedRulesExecutor, parallelWorkflowExecutor)
}

type ApplyStagedRulesTaskExecutor struct {
	store           storage.Storage
	policyRefresher PolicyRefresher
	log             logger.Logger
}

type ApplyStagedRulesResult struct {
}

type ApplyStagedRulesParams struct {
}

func (s *ApplyStagedRulesTaskExecutor) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	s.log.Info("apply project rules starting")

	if err := s.store.ApplyStagedRules(ctx); err != nil {
		s.log.Warnf("error applying staged projects: %s", err.Error())
		return nil, status.Errorf(codes.Internal,
			"error applying staged projects: %s", err.Error())
	}

	// TODO: If the domain services are reading out of the cache below, that is a problem for multinode.
	//       this does not force a refresh on all the nodes. We should read this information from the
	//       database.
	if err := s.policyRefresher.Refresh(ctx); err != nil {
		s.log.Warnf("error refreshing policy cache. the rules were updated but the apply was not started, please try again.")
		return nil, status.Errorf(codes.Internal,
			"error refreshing policy cache: %s", err.Error())
	}

	return ApplyStagedRulesResult{}, nil
}

type CerealProjectUpdateManager struct {
	manager      *cereal.Manager
	workflowName cereal.WorkflowName
	instanceName string
}

func RegisterCerealProjectUpdateManager(manager *cereal.Manager, log logger.Logger, s storage.Storage, pr PolicyRefresher) (ProjectUpdateMgr, error) {
	domainServicesWorkflowExecutor, err := NewWorkflowExecutor()
	if err != nil {
		return nil, err
	}

	if err := manager.RegisterWorkflowExecutor(ProjectUpdateWorkflowName, domainServicesWorkflowExecutor); err != nil {
		return nil, err
	}

	applyStagedRulesTaskExecutor := &ApplyStagedRulesTaskExecutor{
		store:           s,
		policyRefresher: pr,
		log:             log,
	}
	if err := manager.RegisterTaskExecutor(ApplyStagedRulesTaskName, applyStagedRulesTaskExecutor,
		cereal.TaskExecutorOpts{Workers: 1}); err != nil {

	}

	updateManager := &CerealProjectUpdateManager{
		manager:      manager,
		workflowName: ProjectUpdateWorkflowName,
		instanceName: ProjectUpdateInstanceName,
	}

	return updateManager, nil
}

func (m *CerealProjectUpdateManager) Cancel() error {
	err := m.manager.CancelWorkflow(context.Background(), m.workflowName, m.instanceName)
	if err == cereal.ErrWorkflowInstanceNotFound {
		return nil
	}
	return err
}

func (m *CerealProjectUpdateManager) Start() error {
	projectUpdateID := createProjectUpdateID()
	params := map[string]interface{}{}

	parallelSubworkflows := make([]string, 0, len(ParallelProjectUpdateDomainServices)+1)
	parallelSubworkflows = append(parallelSubworkflows, serialSubworkflowName)
	params[serialSubworkflowName] = project_update_tags.SerializedProjectUpdateWorkflowParams{
		ProjectUpdateID: projectUpdateID,
		DomainServices:  SerializedProjectUpdateDomainServices,
	}
	for _, svc := range ParallelProjectUpdateDomainServices {
		params[svc] = project_update_tags.DomainProjectUpdateWorkflowParameters{
			ProjectUpdateID: projectUpdateID,
		}
		parallelSubworkflows = append(parallelSubworkflows, svc)
	}
	domainSvcUpdateWorkflowParams, err := patterns.ToParallelWorkflowParameters(parallelSubworkflows, params)
	if err != nil {
		return err
	}
	return patterns.EnqueueChainWorkflow(context.TODO(), m.manager, m.workflowName, m.instanceName, []interface{}{
		ApplyStagedRulesParams{},
		domainSvcUpdateWorkflowParams,
	})
}

type workflowInstance struct {
	chain *patterns.ChainWorkflowInstance
}

func (w *workflowInstance) Failed() bool {
	return w.FailureMessage() != ""
}

func (w *workflowInstance) Cancelled() bool {
	return w.chain.IsCancelled()
}

func (w *workflowInstance) FailureMessage() string {
	if err := w.chain.Err(); err != nil {
		return err.Error()
	}

	if instance, err := w.GetApplyStagedRulesInstance(); err != nil {
		if err == cereal.ErrWorkflowInstanceNotFound {
			return ""
		}
		return err.Error()
	} else {
		if err := instance.Err(); err != nil {
			return err.Error()
		}
	}

	if updateDomainServicesInstance, err := w.GetUpdateDomainServicesInstance(); err != nil {
		if err == cereal.ErrWorkflowInstanceNotFound {
			return ""
		}
		return err.Error()
	} else {
		if err := updateDomainServicesInstance.Err(); err != nil {
			return err.Error()
		}

		errMsg := ""
		for _, subworkflowKey := range updateDomainServicesInstance.ListSubWorkflows() {
			if subworkflowInstance, err := updateDomainServicesInstance.GetSubWorkflow(subworkflowKey); err != nil {
				if err == cereal.ErrWorkflowInstanceNotFound {
					continue
				}
				errMsg = fmt.Sprintf("%s; %s: %s", errMsg, subworkflowKey, err.Error())
			} else {
				if subworkflowInstance.Err() != nil {
					errMsg = fmt.Sprintf("%s; %s: %s", errMsg, subworkflowKey, subworkflowInstance.Err().Error())
				}
			}
		}
		return errMsg
	}
}

func (w *workflowInstance) PercentageComplete() float64 {
	if !w.IsRunning() {
		return 1.0
	}

	progress, err := w.collectProgress()
	if err != nil {
		logrus.WithError(err).Error("Failed to get progress")
		return 0
	}

	return float64(progress)
}

func (w *workflowInstance) EstimatedTimeComplete() time.Time {
	longestDomainJobStatus := project_update_tags.FindSlowestJobStatus(
		w.collectRunningJobStatuses())

	longestDomainTimeEstimate := time.Unix(longestDomainJobStatus.EstimatedEndTimeInSec, 0)

	// If the estimated time is before now return the null time.
	if longestDomainTimeEstimate.Before(time.Now()) {
		return time.Time{}
	}

	return longestDomainTimeEstimate
}

func (w *workflowInstance) State() ProjectUpdateState {
	if w.IsRunning() {
		return ProjectUpdateRunningState
	}
	return ProjectUpdateNotRunningState
}

func (w *workflowInstance) IsRunning() bool {
	return w.chain.IsRunning()
}

func (w *workflowInstance) Stage() ProjectUpdateStage {
	if !w.IsRunning() {
		return ProjectUpdateStageUpdateDone
	}
	if _, err := w.GetUpdateDomainServicesInstance(); err != nil {
		return ProjectUpdateStageApplyStagedRules
	}
	return ProjectUpdateStageUpdateDomainServices
}

func (w *workflowInstance) GetApplyStagedRulesInstance() (cereal.ImmutableWorkflowInstance, error) {
	return w.chain.GetSubWorkflow(0)
}

func (w *workflowInstance) GetUpdateDomainServicesInstance() (*patterns.ParallelWorkflowInstance, error) {
	instance, err := w.chain.GetSubWorkflow(1)
	if err != nil {
		return nil, err
	}
	return patterns.ToParallelWorkflowInstance(instance)
}

func (m *CerealProjectUpdateManager) getWorkflowInstance(ctx context.Context) (*workflowInstance, error) {
	chainInstance, err := patterns.GetChainWorkflowInstance(ctx, m.manager, m.workflowName, m.instanceName)
	if err != nil {
		return nil, err
	}
	return &workflowInstance{chain: chainInstance}, nil
}

type EmptyProjectUpdateStatus struct{}

func (*EmptyProjectUpdateStatus) Failed() bool {
	return false
}

func (*EmptyProjectUpdateStatus) Cancelled() bool {
	return false
}

func (*EmptyProjectUpdateStatus) FailureMessage() string {
	return ""
}

func (*EmptyProjectUpdateStatus) PercentageComplete() float64 {
	return 1.0
}

func (*EmptyProjectUpdateStatus) EstimatedTimeComplete() time.Time {
	return time.Time{}
}

func (*EmptyProjectUpdateStatus) State() ProjectUpdateState {
	return ProjectUpdateNotRunningState
}

func (*EmptyProjectUpdateStatus) Stage() ProjectUpdateStage {
	return ProjectUpdateStageUpdateNone
}

func (m *CerealProjectUpdateManager) Status() (ProjectUpdateStatus, error) {
	projectUpdateInstance, err := m.getWorkflowInstance(context.TODO())
	if err != nil {
		if err == cereal.ErrWorkflowInstanceNotFound {
			return &EmptyProjectUpdateStatus{}, nil
		}
		return nil, err
	}
	return projectUpdateInstance, nil
}

func (w *workflowInstance) collectProgress() (float32, error) {
	if !w.IsRunning() {
		return 1, nil
	}
	domainServicesUpdateInstance, err := w.GetUpdateDomainServicesInstance()
	if err != nil {
		return 0, err
	}

	progress := []float32{}
	weight := []int{}

	for _, d := range domainServicesUpdateInstance.ListSubWorkflows() {
		subWorkflow, err := domainServicesUpdateInstance.GetSubWorkflow(d)
		if err != nil {
			logrus.WithError(err).Errorf("failed to get subworkflow for %q", d)
			continue
		}

		if d == serialSubworkflowName {
			state := project_update_tags.SerializedProjectUpdateWorkflowState{}
			if subWorkflow.IsRunning() {
				if err := subWorkflow.GetPayload(&state); err != nil {
					logrus.WithError(err).Errorf("failed to get payload for %q", d)
					continue
				}
			} else {
				if err := subWorkflow.GetResult(&state); err != nil {
					logrus.WithError(err).Errorf("failed to get result for %q", d)
					continue
				}
			}

			if state.Phase == project_update_tags.SerializedProjectUpdateWorkflowPhaseRunning && state.Running.TotalTasks > 0 {
				progress = append(progress,
					state.Running.RunningTask.LastStatus.PercentageComplete)
				weight = append(weight, 1)
			}
			progress = append(progress, 1)
			weight = append(weight, state.Running.CompletedTasks)

			progress = append(progress, 0)
			weight = append(weight, len(state.Running.RemainingTasks))
		} else {
			payload := project_update_tags.DomainProjectUpdateWorkflowPayload{}
			if subWorkflow.IsRunning() {
				if err := subWorkflow.GetPayload(&payload); err != nil {
					logrus.WithError(err).Errorf("failed to get payload for %q", d)
					continue
				}
				progress = append(progress, payload.MergedJobStatus.PercentageComplete)
				weight = append(weight, 1)
			} else {
				progress = append(progress, 1)
				weight = append(weight, 1)
			}
		}
	}

	totalWeight := 0
	for i := range weight {
		totalWeight += weight[i]
	}

	if totalWeight == 0 {
		return 0, nil
	}
	var prog float32
	for i := range weight {
		prog += (progress[i] * float32(weight[i]) / float32(totalWeight))
	}

	return prog, nil
}

func (w *workflowInstance) collectRunningJobStatuses() []project_update_tags.JobStatus {
	jobStatuses := make([]project_update_tags.JobStatus, 0)
	if !w.IsRunning() {
		return jobStatuses
	}
	domainServicesUpdateInstance, err := w.GetUpdateDomainServicesInstance()
	if err != nil {
		return jobStatuses
	}

	for _, d := range domainServicesUpdateInstance.ListSubWorkflows() {
		subWorkflow, err := domainServicesUpdateInstance.GetSubWorkflow(d)
		if err != nil {
			logrus.WithError(err).Errorf("failed to get subworkflow for %q", d)
			continue
		}
		payload := project_update_tags.DomainProjectUpdateWorkflowPayload{}
		if subWorkflow.IsRunning() {
			if d == serialSubworkflowName {
				state := project_update_tags.SerializedProjectUpdateWorkflowState{}
				if err := subWorkflow.GetPayload(&state); err != nil {
					logrus.WithError(err).Errorf("failed to get payload for %q", d)
					continue
				}
				if state.Running.TotalTasks > 0 &&
					!state.Running.StartTime.IsZero() && !state.Running.LastUpdated.IsZero() {
					percentComplete := (float64(state.Running.CompletedTasks) + float64(state.Running.RunningTask.LastStatus.PercentageComplete)) / float64(state.Running.TotalTasks)
					if percentComplete == 0 {
						continue
					}
					elapsed := state.Running.LastUpdated.Sub(state.Running.StartTime)
					expectedDuration := time.Duration(float64(elapsed) / percentComplete)
					jobStatuses = append(jobStatuses, project_update_tags.JobStatus{
						Completed:             false,
						PercentageComplete:    float32(percentComplete),
						EstimatedEndTimeInSec: state.Running.StartTime.Add(expectedDuration).Unix(),
					})
				}
			} else {
				if err := subWorkflow.GetPayload(&payload); err != nil {
					logrus.WithError(err).Errorf("failed to get payload for %q", d)
					continue
				}
				jobStatuses = append(jobStatuses, payload.MergedJobStatus)
			}

		}
	}

	return jobStatuses
}
