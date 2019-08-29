package patterns

import (
	"context"
	"encoding/json"
	"errors"

	"github.com/chef/automate/lib/cereal"
)

/*
A parallel workflow is a workflow that combines smaller workflows to run
in parallel. It completes when all subworkflows have completed or failed.
When executed by cereal, the workflows execute under the same workflow instance.
This allows breaking logic apart in small, concise workflows. The parallel
workflow should be used when many things need to happen and they are independent
of each other.

Limitations:
- Tasks launched by the subworkflows must have struct or nil parameters
- When a subworkflow completes, enqueued/running tasks will not be canceled
  until the end of all subworkflows
*/

// ParallelWorkflowExecutorFor is a function used to look up the workflow executor for
// a given subworkflow
type ParallelWorkflowExecutorFor func(subworkflow string) (cereal.WorkflowExecutor, bool)

type ParallelWorkflowExecutor struct {
	executorForFunc ParallelWorkflowExecutorFor
}

// ParallelWorkflowPayload keeps track of the state of all the subworkflows and is the
// payload and result of the parallel workflow
type ParallelWorkflowPayload struct {
	State map[string]WorkflowState
}

// ParallelWorkflowParams are the parameters for the parallel workflow. It consists of
// the names of the subworkflows and their parameters.
type ParallelWorkflowParams struct {
	SubworkflowKeys []string
	WorkflowParams  map[string]json.RawMessage
}

// ParallelWorkflowTaskParam is metadata attached to any enqueued tasks. This allows
// the parallel workflow to figure out which subworkflow to deliver the task completed
// event to.
type ParallelWorkflowTaskParam struct {
	XXX_ParallelWorkflowKey string `json:"__key"`
}

// NewParallelWorkflowExecutor creates a parallel workflow executor. The executorForFunc is used to
// lookup the workflow executor for a given subworkflow
func NewParallelWorkflowExecutor(executorForFunc ParallelWorkflowExecutorFor) *ParallelWorkflowExecutor {
	return &ParallelWorkflowExecutor{
		executorForFunc: executorForFunc,
	}
}

func ToParallelWorkfowParameters(subworkflows []string, parameters map[string]interface{}) (ParallelWorkflowParams, error) {
	transformedParams := ParallelWorkflowParams{
		SubworkflowKeys: subworkflows,
		WorkflowParams:  map[string]json.RawMessage{},
	}
	for key, val := range parameters {
		transformedVal, err := json.Marshal(val)
		if err != nil {
			return transformedParams, err
		}
		transformedParams.WorkflowParams[key] = transformedVal
	}
	return transformedParams, nil
}

func EnqueueParallelWorkflow(ctx context.Context, m *cereal.Manager, workflowName string, instanceName string, subworkflows []string, parameters map[string]interface{}) error {
	transformedParams, err := ToParallelWorkfowParameters(subworkflows, parameters)
	if err != nil {
		return err
	}
	return m.EnqueueWorkflow(ctx, workflowName, instanceName, transformedParams)
}

func (p ParallelWorkflowPayload) Finished() bool {
	for _, state := range p.State {
		if !state.IsFinished {
			return false
		}
	}
	return true
}

type ParallelWorkflowInstance struct {
	payload   *ParallelWorkflowPayload
	result    *ParallelWorkflowPayload
	params    *ParallelWorkflowParams
	isRunning bool
	err       error
}

type immutableWorkflowInstanceImpl struct {
	params json.RawMessage
	state  WorkflowState
}

func (instance *immutableWorkflowInstanceImpl) GetParameters(obj interface{}) error {
	if len(instance.params) > 0 {
		return json.Unmarshal(instance.params, obj)
	}
	return nil
}

func (instance *immutableWorkflowInstanceImpl) GetPayload(obj interface{}) error {
	if instance.state.Err != "" {
		return errors.New(instance.state.Err)
	}
	if len(instance.state.Payload) > 0 {
		return json.Unmarshal(instance.state.Payload, obj)
	}
	return nil
}

func (instance *immutableWorkflowInstanceImpl) GetResult(obj interface{}) error {
	if instance.state.Err != "" {
		return errors.New(instance.state.Err)
	}
	if len(instance.state.Result) > 0 {
		return json.Unmarshal(instance.state.Result, obj)
	}
	return nil
}

func (instance *immutableWorkflowInstanceImpl) IsRunning() bool {
	return !instance.state.IsFinished
}

func (instance *immutableWorkflowInstanceImpl) Err() error {
	if instance.state.Err != "" {
		return errors.New(instance.state.Err)
	}
	return nil
}

func (instance *ParallelWorkflowInstance) GetSubWorkflow(workflowName string) (cereal.ImmutableWorkflowInstance, error) {
	payload := instance.payload
	if !instance.isRunning {
		payload = instance.result
	}

	// OnStart has not run yet
	if payload == nil {
		return nil, cereal.ErrWorkflowInstanceNotFound
	}

	v, ok := payload.State[workflowName]
	if !ok {
		return nil, cereal.ErrWorkflowInstanceNotFound
	}
	var params json.RawMessage
	if instance.params.WorkflowParams != nil {
		params = instance.params.WorkflowParams[workflowName]
	}

	subWorkflow := &immutableWorkflowInstanceImpl{
		state:  v,
		params: params,
	}

	return subWorkflow, nil
}

func (instance *ParallelWorkflowInstance) ListSubWorkflows() []string {
	if instance.params == nil {
		return []string{}
	}
	return instance.params.SubworkflowKeys
}

func (instance *ParallelWorkflowInstance) GetPayload() (*ParallelWorkflowPayload, error) {
	return instance.payload, nil
}

func (instance *ParallelWorkflowInstance) GetParameters() (*ParallelWorkflowParams, error) {
	return instance.params, nil
}

func (instance *ParallelWorkflowInstance) IsRunning() bool {
	return instance.isRunning
}

func (instance *ParallelWorkflowInstance) GetResult() (*ParallelWorkflowPayload, error) {
	return instance.result, nil
}

func (instance *ParallelWorkflowInstance) Err() error {
	return instance.err
}

func ToParallelWorkflowInstance(instance cereal.ImmutableWorkflowInstance) (*ParallelWorkflowInstance, error) {
	parallelInstance := ParallelWorkflowInstance{}
	if err := instance.Err(); err != nil {
		parallelInstance.err = err
	} else {
		if instance.IsRunning() {
			payload := &ParallelWorkflowPayload{}
			if err := instance.GetPayload(&payload); err != nil {
				return nil, err
			}
			parallelInstance.payload = payload
		} else {
			result := &ParallelWorkflowPayload{}
			if err := instance.GetResult(&result); err != nil {
				return nil, err
			}
			parallelInstance.result = result
		}
	}
	parallelInstance.isRunning = instance.IsRunning()

	params := ParallelWorkflowParams{}
	if err := instance.GetParameters(&params); err != nil {
		return nil, err
	}
	parallelInstance.params = &params
	return &parallelInstance, nil
}

func GetWorkflowInstance(ctx context.Context, m *cereal.Manager, workflowName string, instanceName string) (*ParallelWorkflowInstance, error) {
	instance, err := m.GetWorkflowInstanceByName(ctx, instanceName, workflowName)
	if err != nil {
		return nil, err
	}
	return ToParallelWorkflowInstance(instance)
}

func (m *ParallelWorkflowExecutor) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	instances := make(map[string]*workflowInstance)
	parameters := ParallelWorkflowParams{
		WorkflowParams: make(map[string]json.RawMessage),
	}

	if err := w.GetParameters(&parameters); err != nil {
		return w.Fail(err)
	}

	nextPayload := ParallelWorkflowPayload{
		State: make(map[string]WorkflowState),
	}

	for _, key := range parameters.SubworkflowKeys {
		var subWorkflowParams json.RawMessage
		if parameters.WorkflowParams != nil {
			subWorkflowParams = parameters.WorkflowParams[key]
		}
		instance := &workflowInstance{
			attachment: ParallelWorkflowTaskParam{
				XXX_ParallelWorkflowKey: key,
			},
			w:          w,
			parameters: subWorkflowParams,
		}
		instances[key] = instance
		executor, ok := m.executorForFunc(key)
		if !ok {
			return w.Fail(ErrTaskWorkflowInvalid)
		}
		decision := executor.OnStart(instance, ev)
		ns := nextState(instance, decision)
		nextPayload.State[key] = ns
	}
	if nextPayload.Finished() {
		return w.Complete(cereal.WithResult(nextPayload))
	}
	return w.Continue(nextPayload)
}

func (m *ParallelWorkflowExecutor) OnTaskComplete(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
	parallelWorkflowTaskParams := ParallelWorkflowTaskParam{}
	parameters := ParallelWorkflowParams{
		WorkflowParams: make(map[string]json.RawMessage),
	}

	if err := ev.Result.GetParameters(&parallelWorkflowTaskParams); err != nil {
		return w.Fail(err)
	}

	payload := ParallelWorkflowPayload{
		State: make(map[string]WorkflowState),
	}
	if err := w.GetPayload(&payload); err != nil {
		return w.Fail(err)
	}
	if err := w.GetParameters(&parameters); err != nil {
		return w.Fail(err)
	}
	key := parallelWorkflowTaskParams.XXX_ParallelWorkflowKey
	workflowState, ok := payload.State[key]
	if !ok {
		return w.Fail(ErrTaskWorkflowInvalid)
	}

	workflowState.CompletedTasks++
	if !workflowState.IsFinished {
		executor, ok := m.executorForFunc(key)
		if !ok {
			return w.Fail(ErrTaskWorkflowInvalid)
		}
		var subWorkflowParams json.RawMessage
		if parameters.WorkflowParams != nil {
			subWorkflowParams = parameters.WorkflowParams[key]
		}
		instance := &workflowInstance{
			attachment: ParallelWorkflowTaskParam{
				XXX_ParallelWorkflowKey: key,
			},
			w:          w,
			lastState:  &workflowState,
			parameters: subWorkflowParams,
		}
		decision := executor.OnTaskComplete(instance, ev)
		ns := nextState(instance, decision)
		payload.State[key] = ns
	} else {
		payload.State[key] = workflowState
	}

	if payload.Finished() {
		return w.Complete(cereal.WithResult(payload))
	}
	return w.Continue(payload)
}

func (m *ParallelWorkflowExecutor) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	payload := ParallelWorkflowPayload{
		State: make(map[string]WorkflowState),
	}
	if err := w.GetPayload(&payload); err != nil {
		return w.Fail(err)
	}
	parameters := ParallelWorkflowParams{
		WorkflowParams: make(map[string]json.RawMessage),
	}

	if err := w.GetParameters(&parameters); err != nil {
		return w.Fail(err)
	}
	nextPayload := ParallelWorkflowPayload{
		State: make(map[string]WorkflowState),
	}

	for key, state := range payload.State {
		var subWorkflowParams json.RawMessage
		if parameters.WorkflowParams != nil {
			subWorkflowParams = parameters.WorkflowParams[key]
		}
		// Don't remove this copy. the for loop reuses state
		subWorkflowState := state

		if subWorkflowState.IsFinished {
			nextPayload.State[key] = subWorkflowState
			continue
		}

		executor, ok := m.executorForFunc(key)
		if !ok {
			return w.Fail(ErrTaskWorkflowInvalid)
		}

		instance := &workflowInstance{
			attachment: ParallelWorkflowTaskParam{
				XXX_ParallelWorkflowKey: key,
			},
			w:          w,
			lastState:  &subWorkflowState,
			parameters: subWorkflowParams,
		}
		decision := executor.OnCancel(instance, ev)
		ns := nextState(instance, decision)
		nextPayload.State[key] = ns
	}
	if nextPayload.Finished() {
		return w.Complete(cereal.WithResult(nextPayload))
	}
	return w.Continue(nextPayload)
}
