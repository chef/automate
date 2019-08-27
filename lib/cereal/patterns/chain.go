package patterns

import (
	"context"
	"encoding/json"
	"errors"

	"github.com/chef/automate/lib/cereal"
)

var ErrIncorrectParameters = errors.New("Incorrect number of parameters given")
var ErrTaskWorkflowInvalid = errors.New("Could not determine workflow for task")

type ChainWorkflowParams struct {
	WorkflowParams []json.RawMessage
}

type ChainWorkflowPayload struct {
	Canceled bool
	State    []WorkflowState
}

func (p *ChainWorkflowPayload) IsValid() bool {
	return len(p.State) > 0
}

func (p *ChainWorkflowPayload) Finished() bool {
	for _, state := range p.State {
		if !state.IsFinished {
			return false
		}
	}
	return true
}

type ChainWorkflowExecutor struct {
	executors []cereal.WorkflowExecutor
}

type ChainWorkflowTaskParam struct {
	XXX_ChainWorkflowIdx int64 `json:"__idx"`
}

func NewChainWorkflowExecutor(executors ...cereal.WorkflowExecutor) (*ChainWorkflowExecutor, error) {
	if len(executors) <= 0 {
		return nil, errors.New("Can't have zero")
	}
	return &ChainWorkflowExecutor{executors: executors}, nil
}

func ToChainWorkflowParameters(parameters []interface{}) (ChainWorkflowParams, error) {
	params := ChainWorkflowParams{}
	if len(parameters) == 0 {
		return params, errors.New("chain workflow parameters empty")
	}
	params.WorkflowParams = make([]json.RawMessage, len(parameters))
	for idx, subWorkflowParams := range parameters {
		jsonVal, err := json.Marshal(subWorkflowParams)
		if err != nil {
			return params, err
		}
		params.WorkflowParams[idx] = jsonVal
	}
	return params, nil
}

func EnqueueChainWorkflow(ctx context.Context, m *cereal.Manager, workflowName string, instanceName string, parameters []interface{}) error {
	params, err := ToChainWorkflowParameters(parameters)
	if err != nil {
		return err
	}
	return m.EnqueueWorkflow(ctx, workflowName, instanceName, &params)
}

func ToChainWorkflowInstance(instance cereal.ImmutableWorkflowInstance) (*ChainWorkflowInstance, error) {
	chainInstance := ChainWorkflowInstance{}
	if err := instance.Err(); err != nil {
		chainInstance.err = err
	} else {
		if instance.IsRunning() {
			payload := &ChainWorkflowPayload{}
			if err := instance.GetPayload(&payload); err != nil {
				return nil, err
			}
			chainInstance.payload = payload
		} else {
			result := &ChainWorkflowPayload{}
			if err := instance.GetResult(&result); err != nil {
				return nil, err
			}
			chainInstance.result = result
		}
	}
	chainInstance.isRunning = instance.IsRunning()

	params := ChainWorkflowParams{}
	if err := instance.GetParameters(&params); err != nil {
		return nil, err
	}
	chainInstance.params = &params
	return &chainInstance, nil
}

func GetChainWorkflowInstance(ctx context.Context, m *cereal.Manager, workflowName string, instanceName string) (*ChainWorkflowInstance, error) {
	instance, err := m.GetWorkflowInstanceByName(ctx, instanceName, workflowName)
	if err != nil {
		return nil, err
	}
	return ToChainWorkflowInstance(instance)
}

type ChainWorkflowInstance struct {
	payload   *ChainWorkflowPayload
	result    *ChainWorkflowPayload
	params    *ChainWorkflowParams
	isRunning bool
	err       error
}

func (instance *ChainWorkflowInstance) GetSubWorkflow(idx int) (cereal.ImmutableWorkflowInstance, error) {
	if instance.err != nil {
		return nil, instance.err
	}
	payload := instance.payload
	if !instance.isRunning {
		payload = instance.result
	}

	if payload == nil || idx >= len(payload.State) {
		return nil, cereal.ErrWorkflowInstanceNotFound
	}

	v := payload.State[idx]
	var params json.RawMessage
	if instance.params.WorkflowParams != nil {
		params = instance.params.WorkflowParams[idx]
	}

	subWorkflow := &immutableWorkflowInstance{
		state:  v,
		params: params,
	}

	return subWorkflow, nil
}

func (instance *ChainWorkflowInstance) GetPayload() (*ChainWorkflowPayload, error) {
	return instance.payload, instance.err
}

func (instance *ChainWorkflowInstance) GetParameters() (*ChainWorkflowParams, error) {
	return instance.params, nil
}

func (instance *ChainWorkflowInstance) IsRunning() bool {
	return instance.isRunning
}

func (instance *ChainWorkflowInstance) GetResult() (*ChainWorkflowPayload, error) {
	return instance.result, instance.err
}

func (instance *ChainWorkflowInstance) Err() error {
	return instance.err
}

func (m *ChainWorkflowExecutor) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	parameters, err := m.getParameters(w)
	if err != nil {
		return w.Fail(err)
	}

	wrappedInstance := &workflowInstance{
		attachment: ChainWorkflowTaskParam{
			XXX_ChainWorkflowIdx: 0,
		},
		w:          w,
		parameters: parameters.WorkflowParams[0],
	}

	decision := m.executors[0].OnStart(wrappedInstance, ev)
	ns := nextState(wrappedInstance, decision)
	payload := ChainWorkflowPayload{
		State: []WorkflowState{ns},
	}

	if decision.IsFailed() {
		if !ns.IsFinished {
			return bug(w, "expected next state to be finished")
		}
		return w.Complete(cereal.WithResult(payload))
	} else if decision.IsComplete() {
		return m.startNext(w, 0, parameters, payload)
	}

	return w.Continue(payload)
}

func (m *ChainWorkflowExecutor) getParameters(w cereal.WorkflowInstance) (ChainWorkflowParams, error) {
	parameters := ChainWorkflowParams{}
	if err := w.GetParameters(&parameters); err != nil {
		return parameters, err
	}

	if len(m.executors) != len(parameters.WorkflowParams) {
		return parameters, ErrIncorrectParameters
	}
	return parameters, nil
}

func (m *ChainWorkflowExecutor) OnTaskComplete(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
	taskParameters := ChainWorkflowTaskParam{}
	if err := ev.Result.GetParameters(&taskParameters); err != nil {
		return w.Fail(err)
	}

	parameters, err := m.getParameters(w)
	if err != nil {
		return w.Fail(err)
	}

	payload := ChainWorkflowPayload{}
	if err := w.GetPayload(&payload); err != nil {
		return w.Fail(err)
	}

	idx := taskParameters.XXX_ChainWorkflowIdx

	if !payload.IsValid() || int(idx) >= len(payload.State) || int(idx) >= len(m.executors) {
		return w.Fail(ErrTaskWorkflowInvalid)
	}

	workflowState := payload.State[idx]
	workflowState.CompletedTasks++
	payload.State[idx] = workflowState
	if workflowState.IsFinished {
		return w.Continue(payload)
	}

	wrappedInstance := &workflowInstance{
		attachment: ChainWorkflowTaskParam{
			XXX_ChainWorkflowIdx: idx,
		},
		w:          w,
		parameters: parameters.WorkflowParams[idx],
		lastState:  &workflowState,
	}

	decision := m.executors[idx].OnTaskComplete(wrappedInstance, ev)
	ns := nextState(wrappedInstance, decision)
	payload.State[idx] = ns

	if decision.IsFailed() {
		if !ns.IsFinished {
			return bug(w, "expected next state to be finished")
		}
		return w.Complete(cereal.WithResult(payload))
	} else if decision.IsComplete() {
		return m.startNext(w, idx, parameters, payload)
	}

	return w.Continue(payload)
}

func (m *ChainWorkflowExecutor) startNext(w cereal.WorkflowInstance, idx int64, parameters ChainWorkflowParams, payload ChainWorkflowPayload) cereal.Decision {
	// The chain will stop when canceled
	if payload.Canceled {
		return w.Complete(cereal.WithResult(payload))
	}
	nextIdx := idx + 1
	if nextIdx < int64(len(m.executors)) {
		wrappedInstance := &workflowInstance{
			attachment: ChainWorkflowTaskParam{
				XXX_ChainWorkflowIdx: nextIdx,
			},
			w:          w,
			parameters: parameters.WorkflowParams[nextIdx],
		}

		decision := m.executors[nextIdx].OnStart(wrappedInstance, cereal.StartEvent{})
		ns := nextState(wrappedInstance, decision)
		if int64(len(payload.State)) != nextIdx {
			return bug(w, "incorrect length of state")
		}
		payload.State = append(payload.State, ns)
		if decision.IsFailed() {
			return w.Complete(cereal.WithResult(payload))
		} else if decision.IsComplete() {
			return m.startNext(w, nextIdx, parameters, payload)
		}
		return w.Continue(payload)
	} else {
		return w.Complete(cereal.WithResult(payload))
	}
}

func (m *ChainWorkflowExecutor) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	parameters, err := m.getParameters(w)
	if err != nil {
		return w.Fail(err)
	}

	payload := ChainWorkflowPayload{}
	if err := w.GetPayload(&payload); err != nil {
		return w.Fail(err)
	}
	payload.Canceled = true
	idx := len(payload.State) - 1

	if !payload.IsValid() || idx >= len(payload.State) || idx >= len(m.executors) {
		return w.Fail(ErrTaskWorkflowInvalid)
	}

	workflowState := payload.State[idx]

	wrappedInstance := &workflowInstance{
		attachment: ChainWorkflowTaskParam{
			XXX_ChainWorkflowIdx: int64(idx),
		},
		w:          w,
		parameters: parameters.WorkflowParams[idx],
		lastState:  &workflowState,
	}
	decision := m.executors[idx].OnCancel(wrappedInstance, ev)
	ns := nextState(wrappedInstance, decision)
	payload.State[idx] = ns

	if decision.IsFailed() || decision.IsComplete() {
		return w.Complete(cereal.WithResult(payload))
	} else {
		return w.Continue(payload)
	}
}
