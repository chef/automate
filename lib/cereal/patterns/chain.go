package patterns

import (
	"context"
	"encoding/json"
	"errors"

	"github.com/sirupsen/logrus"

	"github.com/davecgh/go-spew/spew"

	"github.com/chef/automate/lib/cereal"
)

type ChainWorkflowParams struct {
	WorkflowParams []json.RawMessage
}

type ChainWorkflowPayload struct {
	State []WorkflowState
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

func EnqueueChainWorkflow(ctx context.Context, m *cereal.Manager, workflowName string, instanceName string, parameters []interface{}) error {
	params := ChainWorkflowParams{}
	params.WorkflowParams = make([]json.RawMessage, len(parameters))
	for idx, subWorkflowParams := range parameters {
		jsonVal, err := json.Marshal(subWorkflowParams)
		if err != nil {
			return err
		}
		params.WorkflowParams[idx] = jsonVal
	}
	return m.EnqueueWorkflow(ctx, workflowName, instanceName, &params)

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
	ns := applyDecision(wrappedInstance, decision)
	payload := &ChainWorkflowPayload{
		State: []WorkflowState{ns},
	}

	// TODO (jaym): if the first workflow completes successfully without enqueuing
	// this logic is wrong and won't run the next thing
	if payload.Finished() {
		return w.Complete(cereal.WithResult(payload))
	}
	return w.Continue(payload)
}

func (m *ChainWorkflowExecutor) getParameters(w cereal.WorkflowInstance) (ChainWorkflowParams, error) {
	parameters := ChainWorkflowParams{}
	if err := w.GetParameters(&parameters); err != nil {
		return parameters, err
	}
	spew.Dump(parameters)
	spew.Dump(m)
	if len(m.executors) != len(parameters.WorkflowParams) {
		return parameters, errors.New("Incorrect length")
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

	spew.Dump(taskParameters)

	payload := ChainWorkflowPayload{}
	if err := w.GetPayload(&payload); err != nil {
		return w.Fail(err)
	}

	idx := taskParameters.XXX_ChainWorkflowIdx
	workflowState := payload.State[idx]
	workflowState.CompletedTasks++
	payload.State[idx] = workflowState
	if workflowState.IsFinished {
		return w.Continue(payload)
	}

	spew.Dump(workflowState)
	logrus.Infof("Delivering to %d", idx)
	wrappedInstance := &workflowInstance{
		attachment: ChainWorkflowTaskParam{
			XXX_ChainWorkflowIdx: idx,
		},
		w:          w,
		parameters: parameters.WorkflowParams[idx],
		lastState:  &workflowState,
	}

	decision := m.executors[idx].OnTaskComplete(wrappedInstance, ev)
	ns := applyDecision(wrappedInstance, decision)
	payload.State[idx] = ns

	if decision.IsFailed() {
		logrus.Info("FINISHED")
		// assert payload.IsFinished
		if !ns.IsFinished {
			panic("Noo")
		}
		return w.Complete(cereal.WithResult(payload))
	} else if decision.IsComplete() {
		logrus.Info("NOT FINISHED")

		nextIdx := idx + 1
		if nextIdx < int64(len(m.executors)) {
			wrappedInstance := &workflowInstance{
				attachment: ChainWorkflowTaskParam{
					XXX_ChainWorkflowIdx: nextIdx,
				},
				w:          w,
				parameters: parameters.WorkflowParams[nextIdx],
			}

			logrus.Infof("starting %d", nextIdx)
			spew.Dump(wrappedInstance.parameters)
			decision := m.executors[nextIdx].OnStart(wrappedInstance, cereal.StartEvent{})
			spew.Dump(decision)
			ns := applyDecision(wrappedInstance, decision)
			spew.Dump(ns)
			if int64(len(payload.State)) != nextIdx {
				panic("incorrect length of state")
			}
			payload.State = append(payload.State, ns)
			spew.Dump(payload)
			// this is wrong if OnStart completes
			return w.Continue(payload)
		} else {
			logrus.Info("NOT FINISHED BUT FAILED")
			return w.Complete(cereal.WithResult(payload))
		}
	}

	return w.Continue(payload)
}

func (*ChainWorkflowExecutor) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	return w.Complete()
}

type ChainWorkflowInstance struct {
	payload   *ChainWorkflowPayload
	result    *ChainWorkflowPayload
	params    *ChainWorkflowParams
	isRunning bool
	err       error
}

func (instance *ChainWorkflowInstance) GetSubWorkflow(idx int) (cereal.ImmutableWorkflowInstance, error) {
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
	return instance.payload, nil
}

func (instance *ChainWorkflowInstance) GetParameters() (*ChainWorkflowParams, error) {
	return instance.params, nil
}

func (instance *ChainWorkflowInstance) IsRunning() bool {
	return instance.isRunning
}

func (instance *ChainWorkflowInstance) GetResult() (*ChainWorkflowPayload, error) {
	return instance.result, nil
}

func (instance *ChainWorkflowInstance) Err() error {
	return instance.err
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
