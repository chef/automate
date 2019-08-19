package patterns

import (
	"context"
	"encoding/json"
	"errors"
	"reflect"

	"github.com/sirupsen/logrus"

	"github.com/davecgh/go-spew/spew"

	"github.com/chef/automate/lib/cereal"
)

type WorkflowState struct {
	Payload        json.RawMessage
	Result         json.RawMessage
	Err            string
	EnqueuedTasks  int
	CompletedTasks int
	IsFinished     bool
}

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

func marshal(idx int64, obj interface{}) (json.RawMessage, error) {
	structFields := []reflect.StructField{}
	if obj != nil {
		structFields = append(structFields, reflect.StructField{
			Name:      "Anonymous",
			Type:      reflect.TypeOf(obj),
			Anonymous: true,
		})
	}
	structFields = append(structFields, reflect.StructField{
		Name: "ChainWorkflowTaskParam",
		Type: reflect.TypeOf(idx),
		Tag:  "json:\"__idx\"",
	})
	newType := reflect.StructOf(structFields)

	v := reflect.New(newType).Elem()
	if obj != nil {
		v.Field(0).Set(reflect.ValueOf(obj))
	}
	v.FieldByName("ChainWorkflowTaskParam").SetInt(idx)
	return json.Marshal(v.Interface())
}

type workflowInstance struct {
	idx        int64
	w          cereal.WorkflowInstance
	lastState  *WorkflowState
	parameters json.RawMessage

	// output
	enqueuedTasks []enqueueTaskRequest
}

func (w *workflowInstance) GetPayload(obj interface{}) error {
	if w.lastState == nil || len(w.lastState.Payload) == 0 {
		return nil
	}
	return json.Unmarshal(w.lastState.Payload, obj)
}
func (w *workflowInstance) GetParameters(obj interface{}) error {
	if len(w.parameters) == 0 {
		return nil
	}
	return json.Unmarshal(w.parameters, obj)
}

type enqueueTaskRequest struct {
	taskName   string
	parameters json.RawMessage
	opts       []cereal.TaskEnqueueOpts
}

func (w *workflowInstance) EnqueueTask(taskName string, parameters interface{}, opts ...cereal.TaskEnqueueOpts) error {
	jsonVal, err := marshal(w.idx, parameters)
	if err != nil {
		return err
	}

	w.enqueuedTasks = append(w.enqueuedTasks, enqueueTaskRequest{
		taskName:   taskName,
		parameters: jsonVal,
		opts:       opts,
	})
	return nil
}
func (w *workflowInstance) Continue(payload interface{}) cereal.Decision {
	return w.w.Continue(payload)
}

func (w *workflowInstance) Complete(opts ...cereal.CompleteOpts) cereal.Decision {
	return w.w.Complete(opts...)
}

func (w *workflowInstance) Fail(err error) cereal.Decision {
	return w.w.Fail(err)
}

func (w *workflowInstance) InstanceName() string {
	return ""
}

func (w *workflowInstance) TotalEnqueuedTasks() int {
	if w.lastState == nil {
		return len(w.enqueuedTasks)
	}
	return len(w.enqueuedTasks) + w.lastState.EnqueuedTasks
}
func (w *workflowInstance) TotalCompletedTasks() int {
	if w.lastState == nil {
		return 0
	}
	return w.lastState.CompletedTasks
}

func NewChainWorkflowExecutor(executors []cereal.WorkflowExecutor) (*ChainWorkflowExecutor, error) {
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
		idx:        0,
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

func applyDecision(instance *workflowInstance, decision cereal.Decision) WorkflowState {
	if decision.IsContinuing() {
		for _, enq := range instance.enqueuedTasks {
			err := instance.w.EnqueueTask(enq.taskName, enq.parameters, enq.opts...)
			if err != nil {
				return WorkflowState{
					IsFinished:     true,
					Err:            err.Error(),
					EnqueuedTasks:  instance.TotalEnqueuedTasks(),
					CompletedTasks: instance.TotalCompletedTasks(),
				}
			}
		}
		var payload json.RawMessage
		var err error
		payload, err = json.Marshal(decision.Payload())
		if err != nil {
			return WorkflowState{
				IsFinished:     true,
				Err:            err.Error(),
				EnqueuedTasks:  instance.TotalEnqueuedTasks(),
				CompletedTasks: instance.TotalCompletedTasks()}
		}
		return WorkflowState{
			IsFinished:     false,
			Payload:        payload,
			EnqueuedTasks:  instance.TotalEnqueuedTasks(),
			CompletedTasks: instance.TotalCompletedTasks(),
		}
	} else if decision.IsFailed() {
		var err string
		if decision.Err() == nil {
			err = "failed"
		} else {
			err = decision.Err().Error()
		}
		return WorkflowState{
			IsFinished: true,
			Err:        err,
		}
	} else if decision.IsComplete() {
		var result json.RawMessage
		var err error
		result, err = json.Marshal(decision.Result())
		if err != nil {
			return WorkflowState{
				IsFinished:     true,
				Err:            err.Error(),
				EnqueuedTasks:  instance.TotalEnqueuedTasks(),
				CompletedTasks: instance.TotalCompletedTasks(),
			}
		}
		return WorkflowState{
			IsFinished:     true,
			Result:         result,
			EnqueuedTasks:  instance.TotalEnqueuedTasks(),
			CompletedTasks: instance.TotalCompletedTasks(),
		}
	}
	return WorkflowState{
		IsFinished: true,
		Err:        "Unknown workflow state",
	}
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

	wrappedInstance := &workflowInstance{
		idx:        idx,
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
				idx:        nextIdx,
				w:          w,
				parameters: parameters.WorkflowParams[nextIdx],
			}

			decision := m.executors[nextIdx].OnStart(wrappedInstance, cereal.StartEvent{})
			ns := applyDecision(wrappedInstance, decision)
			if int64(len(payload.State)) != nextIdx {
				panic("incorrect length of state")
			}
			payload.State = append(payload.State, ns)
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
