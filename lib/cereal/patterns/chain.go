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

func marshal(idx int64, obj interface{}) (interface{}, error) {
	logrus.Infof("marshal idx %d", idx)
	spew.Dump(obj)
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

	spew.Dump(v.Interface())
	return v.Interface(), nil
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
	parameters interface{}
	opts       []cereal.TaskEnqueueOpts
}

func (w *workflowInstance) EnqueueTask(taskName string, parameters interface{}, opts ...cereal.TaskEnqueueOpts) error {
	v, err := marshal(w.idx, parameters)
	if err != nil {
		return err
	}

	w.enqueuedTasks = append(w.enqueuedTasks, enqueueTaskRequest{
		taskName:   taskName,
		parameters: v,
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

	subWorkflow := &immutableWorkflowInstanceImpl{
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
