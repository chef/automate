package multiworkflow

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"reflect"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/cereal"
)

var ErrSubworkflowNotFound error = errors.New("subworkflow not found")

type MultiWorkflow struct {
	executorForFunc ExecutorFor
}

type WorkflowState struct {
	Payload        json.RawMessage
	Result         json.RawMessage
	Err            string
	EnqueuedTasks  int
	CompletedTasks int
	IsFinished     bool
}

type MultiWorkflowPayload struct {
	State map[string]WorkflowState
}

type MultiWorkflowParams struct {
	SubworkflowKeys []string
	WorkflowParams  map[string]json.RawMessage
}

type MultiWorkflowTaskParam struct {
	XXX_MultiWorkflowKey string `json:"__key"`
}

func marshal(key string, obj interface{}) (json.RawMessage, error) {
	structFields := []reflect.StructField{}
	if obj != nil {
		structFields = append(structFields, reflect.StructField{
			Name:      "Anonymous",
			Type:      reflect.TypeOf(obj),
			Anonymous: true,
		})
	}
	structFields = append(structFields, reflect.StructField{
		Name: "MultiWorkflowKey",
		Type: reflect.TypeOf(""),
		Tag:  "json:\"__key\"",
	})
	newType := reflect.StructOf(structFields)

	v := reflect.New(newType).Elem()
	if obj != nil {
		v.Field(0).Set(reflect.ValueOf(obj))
	}
	v.FieldByName("MultiWorkflowKey").SetString(key)
	return json.Marshal(v.Interface())
}

type workflowInstance struct {
	key        string
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
	jsonVal, err := marshal(w.key, parameters)
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

func (p MultiWorkflowPayload) Finished() bool {
	for _, state := range p.State {
		if !state.IsFinished {
			return false
		}
	}
	return true
}

func (m *MultiWorkflow) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	instances := make(map[string]*workflowInstance)
	logrus.Info("ONSTART")
	parameters := MultiWorkflowParams{
		WorkflowParams: make(map[string]json.RawMessage),
	}

	if err := w.GetParameters(&parameters); err != nil {
		return w.Fail(err)
	}

	nextPayload := MultiWorkflowPayload{
		State: make(map[string]WorkflowState),
	}

	for _, key := range parameters.SubworkflowKeys {
		var subWorkflowParams json.RawMessage
		if parameters.WorkflowParams != nil {
			subWorkflowParams = parameters.WorkflowParams[key]
		}
		instance := &workflowInstance{
			key:        key,
			w:          w,
			parameters: subWorkflowParams,
		}
		instances[key] = instance
		executor, ok := m.executorForFunc(key)
		if !ok {
			return w.Fail(fmt.Errorf("could not find workflow executor for %q", key))
		}
		decision := executor.OnStart(instance, ev)
		ns := applyDecision(instance, decision)
		nextPayload.State[key] = ns
	}
	if nextPayload.Finished() {
		return w.Complete(cereal.WithResult(nextPayload))
	}
	return w.Continue(nextPayload)
}

func (m *MultiWorkflow) OnTaskComplete(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
	multiWorkflowTaskParams := MultiWorkflowTaskParam{}
	parameters := MultiWorkflowParams{
		WorkflowParams: make(map[string]json.RawMessage),
	}

	if err := ev.Result.GetParameters(&multiWorkflowTaskParams); err != nil {
		return w.Fail(err)
	}

	payload := MultiWorkflowPayload{
		State: make(map[string]WorkflowState),
	}
	if err := w.GetPayload(&payload); err != nil {
		return w.Fail(err)
	}
	if err := w.GetParameters(&parameters); err != nil {
		return w.Fail(err)
	}
	key := multiWorkflowTaskParams.XXX_MultiWorkflowKey
	workflowState, ok := payload.State[key]
	if !ok {
		return w.Fail(fmt.Errorf("could not find workflow state for %q", key))
	}

	if !workflowState.IsFinished {
		workflowState.CompletedTasks++
		executor, ok := m.executorForFunc(key)
		if !ok {
			return w.Fail(fmt.Errorf("could not find workflow executor for %q", key))
		}
		var subWorkflowParams json.RawMessage
		if parameters.WorkflowParams != nil {
			subWorkflowParams = parameters.WorkflowParams[key]
		}
		instance := &workflowInstance{
			key:        key,
			w:          w,
			lastState:  &workflowState,
			parameters: subWorkflowParams,
		}
		decision := executor.OnTaskComplete(instance, ev)
		ns := applyDecision(instance, decision)
		payload.State[key] = ns
	}

	if payload.Finished() {
		return w.Complete(cereal.WithResult(payload))
	}
	return w.Continue(payload)
}

func (m *MultiWorkflow) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	payload := MultiWorkflowPayload{
		State: make(map[string]WorkflowState),
	}
	if err := w.GetPayload(&payload); err != nil {
		return w.Fail(err)
	}
	parameters := MultiWorkflowParams{
		WorkflowParams: make(map[string]json.RawMessage),
	}

	if err := w.GetParameters(&parameters); err != nil {
		return w.Fail(err)
	}
	nextPayload := MultiWorkflowPayload{
		State: make(map[string]WorkflowState),
	}

	for key, state := range payload.State {
		var subWorkflowParams json.RawMessage
		if parameters.WorkflowParams != nil {
			subWorkflowParams = parameters.WorkflowParams[key]
		}
		// Don't remove this copy. the for loop reuses state
		subWorkflowState := state

		executor, ok := m.executorForFunc(key)
		if !ok {
			return w.Fail(fmt.Errorf("could not find workflow executor for %q", key))
		}

		instance := &workflowInstance{
			key:        key,
			w:          w,
			lastState:  &subWorkflowState,
			parameters: subWorkflowParams,
		}
		decision := executor.OnCancel(instance, ev)
		ns := applyDecision(instance, decision)
		nextPayload.State[key] = ns
	}
	if nextPayload.Finished() {
		return w.Complete(cereal.WithResult(nextPayload))
	}
	return w.Continue(nextPayload)
}

type ExecutorFor func(subworkflow string) (cereal.WorkflowExecutor, bool)

func NewMultiWorkflowExecutor(executorForFunc ExecutorFor) *MultiWorkflow {
	return &MultiWorkflow{
		executorForFunc: executorForFunc,
	}
}

func EnqueueWorkflow(ctx context.Context, m *cereal.Manager, workflowName string, instanceName string, subworkflows []string, parameters map[string]interface{}) error {
	transformedParams := MultiWorkflowParams{
		SubworkflowKeys: subworkflows,
		WorkflowParams:  map[string]json.RawMessage{},
	}

	for key, val := range parameters {
		transformedVal, err := json.Marshal(val)
		if err != nil {
			return err
		}
		transformedParams.WorkflowParams[key] = transformedVal
	}

	return m.EnqueueWorkflow(ctx, workflowName, instanceName, transformedParams)
}

type WorkflowInstance struct {
	payload   *MultiWorkflowPayload
	result    *MultiWorkflowPayload
	params    *MultiWorkflowParams
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

func (instance *WorkflowInstance) GetSubWorkflow(workflowName string) (cereal.ImmutableWorkflowInstance, error) {
	payload := instance.payload
	if !instance.isRunning {
		payload = instance.result
	}

	// OnStart has not run yet
	if payload == nil {
		return nil, ErrSubworkflowNotFound
	}

	v, ok := payload.State[workflowName]
	if !ok {
		return nil, ErrSubworkflowNotFound
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

func (instance *WorkflowInstance) GetPayload() (*MultiWorkflowPayload, error) {
	return instance.payload, nil
}

func (instance *WorkflowInstance) GetParameters() (*MultiWorkflowParams, error) {
	return instance.params, nil
}

func (instance *WorkflowInstance) IsRunning() bool {
	return instance.isRunning
}

func (instance *WorkflowInstance) GetResult() (*MultiWorkflowPayload, error) {
	return instance.result, nil
}

func (instance *WorkflowInstance) Err() error {
	return instance.err
}

func ToMultiWorkflowInstance(instance cereal.ImmutableWorkflowInstance) (*WorkflowInstance, error) {
	multiInstance := WorkflowInstance{}
	if err := instance.Err(); err != nil {
		multiInstance.err = err
	} else {
		if instance.IsRunning() {
			payload := &MultiWorkflowPayload{}
			if err := instance.GetPayload(&payload); err != nil {
				return nil, err
			}
			multiInstance.payload = payload
		} else {
			result := &MultiWorkflowPayload{}
			if err := instance.GetResult(&result); err != nil {
				return nil, err
			}
			multiInstance.result = result
		}
	}
	multiInstance.isRunning = instance.IsRunning()

	params := MultiWorkflowParams{}
	if err := instance.GetParameters(&params); err != nil {
		return nil, err
	}
	multiInstance.params = &params
	return &multiInstance, nil
}

func GetWorkflowInstance(ctx context.Context, m *cereal.Manager, workflowName string, instanceName string) (*WorkflowInstance, error) {
	instance, err := m.GetWorkflowInstanceByName(ctx, instanceName, workflowName)
	if err != nil {
		return nil, err
	}
	return ToMultiWorkflowInstance(instance)
}
