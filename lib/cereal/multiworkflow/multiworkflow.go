package multiworkflow

import (
	"context"
	"encoding/json"
	"fmt"
	"reflect"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/cereal"
)

type MultiWorkflow struct {
	executors map[string]cereal.WorkflowExecutor
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
	WorkflowParams map[string]json.RawMessage
}

type MultiWorkflowTaskParam struct {
	XXX_MultiWorkflowKey string `json:"__key"`
}

func marshal(key string, obj interface{}) (json.RawMessage, error) {
	newType := reflect.StructOf([]reflect.StructField{
		{
			Name:      "Anonymous",
			Type:      reflect.TypeOf(obj),
			Anonymous: true,
		},
		{
			Name: "MultiWorkflowKey",
			Type: reflect.TypeOf(""),
			Tag:  "json:\"__key\"",
		},
	})

	v := reflect.New(newType).Elem()
	v.Field(0).Set(reflect.ValueOf(obj))
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
				logrus.WithError(err).Error("failed to enqueue task")
				return WorkflowState{
					IsFinished: true,
					Err:        err.Error(),
				}
			}
		}
		var payload json.RawMessage
		var err error
		payload, err = json.Marshal(decision.Payload())
		if err != nil {
			return WorkflowState{
				IsFinished: true,
				Err:        err.Error(),
			}
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
				IsFinished: true,
				Err:        err.Error(),
			}
		}
		return WorkflowState{
			IsFinished: true,
			Result:     result,
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
	parameters := MultiWorkflowParams{
		WorkflowParams: make(map[string]json.RawMessage),
	}

	if err := w.GetParameters(&parameters); err != nil {
		return w.Fail(err)
	}
	nextPayload := MultiWorkflowPayload{
		State: make(map[string]WorkflowState),
	}

	for key, executor := range m.executors {
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
		executor, ok := m.executors[key]
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

func (*MultiWorkflow) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	return w.Complete()
}

func NewMultiWorkflowExecutor(executors map[string]cereal.WorkflowExecutor) *MultiWorkflow {
	return &MultiWorkflow{
		executors: executors,
	}
}

func EnqueueWorkflow(ctx context.Context, m *cereal.Manager, workflowName string, instanceName string, parameters map[string]interface{}) error {
	transformedParams := MultiWorkflowParams{
		WorkflowParams: map[string]json.RawMessage{},
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
