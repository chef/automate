package patterns

import (
	"encoding/json"
	"errors"
	"fmt"
	"reflect"

	"github.com/mitchellh/mapstructure"

	"github.com/chef/automate/lib/cereal"
)

var ErrCannotMergeTypes = errors.New("Cannot merge types")

type WorkflowState struct {
	Payload        json.RawMessage
	Result         json.RawMessage
	Err            string
	EnqueuedTasks  int
	CompletedTasks int
	IsFinished     bool
}

type workflowInstance struct {
	attachment interface{}
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
	v, err := merge(parameters, w.attachment)
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
	return w.w.InstanceName()
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

type immutableWorkflowInstance struct {
	params json.RawMessage
	state  WorkflowState
}

func (instance *immutableWorkflowInstance) GetParameters(obj interface{}) error {
	if len(instance.params) > 0 {
		return json.Unmarshal(instance.params, obj)
	}
	return nil
}

func (instance *immutableWorkflowInstance) GetPayload(obj interface{}) error {
	if instance.state.Err != "" {
		return errors.New(instance.state.Err)
	}
	if len(instance.state.Payload) > 0 {
		return json.Unmarshal(instance.state.Payload, obj)
	}
	return nil
}

func (instance *immutableWorkflowInstance) GetResult(obj interface{}) error {
	if instance.state.Err != "" {
		return errors.New(instance.state.Err)
	}
	if len(instance.state.Result) > 0 {
		return json.Unmarshal(instance.state.Result, obj)
	}
	return nil
}

func (instance *immutableWorkflowInstance) IsRunning() bool {
	return !instance.state.IsFinished
}

func (instance *immutableWorkflowInstance) Err() error {
	if instance.state.Err != "" {
		return errors.New(instance.state.Err)
	}
	return nil
}

func asMapInterface(rawJson json.RawMessage) (map[string]interface{}, error) {
	var aMap map[string]interface{}
	if rawJson == nil {
		return map[string]interface{}{}, nil
	}
	if err := json.Unmarshal(rawJson, &aMap); err != nil {
		return nil, err
	}
	return aMap, nil
}

func isNil(a interface{}) bool {
	return (reflect.ValueOf(a).Kind() == reflect.Ptr && reflect.ValueOf(a).IsNil())
}

func decode(input interface{}, output interface{}) error {
	config := &mapstructure.DecoderConfig{
		Metadata: nil,
		Result:   output,
		TagName:  "json",
	}

	decoder, err := mapstructure.NewDecoder(config)
	if err != nil {
		return err
	}

	return decoder.Decode(input)
}

func merge(a interface{}, b interface{}) (interface{}, error) {
	v := map[string]interface{}{}
	if a != nil && !isNil(a) {
		if rawA, ok := a.(json.RawMessage); ok {
			var err error
			a, err = asMapInterface(rawA)
			if err != nil {
				return nil, err
			}
		}

		if err := decode(a, &v); err != nil {
			return nil, err
		}
	}

	if b != nil && !isNil(b) {
		if rawB, ok := b.(json.RawMessage); ok {
			var err error
			b, err = asMapInterface(rawB)
			if err != nil {
				return nil, err
			}
		}

		if err := decode(b, &v); err != nil {
			return nil, err
		}
	}

	return v, nil
}

func nextState(instance *workflowInstance, decision cereal.Decision) WorkflowState {
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
	} else {
		if len(instance.enqueuedTasks) > 0 {
			instance.enqueuedTasks = nil
		}
		if decision.IsFailed() {
			var err string
			if decision.Err() == nil {
				err = "failed"
			} else {
				err = decision.Err().Error()
			}
			return WorkflowState{
				IsFinished:     true,
				Err:            err,
				EnqueuedTasks:  instance.TotalEnqueuedTasks(),
				CompletedTasks: instance.TotalCompletedTasks(),
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
	}
	return WorkflowState{
		IsFinished:     true,
		Err:            "Unknown workflow state",
		EnqueuedTasks:  instance.TotalEnqueuedTasks(),
		CompletedTasks: instance.TotalCompletedTasks(),
	}
}

func bug(w cereal.WorkflowInstance, msg string) cereal.Decision {
	return w.Fail(fmt.Errorf("BUG: %s", msg))
}
