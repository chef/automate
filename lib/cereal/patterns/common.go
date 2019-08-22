package patterns

import (
	"encoding/json"
	"errors"
	"fmt"
	"reflect"

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

type TaskParameterMetadata map[string]interface{}

func isEmbeddable(t reflect.Type) bool {
	fmt.Println(t.String())
	switch t.Kind() {
	case reflect.Struct:
		return true
	case reflect.Ptr:
		if t.Elem().Kind() == reflect.Ptr {
			return false
		}
		return isEmbeddable(t.Elem())
	default:
		return false
	}
}

func merge(a interface{}, b interface{}) (interface{}, error) {
	structFields := []reflect.StructField{}
	idx := 0
	aIdx := 0
	bIdx := 0
	if a != nil {
		if !isEmbeddable(reflect.TypeOf(a)) {
			return nil, ErrCannotMergeTypes
		}
		structFields = append(structFields, reflect.StructField{
			Name:      "A",
			Type:      reflect.TypeOf(a),
			Anonymous: true,
		})
		aIdx = idx
		idx++
	}
	if b != nil {
		if !isEmbeddable(reflect.TypeOf(b)) {
			return nil, ErrCannotMergeTypes
		}
		structFields = append(structFields, reflect.StructField{
			Name:      "B",
			Type:      reflect.TypeOf(b),
			Anonymous: true,
		})
		bIdx = idx
	}

	newType := reflect.StructOf(structFields)

	v := reflect.New(newType).Elem()
	if a != nil {
		v.Field(aIdx).Set(reflect.ValueOf(a))
	}
	if b != nil {
		v.Field(bIdx).Set(reflect.ValueOf(b))
	}

	return v.Interface(), nil
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
