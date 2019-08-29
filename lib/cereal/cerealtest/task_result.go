package cerealtest

import (
	"encoding/json"
	"testing"

	"github.com/stretchr/testify/require"
)

type TaskResult struct {
	t *testing.T

	err        error
	parameters json.RawMessage
	result     json.RawMessage
}

func (tr *TaskResult) GetParameters(obj interface{}) error {
	if len(tr.parameters) == 0 {
		return nil
	}
	err := json.Unmarshal(tr.parameters, obj)
	require.NoError(tr.t, err)
	return err
}

func (tr *TaskResult) Get(obj interface{}) error {
	if tr.Err() != nil {
		return tr.Err()
	}
	if len(tr.result) == 0 {
		return nil
	}
	err := json.Unmarshal(tr.result, obj)
	require.NoError(tr.t, err)
	return err
}

func (tr *TaskResult) Err() error {
	return tr.err
}

func (tr *TaskResult) WithResult(obj interface{}) *TaskResult {
	v, err := json.Marshal(obj)
	require.NoError(tr.t, err)
	tr.result = v
	return tr
}

func (tr *TaskResult) WithParameters(obj interface{}) *TaskResult {
	v, err := json.Marshal(obj)
	require.NoError(tr.t, err)
	tr.parameters = v
	return tr
}

func (tr *TaskResult) WithError(err error) *TaskResult {
	tr.err = err
	return tr
}

func NewTaskResult(t *testing.T) *TaskResult {
	return &TaskResult{
		t: t,
	}
}
