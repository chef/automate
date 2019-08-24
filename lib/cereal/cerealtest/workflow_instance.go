package cerealtest

import (
	"encoding/json"
	"testing"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/backend"
	"github.com/stretchr/testify/require"
)

type WorkflowInstance struct {
	t *testing.T
	// input
	instanceName      string
	inputParams       json.RawMessage
	inputPayload      json.RawMessage
	numEnqueuedTasks  int
	numCompletedTasks int

	// output
	enqueuedTasks []*TestableTask
	decision      *TestableDecision
}

type TestableDecision struct {
	Complete *TestableCompleteDecision
	Fail     *TestableFailDecision
	Continue *TestableContinueDecision
}

type TestableCompleteDecision struct {
	t      *testing.T
	result []byte
}

func (d *TestableCompleteDecision) GetResult(obj interface{}) {
	if len(d.result) == 0 {
		return
	}
	err := json.Unmarshal(d.result, obj)
	require.NoError(d.t, err)
}

type TestableFailDecision struct {
	t   *testing.T
	Err error
}

type TestableContinueDecision struct {
	t             *testing.T
	payload       []byte
	enqueuedTasks []*TestableTask
}

func (d *TestableContinueDecision) GetPayload(obj interface{}) {
	if len(d.payload) == 0 {
		return
	}
	err := json.Unmarshal(d.payload, obj)
	require.NoError(d.t, err)
}

type TestableTask struct {
	t          *testing.T
	parameters []byte

	Name string
	Opts backend.TaskEnqueueOpts
}

func (d *TestableTask) GetParameters(obj interface{}) {
	if len(d.parameters) == 0 {
		return
	}
	err := json.Unmarshal(d.parameters, obj)
	require.NoError(d.t, err)
}

type TestableTaskEnqueued struct {
	t     *testing.T
	Tasks []*TestableTask
}

func (t *TestableTaskEnqueued) AssertCount(c int) *TestableTaskEnqueued {
	require.Len(t.t, t.Tasks, c)
	return t
}

func (d *TestableContinueDecision) AssertTaskEnqueued(taskName string) *TestableTaskEnqueued {
	e := &TestableTaskEnqueued{
		t: d.t,
	}
	for _, task := range d.enqueuedTasks {
		if task.Name == taskName {
			e.Tasks = append(e.Tasks, task)
		}
	}

	require.True(d.t, len(e.Tasks) > 0, "task not found with the given parameters")
	return e
}

func NewWorkflowInstance(t *testing.T, instanceName string) *WorkflowInstance {
	return &WorkflowInstance{
		t:            t,
		instanceName: instanceName,
	}
}

func (w *WorkflowInstance) WithPayload(obj interface{}) *WorkflowInstance {
	v, err := json.Marshal(obj)
	require.NoError(w.t, err)
	w.inputPayload = v
	return w
}

func (w *WorkflowInstance) WithParameters(obj interface{}) *WorkflowInstance {
	v, err := json.Marshal(obj)
	require.NoError(w.t, err)
	w.inputParams = v
	return w
}

func (w *WorkflowInstance) WithNumEnqueuedTasks(numEnqueuedTasks int) *WorkflowInstance {
	w.numEnqueuedTasks = numEnqueuedTasks
	return w
}

func (w *WorkflowInstance) WithNumCompletedTasks(numCompletedTasks int) *WorkflowInstance {
	w.numCompletedTasks = numCompletedTasks
	return w
}

func (w *WorkflowInstance) ResetOutput() {
	w.decision = nil
	w.enqueuedTasks = nil
}

func (w *WorkflowInstance) GetPayload(obj interface{}) error {
	if len(w.inputPayload) == 0 {
		return nil
	}
	return json.Unmarshal(w.inputPayload, obj)
}

func (w *WorkflowInstance) GetParameters(obj interface{}) error {
	if len(w.inputParams) == 0 {
		return nil
	}
	return json.Unmarshal(w.inputParams, obj)
}

func (w *WorkflowInstance) EnqueueTask(taskName string, parameters interface{}, opts ...cereal.TaskEnqueueOpts) error {
	bopts := backend.TaskEnqueueOpts{}
	for _, o := range opts {
		o(&bopts)
	}
	v, err := json.Marshal(parameters)
	require.NoError(w.t, err)
	w.enqueuedTasks = append(w.enqueuedTasks, &TestableTask{
		t:          w.t,
		Name:       taskName,
		parameters: v,
		Opts:       bopts,
	})
	return nil
}

func (w *WorkflowInstance) Continue(payload interface{}) cereal.Decision {
	v, err := json.Marshal(payload)
	require.NoError(w.t, err)
	w.decision = &TestableDecision{
		Continue: &TestableContinueDecision{
			t:             w.t,
			payload:       v,
			enqueuedTasks: w.enqueuedTasks,
		},
	}
	return cereal.NewContinueDecision(payload)
}

func (w *WorkflowInstance) Complete(opts ...cereal.CompleteOpts) cereal.Decision {
	d := cereal.NewCompleteDecision(nil)
	for _, o := range opts {
		o(&d)
	}

	v, err := json.Marshal(d.Result())
	require.NoError(w.t, err)

	w.decision = &TestableDecision{
		Complete: &TestableCompleteDecision{
			t:      w.t,
			result: v,
		},
	}

	return d
}

func (w *WorkflowInstance) Fail(err error) cereal.Decision {
	w.decision = &TestableDecision{
		Fail: &TestableFailDecision{
			t:   w.t,
			Err: err,
		},
	}

	return cereal.NewFailDecision(err)
}

func (w *WorkflowInstance) InstanceName() string {
	return w.instanceName
}

func (w *WorkflowInstance) TotalEnqueuedTasks() int {
	return len(w.enqueuedTasks) + w.numEnqueuedTasks
}
func (w *WorkflowInstance) TotalCompletedTasks() int {
	return w.numCompletedTasks
}

func (w *WorkflowInstance) AssertComplete() *TestableCompleteDecision {
	require.NotNil(w.t, w.decision, "no decisions made")
	require.Nil(w.t, w.decision.Fail, "expected complete, got failed")
	require.Nil(w.t, w.decision.Continue, "expected complete, got continuing")
	require.NotNil(w.t, w.decision.Complete, "workflow instance not completed")

	return w.decision.Complete
}

func (w *WorkflowInstance) AssertContinuing() *TestableContinueDecision {
	require.NotNil(w.t, w.decision, "no decisions made")
	require.Nil(w.t, w.decision.Complete, "expected continuing, got completed")
	require.Nil(w.t, w.decision.Fail, "expected continuing, got failed")
	require.NotNil(w.t, w.decision.Continue, "workflow instance not continuing")
	return w.decision.Continue
}

func (w *WorkflowInstance) AssertFailed() *TestableFailDecision {
	require.NotNil(w.t, w.decision, "no decisions made")
	require.Nil(w.t, w.decision.Continue, "expected failed, got continuing")
	require.Nil(w.t, w.decision.Complete, "expected failed, got completed")
	require.NotNil(w.t, w.decision.Fail, "workflow instance not failed")

	return w.decision.Fail
}

func (d *TestableFailDecision) WithErrorEqual(err error) *TestableFailDecision {
	require.Equal(d.t, err, d.Err)
	return d
}
