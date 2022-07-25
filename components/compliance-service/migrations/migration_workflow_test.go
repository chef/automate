package migrations

import (
	"encoding/json"
	"fmt"
	"github.com/chef/automate/lib/cereal"
	"github.com/stretchr/testify/assert"
	"testing"
)

var expectedGenericError = "failed to unmarshal migration-workflow parameters: error in fetching parameters"

func TestOnStart(t *testing.T) {
	tests := []struct {
		name                  string
		isGetParameterFailure bool
		isEnqueueFailure      bool
		isFailedExpected      bool
		isContinueExpected    bool
		expectedError         string
	}{
		{
			name:                  "testOnStart_Success",
			isGetParameterFailure: false,
			isEnqueueFailure:      false,
			isFailedExpected:      false,
			isContinueExpected:    true,
			expectedError:         "",
		},
		{
			name:                  "testOnStart_GetParameter_Fail",
			isGetParameterFailure: true,
			isEnqueueFailure:      false,
			isFailedExpected:      true,
			isContinueExpected:    false,
			expectedError:         expectedGenericError,
		},
		{
			name:                  "testOnStart_Enqueue_Fail",
			isGetParameterFailure: false,
			isEnqueueFailure:      true,
			isFailedExpected:      true,
			isContinueExpected:    false,
			expectedError:         "failed to enqueue the migration-task: error in enqueuing",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {

			workFlow := MigrationWorkflow{}

			workflowInstance := &CerealWorkflow{
				t: t,
			}
			if tc.isGetParameterFailure {
				workflowInstance.failGetParameters = true
			}
			if tc.isEnqueueFailure {
				workflowInstance.failEnqueueTask = true
			}

			result := workFlow.OnStart(workflowInstance, cereal.StartEvent{})
			assert.Equal(t, tc.isFailedExpected, result.IsFailed())
			assert.Equal(t, tc.isContinueExpected, result.IsContinuing())
			if tc.isFailedExpected {
				assert.Error(t, result.Err())
				assert.Equal(t, tc.expectedError, result.Err().Error())
			}
		})
	}
}

type CerealWorkflow struct {
	t                 *testing.T
	failGetParameters bool
	failEnqueueTask   bool
	failGetPayload    bool
	isRetriesLeft     bool
	isRetryTest       bool
}

func (c CerealWorkflow) GetPayload(obj interface{}) error {
	if c.failGetPayload {
		return fmt.Errorf("Error in fetching payload")
	}

	jsonString := `{"DayLatestFlag":true}`
	jsonBytes := []byte(jsonString)
	err := json.Unmarshal(jsonBytes, obj)
	return err
}

func (c CerealWorkflow) GetParameters(obj interface{}) error {
	if c.failGetParameters {
		return fmt.Errorf("error in fetching parameters")
	}
	jsonString := `{"DayLatestFlag":true}`
	jsonBytes := []byte(jsonString)
	err := json.Unmarshal(jsonBytes, obj)
	return err
}

func (c CerealWorkflow) EnqueueTask(taskName cereal.TaskName, parameters interface{}, opts ...cereal.TaskEnqueueOpt) error {
	if c.failEnqueueTask {
		return fmt.Errorf("error in enqueuing")
	}
	assert.Equal(c.t, "upgrade-task", taskName.String())
	params := parameters.(UpgradeParameters)
	assert.Equal(c.t, true, params.DayLatestFlag)
	return nil
}

func (c CerealWorkflow) Complete(opt ...cereal.CompleteOpt) cereal.Decision {
	return cereal.NewCompleteDecision("")
}

func (c CerealWorkflow) Continue(payload interface{}) cereal.Decision {
	workflowPayload := payload.(*MigrationWorkflowPayload)
	assert.Equal(c.t, true, workflowPayload.DayLatestFlag)
	return cereal.NewContinueDecision(payload)
}

func (c CerealWorkflow) Fail(err error) cereal.Decision {
	return cereal.NewFailDecision(err)
}

func (c CerealWorkflow) InstanceName() string {
	return ""
}

func (c CerealWorkflow) TotalEnqueuedTasks() int {
	return 1
}

func (c CerealWorkflow) TotalCompletedTasks() int {
	return 1
}

func TestOnTaskComplete(t *testing.T) {

	tests := []struct {
		name                 string
		isGetPayloadFailure  bool
		isEnqueueFailure     bool
		isFailedExpected     bool
		isContinueExpected   bool
		isCompleteExpected   bool
		isRetriesLeft        bool
		isTaskError          bool
		isRetryTest          bool
		isParameterFailure   bool
		expectedError        string
		isFetchTaskResultErr bool
	}{
		{
			name:                "testOnTaskComplete_Success",
			isGetPayloadFailure: false,
			isEnqueueFailure:    false,
			isFailedExpected:    false,
			isContinueExpected:  false,
			isCompleteExpected:  true,
			isRetriesLeft:       true,
			expectedError:       "",
		},
		{
			name:                "testOnTaskComplete_GetPayload_Fail",
			isGetPayloadFailure: true,
			isEnqueueFailure:    false,
			isFailedExpected:    true,
			isContinueExpected:  false,
			isCompleteExpected:  false,
			expectedError:       "failed to unmarshal migration-workflow payload: Error in fetching payload",
		}}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {

			workFlow := MigrationWorkflow{}

			workflowInstance := &CerealWorkflow{
				t:           t,
				isRetryTest: tc.isRetryTest,
			}
			workflowInstance.failGetPayload = tc.isGetPayloadFailure
			workflowInstance.isRetriesLeft = tc.isRetriesLeft
			workflowInstance.failEnqueueTask = tc.isEnqueueFailure
			workflowInstance.failGetParameters = tc.isParameterFailure

			result := workFlow.OnTaskComplete(workflowInstance, cereal.TaskCompleteEvent{
				Result: &TaskResult{
					isError: tc.isTaskError,
					failGet: tc.isFetchTaskResultErr,
				},
			})
			assert.Equal(t, tc.isFailedExpected, result.IsFailed())
			assert.Equal(t, tc.isContinueExpected, result.IsContinuing())
			assert.Equal(t, tc.isCompleteExpected, result.IsComplete())
			if tc.isFailedExpected {
				assert.Error(t, result.Err())
				assert.Equal(t, tc.expectedError, result.Err().Error())
			}
		})
	}
}

type TaskResult struct {
	isError bool
	failGet bool
}

func (t TaskResult) GetParameters(obj interface{}) error {

	if t.failGet {
		return fmt.Errorf("Error in fetching job results")
	}
	jsonString := `{"DayLatestFlag":true}`
	jsonBytes := []byte(jsonString)
	err := json.Unmarshal(jsonBytes, obj)
	return err
}

func (t TaskResult) Get(obj interface{}) error {
	return nil
}

func (t TaskResult) Err() error {
	if t.isError {
		return fmt.Errorf("error in task execution")
	}
	return nil
}
