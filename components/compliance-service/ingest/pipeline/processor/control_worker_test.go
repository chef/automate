package processor

import (
	"context"
	"encoding/json"
	"fmt"
	"github.com/chef/automate/lib/cereal"
	"github.com/stretchr/testify/assert"
	"testing"
)

var expectedGenericError = "failed to unmarshal control-workflow parameters: error in fetching parameters"

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
			expectedError:         "failed to enqueue the control-task: error in enqueuing",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {

			workFlow := ControlWorkflow{}

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

func (c *CerealWorkflow) GetPayload(obj interface{}) error {
	if c.failGetPayload {
		return fmt.Errorf("Error in fetching payload")
	}
	var jsonString string
	if c.isRetriesLeft {
		jsonString = `{"ReportUuid":"test","Status":"running","RetriesLeft":5}`
	} else {
		jsonString = `{"ReportUuid":"test","Status":"running","RetriesLeft":0}`
	}

	jsonBytes := []byte(jsonString)
	err := json.Unmarshal(jsonBytes, obj)
	return err
}

func (c *CerealWorkflow) GetParameters(obj interface{}) error {
	if c.failGetParameters {
		return fmt.Errorf("error in fetching parameters")
	}
	jsonString := `{"ReportUuid":"test","Retries":2}`
	jsonBytes := []byte(jsonString)
	err := json.Unmarshal(jsonBytes, obj)
	return err
}

func (c *CerealWorkflow) EnqueueTask(taskName cereal.TaskName, parameters interface{}, opts ...cereal.TaskEnqueueOpt) error {
	if c.failEnqueueTask {
		return fmt.Errorf("error in enqueuing")
	}
	assert.Equal(c.t, "control-task", taskName.String())
	params := parameters.(GenerateControlParameters)
	assert.Equal(c.t, "test", params.ReportUuid)
	return nil
}

func (c *CerealWorkflow) Complete(opt ...cereal.CompleteOpt) cereal.Decision {
	return cereal.NewCompleteDecision("")
}

func (c *CerealWorkflow) Continue(payload interface{}) cereal.Decision {
	workflowPayload := payload.(*ControlWorkflowPayload)
	assert.Equal(c.t, "test", workflowPayload.ReportUuid)
	if c.isRetryTest {
		assert.Equal(c.t, 4, workflowPayload.RetriesLeft)
	} else {
		assert.Equal(c.t, 2, workflowPayload.RetriesLeft)
	}
	assert.Equal(c.t, "running", workflowPayload.Status)
	return cereal.NewContinueDecision(payload)
}

func (c *CerealWorkflow) Fail(err error) cereal.Decision {

	return cereal.NewFailDecision(err)
}

func (c *CerealWorkflow) InstanceName() string {
	return ""
}

func (c CerealWorkflow) TotalEnqueuedTasks() int {
	return 1
}

func (c *CerealWorkflow) TotalCompletedTasks() int {
	return 1
}

func TestRun(t *testing.T) {
	tests := []struct {
		name                   string
		expectedError          string
		isGetParametersFailure bool
	}{
		{
			name:          "testRun_Success",
			expectedError: "",
		},
		{
			name:                   "testRun_ParametersFailure",
			expectedError:          "could not unmarshal GenerateReportParameters: error in fetching parameters",
			isGetParametersFailure: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			task := GenerateControlTask{}
			result, err := task.Run(context.Background(), &CerealTask{
				isParameterFailure: tc.isGetParametersFailure,
			})

			if tc.expectedError != "" {
				assert.Error(t, err)
				assert.Equal(t, tc.expectedError, err.Error())
			}
			if tc.expectedError == "" {
				assert.NoError(t, err)
			}
			if result != nil {
				_ = result.(*GenerateControlParameters)

			}
		})
	}
}

type CerealTask struct {
	isParameterFailure bool
}

func (c CerealTask) GetParameters(obj interface{}) error {
	if c.isParameterFailure {
		return fmt.Errorf("error in fetching parameters")
	}
	jsonString := `{"ReportUuid":"test"}`
	jsonBytes := []byte(jsonString)
	err := json.Unmarshal(jsonBytes, obj)
	return err
}

func (c CerealTask) GetMetadata() cereal.TaskMetadata {
	return cereal.TaskMetadata{}
}

type TaskResult struct {
	isError bool
	failGet bool
}

func (t TaskResult) GetParameters(obj interface{}) error {
	if t.failGet {
		return fmt.Errorf("Error in fetching job results")
	}
	jsonString := `{"ReportUuid":"Test","Status":"complete"}`
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
			expectedError:       "failed to unmarshal control-workflow payload: Error in fetching payload",
		},
		{
			name:                 "testOnTaskComplete_NoRetriesLeft_Fail",
			isGetPayloadFailure:  false,
			isEnqueueFailure:     false,
			isFailedExpected:     true,
			isContinueExpected:   false,
			isCompleteExpected:   false,
			isRetriesLeft:        false,
			isTaskError:          true,
			isFetchTaskResultErr: true,
			expectedError:        "failed to run control-task: error in task execution",
		},
		{
			name:                "testOnTaskComplete_RetriesLeft_Success",
			isGetPayloadFailure: false,
			isEnqueueFailure:    false,
			isFailedExpected:    false,
			isContinueExpected:  true,
			isCompleteExpected:  false,
			isRetriesLeft:       true,
			isTaskError:         true,
			isRetryTest:         true,
			expectedError:       "",
		},
		{
			name:                "testOnTaskComplete_RetriesLeft_GetParameter_Fail",
			isGetPayloadFailure: false,
			isEnqueueFailure:    false,
			isFailedExpected:    true,
			isContinueExpected:  false,
			isCompleteExpected:  false,
			isRetriesLeft:       true,
			isTaskError:         true,
			isRetryTest:         true,
			isParameterFailure:  true,
			expectedError:       expectedGenericError,
		},
		{
			name:                "testOnTaskComplete_RetriesLeft_GetParameter_Fail",
			isGetPayloadFailure: false,
			isEnqueueFailure:    false,
			isFailedExpected:    true,
			isContinueExpected:  false,
			isCompleteExpected:  false,
			isRetriesLeft:       true,
			isTaskError:         true,
			isRetryTest:         true,
			isParameterFailure:  true,
			expectedError:       expectedGenericError,
		},
		{
			name:                "testOnTaskComplete_RetriesLeft_EnqueueFail",
			isGetPayloadFailure: false,
			isEnqueueFailure:    true,
			isFailedExpected:    true,
			isContinueExpected:  false,
			isCompleteExpected:  false,
			isRetriesLeft:       true,
			isTaskError:         true,
			isRetryTest:         true,
			expectedError:       "failed to enqueue the control-task: error in enqueuing",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {

			workFlow := ControlWorkflow{}

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

func setWorkflowParameters(isGetPayloadFailure bool, isRetriesLeft bool, isEnqueueFailure bool, isParameterFailure bool) {

}
