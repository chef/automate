package licenseaudit

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/lib/cereal"
	"github.com/stretchr/testify/assert"
)

type TestExecute struct {
	wantError bool
}

func (test TestExecute) Execute(command string) (string, error) {
	if test.wantError {
		return "", errors.New("Received Error from command : exit status 1")
	}
	return command, nil
}

func Test_OnStart(t *testing.T) {
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
			name:                  "testOnStart_GetParametsFailure",
			isGetParameterFailure: true,
			isEnqueueFailure:      false,
			isFailedExpected:      true,
			isContinueExpected:    false,
			expectedError:         "failed to unmarshal license-audit workflow parameters: error in fetching parameters",
		},
		{
			name:               "testOnStart_Enqueue_Fail",
			isEnqueueFailure:   true,
			isFailedExpected:   true,
			isContinueExpected: false,
			expectedError:      "failed to enqueue the license-audit task: error in enqueuing",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {

			workFlow := LicenseAuditWorkflow{}

			workflowInstance := &CerealWorkflow{
				t:                 t,
				failEnqueueTask:   tc.isEnqueueFailure,
				failGetParameters: tc.isGetParameterFailure,
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

	var jsonString string
	if c.isRetriesLeft {
		jsonString = `{"Retries":3}`
	} else {
		jsonString = `{"Retries":0}`
	}

	jsonBytes := []byte(jsonString)
	err := json.Unmarshal(jsonBytes, obj)
	return err
}

func (c CerealWorkflow) GetParameters(obj interface{}) error {
	if c.failGetParameters {
		return fmt.Errorf("error in fetching parameters")
	}
	jsonString := `{"Retries":3}`
	jsonBytes := []byte(jsonString)
	err := json.Unmarshal(jsonBytes, obj)
	return err
}

func (c CerealWorkflow) EnqueueTask(taskName cereal.TaskName, parameters interface{}, opts ...cereal.TaskEnqueueOpt) error {
	if c.failEnqueueTask {
		return fmt.Errorf("error in enqueuing")
	}
	assert.Equal(c.t, "license-audit-task", taskName.String())
	return nil
}

func (c CerealWorkflow) Complete(opt ...cereal.CompleteOpt) cereal.Decision {
	return cereal.NewCompleteDecision("")
}

func (c CerealWorkflow) Continue(payload interface{}) cereal.Decision {
	workflowPayload := payload.(*AuditWorkflowPayload)
	// assert.Equal(c.t, true, workflowPayload.ControlIndexFlag)
	if c.isRetryTest {
		assert.Equal(c.t, 2, workflowPayload.Retries)
	} else {
		assert.Equal(c.t, 3, workflowPayload.Retries)
	}
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

type TaskResult struct {
	isError         bool
	taskResultError bool
}

func (r *TaskResult) GetParameters(obj interface{}) error {
	return nil
}

func (r *TaskResult) Get(obj interface{}) error {
	if r.taskResultError {
		return fmt.Errorf("Error in fetching job results")
	}
	// jsonString := `{"JobID":"1234-5678","ReportSize":8200,"PreSignedURL":"www.test.com/object"}`
	// jsonBytes := []byte(jsonString)
	// err := json.Unmarshal(jsonBytes, obj)
	// return err
	return nil
}

func (r *TaskResult) Err() error {
	if r.isError {
		return fmt.Errorf("error in task execution")
	}
	return nil
}

func Test_OnTaskComplete(t *testing.T) {
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
			expectedError:       "failed to unmarshal license-audit payload in OnComplete: Error in fetching payload",
		},
		{
			name:                "testOnTaskComplete_NoRetriesLeft_Fail",
			isGetPayloadFailure: false,
			isEnqueueFailure:    false,
			isFailedExpected:    true,
			isContinueExpected:  false,
			isCompleteExpected:  false,
			isRetriesLeft:       false,
			isTaskError:         true,
			expectedError:       "failed to run license-audit: error in task execution",
		},
		{
			name:                 "testOnTaskComplete_GetTaskResult_fail_Retries_left",
			isGetPayloadFailure:  false,
			isEnqueueFailure:     false,
			isFailedExpected:     true,
			isContinueExpected:   false,
			isCompleteExpected:   false,
			isRetriesLeft:        true,
			isFetchTaskResultErr: true,
			expectedError:        "failed to get the task run result in OnTaskComplete for license audit: Error in fetching job results",
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
			expectedError:       "failed to unmarshal license-audit workflow parameters: error in fetching parameters",
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
			expectedError:       "failed to unmarshal license-audit workflow parameters: error in fetching parameters",
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
			expectedError:       "failed to enqueue the license-audit task: error in enqueuing",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {

			workFlow := LicenseAuditWorkflow{}

			workflowInstance := &CerealWorkflow{
				t:                 t,
				isRetryTest:       tc.isRetryTest,
				failGetPayload:    tc.isGetPayloadFailure,
				isRetriesLeft:     tc.isRetriesLeft,
				failGetParameters: tc.isParameterFailure,
				failEnqueueTask:   tc.isEnqueueFailure,
			}

			result := workFlow.OnTaskComplete(workflowInstance, cereal.TaskCompleteEvent{
				Result: &TaskResult{
					isError:         tc.isTaskError,
					taskResultError: tc.isFetchTaskResultErr,
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

func Test_getAppendedCommand(t *testing.T) {
	tests := []struct {
		name    string
		command string
		want    string
	}{
		{
			name:    "Checking the output of the string",
			command: "%s %s",
		},
		{
			name:    "Getting license command",
			command: Command,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			dateToBeAppended := time.Now().AddDate(0, 0, -1).UTC().Format("2006-01-02")
			tt.want = fmt.Sprintf(tt.command, dateToBeAppended, dateToBeAppended)
			got := getAppendedCommand(tt.command)
			assert.Equal(t, tt.want, got)
		})
	}
}

func Test_executeCommandforAudit(t *testing.T) {
	type args struct {
		executeCommand ExecuteCommand
		command        string
	}
	tests := []struct {
		name    string
		args    args
		want    string
		wantErr bool
		err     error
	}{
		{
			name:    "Getting output without error",
			args:    args{executeCommand: &TestExecute{}, command: "test"},
			want:    "test",
			wantErr: false,
		},
		{
			name:    "Getting error from command",
			args:    args{executeCommand: &TestExecute{wantError: true}, command: ""},
			want:    "",
			wantErr: true,
			err:     errors.New("Received Error from command : exit status 1"),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := executeCommandforAudit(tt.args.executeCommand, tt.args.command)
			assert.Equal(t, tt.want, got)
			if !tt.wantErr {
				assert.Empty(t, err)
			} else {
				assert.NotEmpty(t, err)
				assert.Equal(t, tt.err, err)
			}
		})
	}
}

type CerealTask struct {
	isParameterFailure bool
}

func (t *CerealTask) GetParameters(obj interface{}) error {
	if t.isParameterFailure {
		return fmt.Errorf("error in fetching parameters")
	}
	return nil
}

func (t *CerealTask) GetMetadata() cereal.TaskMetadata {
	return cereal.TaskMetadata{}
}

func Test_Run(t *testing.T) {
	tests := []struct {
		name                   string
		isGetParametersFailure bool
		expectedErrorString    error
		commandError           bool
		expectedError          bool
	}{
		{
			name:                   "Test Run Success",
			isGetParametersFailure: false,
			commandError:           false,
			expectedError:          false,
			expectedErrorString:    nil,
		},
		{
			name:                   "Task Run paramaters failure",
			isGetParametersFailure: true,
			commandError:           false,
			expectedError:          true,
			expectedErrorString:    errors.New("Unable to marshal Task Parameters for Audit task: error in fetching parameters"),
		},
		{
			name:                   "Task Run output from command failure",
			isGetParametersFailure: false,
			commandError:           true,
			expectedError:          true,
			expectedErrorString:    errors.New("Received Error from command : exit status 1"),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {

			task := &LicenseAuditTask{
				ExecuteCommand: &TestExecute{wantError: tc.commandError},
				Command:        "testingcommand",
			}
			result, err := task.Run(context.Background(), &CerealTask{
				isParameterFailure: tc.isGetParametersFailure,
			})

			if tc.expectedError {
				assert.Equal(t, tc.expectedErrorString.Error(), err.Error())
			} else {
				assert.NoError(t, err)
				assert.NotNil(t, result)
			}
		})
	}
}
