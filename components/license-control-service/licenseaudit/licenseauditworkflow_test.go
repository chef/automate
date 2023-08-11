package licenseaudit

import (
	"context"
	"fmt"
	"reflect"
	"testing"

	"github.com/chef/automate/lib/cereal"
	"github.com/stretchr/testify/assert"
)

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
			name:               "testOnStart_Enqueue_Fail",
			isEnqueueFailure:   true,
			isFailedExpected:   true,
			isContinueExpected: false,
			expectedError:      "failed to enqueue the license-audit: error in enqueuing",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {

			workFlow := LicenseAuditWorkflow{}

			workflowInstance := &CerealWorkflow{
				t: t,
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
	return nil
}

func (c CerealWorkflow) GetParameters(obj interface{}) error {
	// if c.failGetParameters {
	// 	return fmt.Errorf("error in fetching parameters")
	// }
	// jsonString := `{"ControlIndexFlag":true}`
	// jsonBytes := []byte(jsonString)
	// err := json.Unmarshal(jsonBytes, obj)
	return nil
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
	// workflowPayload := payload.(*MigrationWorkflowPayload)
	// assert.Equal(c.t, true, workflowPayload.ControlIndexFlag)
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

func TestLicenseAuditTask_Run(t *testing.T) {
	type fields struct {
		Command        string
		ExecuteCommand ExecuteCommand
	}
	type args struct {
		ctx  context.Context
		task cereal.Task
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    interface{}
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tr := &LicenseAuditTask{
				Command:        tt.fields.Command,
				ExecuteCommand: tt.fields.ExecuteCommand,
			}
			got, err := tr.Run(tt.args.ctx, tt.args.task)
			if (err != nil) != tt.wantErr {
				t.Errorf("LicenseAuditTask.Run() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("LicenseAuditTask.Run() = %v, want %v", got, tt.want)
			}
		})
	}
}
