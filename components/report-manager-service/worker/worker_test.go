package worker_test

import (
	"context"
	"encoding/json"
	"fmt"
	"testing"

	"github.com/DATA-DOG/go-sqlmock"
	"github.com/chef/automate/components/report-manager-service/storage"
	"github.com/chef/automate/components/report-manager-service/worker"
	"github.com/chef/automate/lib/cereal"
	"github.com/go-gorp/gorp"
	"github.com/stretchr/testify/assert"
)

func TestOnStart(t *testing.T) {
	tests := []struct {
		name                  string
		isDBFailure           bool
		isGetParameterFailure bool
		isEnqueueFailure      bool
		isFailedExpected      bool
		isContinueExpected    bool
		expectedError         string
	}{
		{
			name:                  "testOnStart_Success",
			isDBFailure:           false,
			isGetParameterFailure: false,
			isEnqueueFailure:      false,
			isFailedExpected:      false,
			isContinueExpected:    true,
			expectedError:         "",
		},
		{
			name:                  "testOnStart_GetParameter_Fail",
			isDBFailure:           false,
			isGetParameterFailure: true,
			isEnqueueFailure:      false,
			isFailedExpected:      true,
			isContinueExpected:    false,
			expectedError:         "failed to unmarshal report-workflow parameters: Error in fetching parameters",
		},
		{
			name:                  "testOnStart_Enqueue_Fail",
			isDBFailure:           false,
			isGetParameterFailure: false,
			isEnqueueFailure:      true,
			isFailedExpected:      true,
			isContinueExpected:    false,
			expectedError:         "failed to enqueue the report-task: Error in enqueuing",
		},
		{
			name:                  "testOnStart_DB_Fail",
			isDBFailure:           true,
			isGetParameterFailure: false,
			isEnqueueFailure:      false,
			isFailedExpected:      true,
			isContinueExpected:    false,
			expectedError:         "failed to insert the record in postgres: error in executing the insert task: insert error",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {

			dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
			assert.NoError(t, err)
			defer dbConn.Close()

			db := &storage.DB{
				DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
			}

			workFlow := worker.ReportWorkflow{
				DB: db,
			}

			query := `INSERT INTO custom_report_requests(id, requestor, status,created_at,updated_at) VALUES ($1, $2, $3, $4, $5);`
			if tc.isDBFailure {
				mock.ExpectExec(query).WithArgs("1234-5678", "reqID123", "running", sqlmock.AnyArg(), sqlmock.AnyArg()).WillReturnError(fmt.Errorf("insert error"))
			} else {
				mock.ExpectExec(query).WithArgs("1234-5678", "reqID123", "running", sqlmock.AnyArg(), sqlmock.AnyArg()).WillReturnResult(sqlmock.NewResult(1, 1))
			}

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

func (w *CerealWorkflow) GetPayload(obj interface{}) error {
	if w.failGetPayload {
		return fmt.Errorf("Error in fetching payload")
	}
	var jsonString string
	if w.isRetriesLeft {
		jsonString = `{"JobID":"1234-5678","Status":"running","RetriesLeft":5}`
	} else {
		jsonString = `{"JobID":"1234-5678","Status":"running","RetriesLeft":0}`
	}

	jsonBytes := []byte(jsonString)
	err := json.Unmarshal(jsonBytes, obj)
	return err
}

func (w *CerealWorkflow) GetParameters(obj interface{}) error {
	if w.failGetParameters {
		return fmt.Errorf("Error in fetching parameters")
	}
	jsonString := `{"JobID":"1234-5678","RequestorID":"reqID123","Retries":5}`
	jsonBytes := []byte(jsonString)
	err := json.Unmarshal(jsonBytes, obj)
	return err
}

func (w *CerealWorkflow) EnqueueTask(taskName cereal.TaskName, parameters interface{}, opts ...cereal.TaskEnqueueOpt) error {
	if w.failEnqueueTask {
		return fmt.Errorf("Error in enqueuing")
	}
	assert.Equal(w.t, "report-task", taskName.String())
	params := parameters.(worker.GenerateReportParameters)
	assert.Equal(w.t, "1234-5678", params.JobID)
	return nil
}

func (w *CerealWorkflow) Complete(...cereal.CompleteOpt) cereal.Decision {
	return cereal.NewCompleteDecision("")
}

func (w *CerealWorkflow) Continue(payload interface{}) cereal.Decision {
	workflowPayload := payload.(*worker.ReportWorkflowPayload)
	assert.Equal(w.t, "1234-5678", workflowPayload.JobID)
	if w.isRetryTest {
		assert.Equal(w.t, 4, workflowPayload.RetriesLeft)
	} else {
		assert.Equal(w.t, 5, workflowPayload.RetriesLeft)
	}
	assert.Equal(w.t, "running", workflowPayload.Status)
	return cereal.NewContinueDecision(payload)
}

func (w *CerealWorkflow) Fail(err error) cereal.Decision {
	return cereal.NewFailDecision(err)
}

func (w *CerealWorkflow) InstanceName() string {
	return ""
}

func (w *CerealWorkflow) TotalEnqueuedTasks() int {
	return 1
}

func (w *CerealWorkflow) TotalCompletedTasks() int {
	return 1
}

type TaskResult struct {
	isError bool
}

func (r *TaskResult) GetParameters(obj interface{}) error {
	return nil
}

func (r *TaskResult) Get(obj interface{}) error {
	return nil
}

func (r *TaskResult) Err() error {
	if r.isError {
		return fmt.Errorf("error in task execution")
	}
	return nil
}

func TestOnTaskComplete(t *testing.T) {
	tests := []struct {
		name                string
		isDBFailure         bool
		isGetPayloadFailure bool
		isEnqueueFailure    bool
		isFailedExpected    bool
		isContinueExpected  bool
		isCompleteExpected  bool
		isRetriesLeft       bool
		isTaskError         bool
		isRetryTest         bool
		expectedError       string
	}{
		{
			name:                "testOnTaskComplete_Success",
			isDBFailure:         false,
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
			isDBFailure:         false,
			isGetPayloadFailure: true,
			isEnqueueFailure:    false,
			isFailedExpected:    true,
			isContinueExpected:  false,
			isCompleteExpected:  false,
			expectedError:       "failed to unmarshal report-workflow payload: Error in fetching payload",
		},
		{
			name:                "testOnTaskComplete_NoRetriesLeft_Fail",
			isDBFailure:         false,
			isGetPayloadFailure: false,
			isEnqueueFailure:    false,
			isFailedExpected:    true,
			isContinueExpected:  false,
			isCompleteExpected:  false,
			isRetriesLeft:       false,
			isTaskError:         true,
			expectedError:       "failed to run report-task: error in task execution",
		},
		{
			name:                "testOnTaskComplete_NoRetriesLeft_DBFailure_Fail",
			isDBFailure:         true,
			isGetPayloadFailure: false,
			isEnqueueFailure:    false,
			isFailedExpected:    true,
			isContinueExpected:  false,
			isCompleteExpected:  false,
			isRetriesLeft:       false,
			isTaskError:         true,
			expectedError:       "failed to update the record in postgres: error in executing the update task: update error",
		},
		{
			name:                "testOnTaskComplete_RetriesLeft_Success",
			isDBFailure:         false,
			isGetPayloadFailure: false,
			isEnqueueFailure:    false,
			isFailedExpected:    false,
			isContinueExpected:  true,
			isCompleteExpected:  false,
			isRetriesLeft:       true,
			isTaskError:         true,
			isRetryTest:         true,
			expectedError:       "failed to run report-task: error in task execution",
		},
		{
			name:                "testOnTaskComplete_RetriesLeft_EnqueueFail",
			isDBFailure:         false,
			isGetPayloadFailure: false,
			isEnqueueFailure:    true,
			isFailedExpected:    true,
			isContinueExpected:  false,
			isCompleteExpected:  false,
			isRetriesLeft:       true,
			isTaskError:         true,
			isRetryTest:         true,
			expectedError:       "failed to enqueue the report-task: Error in enqueuing",
		},
		{
			name:                "testOnTaskComplete_DB_Fail",
			isDBFailure:         true,
			isGetPayloadFailure: false,
			isEnqueueFailure:    false,
			isFailedExpected:    true,
			isContinueExpected:  false,
			isCompleteExpected:  false,
			isRetriesLeft:       true,
			isTaskError:         false,
			expectedError:       "failed to update the record in postgres: error in executing the update task: update error",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {

			dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
			assert.NoError(t, err)
			defer dbConn.Close()

			db := &storage.DB{
				DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
			}

			workFlow := worker.ReportWorkflow{
				DB: db,
			}

			workflowInstance := &CerealWorkflow{
				t:           t,
				isRetryTest: tc.isRetryTest,
			}
			if tc.isGetPayloadFailure {
				workflowInstance.failGetPayload = true
			}
			if tc.isRetriesLeft {
				workflowInstance.isRetriesLeft = true
			}
			if tc.isEnqueueFailure {
				workflowInstance.failEnqueueTask = true
			}

			query := `UPDATE custom_report_requests SET status = $1, message = $2, updated_at = $3 WHERE id = $4;`
			if tc.isDBFailure {
				if !workflowInstance.isRetriesLeft {
					mock.ExpectExec(query).WithArgs("failed", "error in task execution", sqlmock.AnyArg(), "1234-5678").WillReturnError(fmt.Errorf("update error"))
				} else {
					mock.ExpectExec(query).WithArgs("success", "", sqlmock.AnyArg(), "1234-5678").WillReturnError(fmt.Errorf("update error"))
				}
			} else {
				if !workflowInstance.isRetriesLeft {
					mock.ExpectExec(query).WithArgs("failed", "error in task execution", sqlmock.AnyArg(), "1234-5678").WillReturnResult(sqlmock.NewResult(1, 1))
				} else {
					mock.ExpectExec(query).WithArgs("success", "", sqlmock.AnyArg(), "1234-5678").WillReturnResult(sqlmock.NewResult(1, 1))
				}
			}

			result := workFlow.OnTaskComplete(workflowInstance, cereal.TaskCompleteEvent{
				Result: &TaskResult{
					isError: tc.isTaskError,
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

type CerealTask struct {
	isParameterFailure bool
}

func TestRun(t *testing.T) {
	tests := []struct {
		name                   string
		isGetParametersFailure bool
		expectedError          string
	}{
		{
			name:          "testRun_Success",
			expectedError: "",
		},
		{
			name:                   "testRun_GetParameter_Fail",
			isGetParametersFailure: true,
			expectedError:          "could not unmarshal GenerateReportParameters: error in fetching parameters",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			task := worker.GenerateReportTask{}
			_, err := task.Run(context.Background(), &CerealTask{
				isParameterFailure: tc.isGetParametersFailure,
			})
			if tc.expectedError != "" {
				assert.Error(t, err)
				assert.Equal(t, tc.expectedError, err.Error())
			}
		})
	}
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
