package worker_test

import (
	"bytes"
	"context"
	"encoding/csv"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net"
	"net/http"
	"net/url"
	"strings"
	"testing"
	"time"

	"github.com/DATA-DOG/go-sqlmock"
	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	"github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/chef/automate/components/report-manager-service/storage"
	"github.com/chef/automate/components/report-manager-service/worker"
	"github.com/chef/automate/lib/cereal"
	"github.com/go-gorp/gorp"
	"github.com/minio/minio-go/v7"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc"
	"google.golang.org/grpc/test/bufconn"
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

			query := `INSERT INTO custom_report_requests(id, requestor, status, custom_report_type, created_at, updated_at) VALUES ($1, $2, $3, $4, $5, $6);`
			if tc.isDBFailure {
				mock.ExpectExec(query).WithArgs("1234-5678", "reqID123", "running", "json", sqlmock.AnyArg(), sqlmock.AnyArg()).WillReturnError(fmt.Errorf("insert error"))
			} else {
				mock.ExpectExec(query).WithArgs("1234-5678", "reqID123", "running", "json", sqlmock.AnyArg(), sqlmock.AnyArg()).WillReturnResult(sqlmock.NewResult(1, 1))
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
	jsonString := `{"JobID":"1234-5678","RequestorID":"reqID123","Retries":5,"RequestToProcess":{"requestor_id":"reqID123","report_type":"json","reports":[{"report_id":"r1","profiles":[{"profile_id":"r1p1","controls":["r1p1c1","r1p1c2"]},{"profile_id":"r1p2","controls":["r1p2c1","r1p2c2"]}]},{"report_id":"r2","profiles":[{"profile_id":"r2p1","controls":["r2p1c1","r2p1c2"]},{"profile_id":"r2p2","controls":["r2p2c1","r2p2c2"]}]}]}}`
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
	assert.Equal(w.t, "reqID123", params.RequestToProcess.RequestorId)
	assert.Equal(w.t, "json", params.RequestToProcess.ReportType)
	assert.Equal(w.t, 0, len(params.RequestToProcess.Filters))
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
	failGet bool
}

func (r *TaskResult) GetParameters(obj interface{}) error {
	return nil
}

func (r *TaskResult) Get(obj interface{}) error {
	if r.failGet {
		return fmt.Errorf("Error in fetching job results")
	}
	jsonString := `{"JobID":"1234-5678","ReportSize":8200,"PreSignedURL":"www.test.com/object"}`
	jsonBytes := []byte(jsonString)
	err := json.Unmarshal(jsonBytes, obj)
	return err
}

func (r *TaskResult) Err() error {
	if r.isError {
		return fmt.Errorf("error in task execution")
	}
	return nil
}

func TestOnTaskComplete(t *testing.T) {
	tests := []struct {
		name                 string
		isDBFailure          bool
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
			name:                 "testOnTaskComplete_GetTaskResult_fail",
			isDBFailure:          false,
			isGetPayloadFailure:  false,
			isEnqueueFailure:     false,
			isFailedExpected:     true,
			isContinueExpected:   false,
			isCompleteExpected:   false,
			isRetriesLeft:        true,
			isFetchTaskResultErr: true,
			expectedError:        "failed to get the task run result in OnTaskComplete: Error in fetching job results",
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
			expectedError:       "",
		},
		{
			name:                "testOnTaskComplete_RetriesLeft_GetParameter_Fail",
			isDBFailure:         false,
			isGetPayloadFailure: false,
			isEnqueueFailure:    false,
			isFailedExpected:    true,
			isContinueExpected:  false,
			isCompleteExpected:  false,
			isRetriesLeft:       true,
			isTaskError:         true,
			isRetryTest:         true,
			isParameterFailure:  true,
			expectedError:       "failed to unmarshal report-workflow parameters: Error in fetching parameters",
		},
		{
			name:                "testOnTaskComplete_RetriesLeft_GetParameter_Fail",
			isDBFailure:         false,
			isGetPayloadFailure: false,
			isEnqueueFailure:    false,
			isFailedExpected:    true,
			isContinueExpected:  false,
			isCompleteExpected:  false,
			isRetriesLeft:       true,
			isTaskError:         true,
			isRetryTest:         true,
			isParameterFailure:  true,
			expectedError:       "failed to unmarshal report-workflow parameters: Error in fetching parameters",
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
			if tc.isParameterFailure {
				workflowInstance.failGetParameters = true
			}

			query := `UPDATE custom_report_requests SET status = $1, message = $2, custom_report_size = $3, custom_report_url = $4, updated_at = $5 WHERE id = $6;`
			if tc.isDBFailure {
				if !workflowInstance.isRetriesLeft {
					mock.ExpectExec(query).WithArgs("failed", "error in task execution", 0, "", sqlmock.AnyArg(), "1234-5678").WillReturnError(fmt.Errorf("update error"))
				} else {
					mock.ExpectExec(query).WithArgs("success", "", 8200, "www.test.com/object", sqlmock.AnyArg(), "1234-5678").WillReturnError(fmt.Errorf("update error"))
				}
			} else {
				if !workflowInstance.isRetriesLeft {
					mock.ExpectExec(query).WithArgs("failed", "error in task execution", 0, "", sqlmock.AnyArg(), "1234-5678").WillReturnResult(sqlmock.NewResult(1, 1))
				} else {
					mock.ExpectExec(query).WithArgs("success", "", 8200, "www.test.com/object", sqlmock.AnyArg(), "1234-5678").WillReturnResult(sqlmock.NewResult(1, 1))
				}
			}

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

type CerealTask struct {
	isParameterFailure bool
	isCSVExport        bool
}

func (t *CerealTask) GetParameters(obj interface{}) error {
	if t.isParameterFailure {
		return fmt.Errorf("error in fetching parameters")
	}
	jsonString := `{"JobID":"1234-5678","RequestToProcess":{"requestor_id":"reqID123","report_type":"json","reports":[{"report_id":"r1","profiles":[{"profile_id":"r1p1","controls":["r1p1c1","r1p1c2"]},{"profile_id":"r1p2","controls":["r1p2c1","r1p2c2"]}]},{"report_id":"r2","profiles":[{"profile_id":"r2p1","controls":["r2p1c1","r2p1c2"]},{"profile_id":"r2p2","controls":["r2p2c1","r2p2c2"]}]}]}}`
	if t.isCSVExport {
		jsonString = `{"JobID":"1234-5678","RequestToProcess":{"requestor_id":"reqID123","report_type":"csv","reports":[{"report_id":"r1","profiles":[{"profile_id":"r1p1","controls":["r1p1c1","r1p1c2"]},{"profile_id":"r1p2","controls":["r1p2c1","r1p2c2"]}]},{"report_id":"r2","profiles":[{"profile_id":"r2p1","controls":["r2p1c1","r2p1c2"]},{"profile_id":"r2p2","controls":["r2p2c1","r2p2c2"]}]}]}}`
	}
	jsonBytes := []byte(jsonString)
	err := json.Unmarshal(jsonBytes, obj)
	return err
}

func (t *CerealTask) GetMetadata() cereal.TaskMetadata {
	return cereal.TaskMetadata{}
}

type mockObjStore struct {
	T                         *testing.T
	ForGetObjectFailure       bool
	ForPutObjectFailure       bool
	ForUnmarshallFailure      bool
	IsBucketExisted           bool
	ForBucketExistenceFail    bool
	ForMakeBucketFail         bool
	ForPresignedGetObjectFail bool
	ForCSVExporter            bool
	ForObjectNotAvailable     bool
}

func (m mockObjStore) PutObject(ctx context.Context, bucketName, objectName string, reader io.Reader, objectSize int64,
	opts minio.PutObjectOptions) (minio.UploadInfo, error) {
	if m.ForObjectNotAvailable && objectName == "r1.json" {
		assert.Equal(m.T, "testBucket", bucketName)
		assert.Equal(m.T, "r1.json", objectName)
		return minio.UploadInfo{
			Size: 8200,
		}, nil
	} else {
		assert.Equal(m.T, "reqID123", bucketName)
		if m.ForCSVExporter {
			assert.Equal(m.T, "1234-5678.csv", objectName)
		} else {
			assert.Equal(m.T, "1234-5678.json", objectName)
		}
		assert.Equal(m.T, int64(-1), objectSize)
		if m.ForPutObjectFailure {
			return minio.UploadInfo{}, fmt.Errorf("error in storing object to object store")
		}

		if m.ForCSVExporter {
			lines, err := csv.NewReader(reader).ReadAll()
			assert.NoError(m.T, err)
			assert.Equal(m.T, 2, len(lines))
			assert.Equal(m.T, 22, len(lines[0]))
			assert.Equal(m.T, []string{"Node Name", "End Time", "Platform Name", "Platform Release",
				"Environment", "IPAddress", "FQDN", "Profile Name", "Profile Title", "Profile Version",
				"Profile Summary", "Control ID", "Control Title", "Control Impact", "Waived (true/false)",
				"Result Status", "Result Run Time", "Result Code Description", "Result Message",
				"Result Skip Message", "Waiver Justification", "Waiver Expiration"},
				lines[0])
			assert.Equal(m.T, []string{"TestNode", "2021-11-17 10:53:52 +0000 UTC", "test_platform", "1.0",
				"testEnv", "0.0.0.0", "test_fqdn", "r1p1", "r1p1", "v1", "test_summary", "r1p1c1", "r1p1c1",
				"0.00", "false", "failed", "0.006", "test code desc", "test_msg", "test_skip_msg", "", ""},
				lines[1])
		} else {
			bytes, err := ioutil.ReadAll(reader)
			assert.NoError(m.T, err)
			//expects new line at the end of the content
			assert.Equal(m.T, `[{"id":"r1","node_id":"nodeID","node_name":"TestNode","profiles":[{"name":"r1p1","title":"r1p1","full":"r1p1, v","sha256":"r1p1","controls":[{"id":"r1p1c1","title":"r1p1c1"},{"id":"r1p1c2","title":"r1p1c2"}]},{"name":"r1p2","title":"r1p2","full":"r1p2, v","sha256":"r1p2","controls":[{"id":"r1p2c1","title":"r1p2c1"},{"id":"r1p2c2","title":"r1p2c2"}]}]},{"id":"r2","node_id":"nodeID","node_name":"TestNode","profiles":[{"name":"r2p1","title":"r2p1","full":"r2p1, v","sha256":"r2p1","controls":[{"id":"r2p1c1","title":"r2p1c1"},{"id":"r2p1c2","title":"r2p1c2"}]},{"name":"r2p2","title":"r2p2","full":"r2p2, v","sha256":"r2p2","controls":[{"id":"r2p2c1","title":"r2p2c1"},{"id":"r2p2c2","title":"r2p2c2"}]}]}]
`, string(bytes))
		}

		return minio.UploadInfo{
			Size: 8200,
		}, nil
	}
}

func (m mockObjStore) GetObject(ctx context.Context, bucketName, objectName string, opts minio.GetObjectOptions) (io.Reader, error) {
	assert.Equal(m.T, "testBucket", bucketName)

	if m.ForGetObjectFailure {
		return nil, fmt.Errorf("Error in fetching the object from object store")
	} else if m.ForUnmarshallFailure {
		return strings.NewReader(""), nil
	}

	var json string
	if objectName == "r1.json" {
		if m.ForCSVExporter {
			json = `{"profiles":[{"name":"r1p1","title":"r1p1","sha256":"r1p1","version":"v1","summary":"test_summary","controls":[{"id":"r1p1c1","title":"r1p1c1","results":[{"status":"failed","code_desc":"test code desc","run_time":0.00642,"start_time":"2021-11-18T15:02:27+01:00","message":"test_msg","skip_message":"test_skip_msg"}]},{"id":"r1p1c2","title":"r1p1c2"}]},{"name":"r1p2","title":"r1p2","sha256":"r1p2","controls":[{"id":"r1p2c1","title":"r1p2c1"},{"id":"r1p2c2","title":"r1p2c2"}]}],"report_uuid":"r1","node_uuid":"nodeID","node_name":"TestNode","environment":"testEnv","ipaddress":"0.0.0.0","fqdn":"test_fqdn","end_time":"2021-11-17T10:53:52Z","platform":{"name":"test_platform","release":"1.0"}}`
		} else {
			json = `{"profiles":[{"name":"r1p1","title":"r1p1","sha256":"r1p1","controls":[{"id":"r1p1c1","title":"r1p1c1"},{"id":"r1p1c2","title":"r1p1c2"}]},{"name":"r1p2","title":"r1p2","sha256":"r1p2","controls":[{"id":"r1p2c1","title":"r1p2c1"},{"id":"r1p2c2","title":"r1p2c2"}]}],"report_uuid":"r1","node_uuid":"nodeID","node_name":"TestNode"}`
		}

	} else if objectName == "r2.json" {
		json = `{"profiles":[{"name":"r2p1","title":"r2p1","sha256":"r2p1","controls":[{"id":"r2p1c1","title":"r2p1c1"},{"id":"r2p1c2","title":"r2p1c2"}]},{"name":"r2p2","title":"r2p2","sha256":"r2p2","controls":[{"id":"r2p2c1","title":"r2p2c1"},{"id":"r2p2c2","title":"r2p2c2"}]}],"report_uuid":"r2","node_uuid":"nodeID","node_name":"TestNode"}`
	}

	return strings.NewReader(json), nil
}

func (m mockObjStore) BucketExists(ctx context.Context, bucketName string) (bool, error) {
	assert.Equal(m.T, "reqID123", bucketName)
	if m.ForBucketExistenceFail {
		return false, fmt.Errorf("error in checking the existence of a bucket")
	}
	if m.IsBucketExisted {
		return true, nil
	}
	return false, nil
}

func (m mockObjStore) MakeBucket(ctx context.Context, bucketName string, opts minio.MakeBucketOptions) error {
	assert.Equal(m.T, "reqID123", bucketName)
	if m.ForMakeBucketFail {
		return fmt.Errorf("error in creating a bucket in object store")
	}
	return nil
}

func (m mockObjStore) PresignedGetObject(ctx context.Context, bucketName string, objectName string, expires time.Duration, reqParams url.Values) (u *url.URL, err error) {
	assert.Equal(m.T, "reqID123", bucketName)
	if m.ForCSVExporter {
		assert.Equal(m.T, "1234-5678.csv", objectName)
	} else {
		assert.Equal(m.T, "1234-5678.json", objectName)
	}

	assert.Equal(m.T, time.Hour*25, expires)

	if m.ForPresignedGetObjectFail {
		return nil, fmt.Errorf("error in getting pre-signed URL")
	}
	link, err := url.Parse("www.test.com/object")
	assert.NoError(m.T, err)
	return link, nil

}

func (m mockObjStore) StatObject(ctx context.Context, bucketName string, objectName string, opts minio.GetObjectOptions) (info minio.ObjectInfo, err error) {
	assert.Equal(m.T, "testBucket", bucketName)
	if m.ForObjectNotAvailable && objectName == "r1.json" {
		errResp := minio.ErrorResponse{
			Code:       "NoSuchKey",
			Message:    "The specified key does not exist.",
			StatusCode: http.StatusNotFound,
			RequestID:  "minio",
		}
		return minio.ObjectInfo{}, errResp
	}

	return minio.ObjectInfo{}, nil
}

func TestRun(t *testing.T) {
	tests := []struct {
		name                            string
		isGetParametersFailure          bool
		isDataStoreGetError             bool
		isDataStorePutError             bool
		isUnmarshalError                bool
		expectedError                   string
		isBucketExisted                 bool
		isBucketExistenceCheckFail      bool
		isCreateBucketFail              bool
		isPresignedGetObjectFail        bool
		isCSVExporter                   bool
		isObjectNotAvailableInDataStore bool
	}{
		{
			name:          "testRun_Success",
			expectedError: "",
		},
		{
			name:          "testRun_CSVExporter_Success",
			expectedError: "",
			isCSVExporter: true,
		},
		{
			name:            "testRun_Success_WithExistingBucket",
			expectedError:   "",
			isBucketExisted: true,
		},
		{
			name:                   "testRun_GetParameter_Fail",
			isGetParametersFailure: true,
			expectedError:          "could not unmarshal GenerateReportParameters: error in fetching parameters",
		},
		{
			name:                "testRun_GetObject_Fail",
			isDataStoreGetError: true,
			expectedError:       "could not get the file from object store: Error in fetching the object from object store",
		},
		{
			name:             "testRun_GetObject_Unmarshall_Fail",
			isUnmarshalError: true,
			expectedError:    "error in unmarshalling the report content: EOF",
		},
		{
			name:                       "testRun_BucketExistence_Fail",
			isBucketExistenceCheckFail: true,
			expectedError:              "error in checking the existence of bucket in objectstore: error in checking the existence of a bucket",
		},
		{
			name:               "testRun_CreateBucket_Fail",
			isCreateBucketFail: true,
			expectedError:      "error in creating a bucket in objectstore: error in creating a bucket in object store",
		},
		{
			name:                "testRun_PutObject_Fail",
			isDataStorePutError: true,
			expectedError:       "error in storing the data object to objectstore: error in storing object to object store",
		},
		{
			name:                     "testRun_PreSignedURL_Fail",
			isPresignedGetObjectFail: true,
			expectedError:            "error in creating a presigned url for object: error in getting pre-signed URL",
		},
		{
			name:                "testRun_CSVExporter_PutObject_Fail",
			expectedError:       "error in storing the data object to objectstore: error in storing object to object store",
			isCSVExporter:       true,
			isDataStorePutError: true,
		},
		{
			name:                            "testRun_Object_Not_in_ObjStore",
			isObjectNotAvailableInDataStore: true,
			expectedError:                   "",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {

			conn, err := grpc.DialContext(context.Background(), "", grpc.WithInsecure(), grpc.WithContextDialer(dialer(t)))
			assert.NoError(t, err)

			defer conn.Close()
			complianceReportingClient := reporting.NewReportingServiceClient(conn)

			task := worker.GenerateReportTask{
				ObjStoreClient: mockObjStore{
					T:                         t,
					ForGetObjectFailure:       tc.isDataStoreGetError,
					ForPutObjectFailure:       tc.isDataStorePutError,
					ForUnmarshallFailure:      tc.isUnmarshalError,
					IsBucketExisted:           tc.isBucketExisted,
					ForBucketExistenceFail:    tc.isBucketExistenceCheckFail,
					ForMakeBucketFail:         tc.isCreateBucketFail,
					ForPresignedGetObjectFail: tc.isPresignedGetObjectFail,
					ForCSVExporter:            tc.isCSVExporter,
					ForObjectNotAvailable:     tc.isObjectNotAvailableInDataStore,
				},
				ObjBucketName:             "testBucket",
				ComplianceReportingClient: complianceReportingClient,
			}
			result, err := task.Run(context.Background(), &CerealTask{
				isParameterFailure: tc.isGetParametersFailure,
				isCSVExport:        tc.isCSVExporter,
			})
			if tc.expectedError != "" {
				assert.Error(t, err)
				assert.Equal(t, tc.expectedError, err.Error())
			}
			if tc.expectedError == "" {
				assert.NoError(t, err)
			}
			if result != nil {
				formattedResult := result.(*worker.GenerateReportParameters)
				assert.Equal(t, int64(8200), formattedResult.ReportSize)
			}
		})
	}
}

func dialer(t *testing.T) func(context.Context, string) (net.Conn, error) {
	listener := bufconn.Listen(1024 * 1024)

	server := grpc.NewServer()

	reporting.RegisterReportingServiceServer(server, &mockReportingServer{})

	go func() {
		if err := server.Serve(listener); err != nil {
			log.Fatal(err)
		}
	}()

	return func(context.Context, string) (net.Conn, error) {
		return listener.Dial()
	}
}

type mockReportingServer struct {
	T *testing.T
}

func (m *mockReportingServer) GetReportListForReportManager(filters *reporting.ListFilters, stream reporting.ReportingService_GetReportListForReportManagerServer) error {

	result := reporting.ReportListForReportManagerResponse{}
	result.Reports = append(result.Reports, &reporting.ReportResponse{
		ReportId: "r1",
		Profiles: []*reporting.ProfileResponse{
			{
				ProfileId: "r1p1",
				Controls:  []string{"r1p1c1", "r1p1c2"},
			},
			{
				ProfileId: "r1p2",
				Controls:  []string{"r1p2c1", "r1p2c2"},
			},
		},
	})
	result.Reports = append(result.Reports, &reporting.ReportResponse{
		ReportId: "r2",
		Profiles: []*reporting.ProfileResponse{
			{
				ProfileId: "r2p1",
				Controls:  []string{"r2p1c1", "r2p1c2"},
			},
			{
				ProfileId: "r2p2",
				Controls:  []string{"r2p2c1", "r2p2c2"},
			},
		},
	})

	jsonBytes, _ := json.Marshal(result)

	reader := bytes.NewReader(jsonBytes)
	buffer := make([]byte, 1024)

	for {
		n, err := reader.Read(buffer)
		if err == io.EOF {
			break
		}
		request := &reporting.ReportContentResponse{Content: buffer[:n]}
		logrus.Debugf("sending %d bytes", n)
		err = stream.Send(request)
		assert.NoError(m.T, err)
	}
	return nil
}

func (m *mockReportingServer) Export(*reporting.Query, reporting.ReportingService_ExportServer) error {
	return nil
}
func (m *mockReportingServer) ExportNode(*reporting.Query, reporting.ReportingService_ExportNodeServer) error {
	return nil
}

func (m *mockReportingServer) ListReports(context.Context, *reporting.Query) (*reporting.ReportsSummaryLevelOne, error) {
	return nil, nil
}
func (m *mockReportingServer) ListReportIds(context.Context, *reporting.Query) (*reporting.ReportIds, error) {
	return nil, nil
}
func (m *mockReportingServer) ReadReport(context.Context, *reporting.Query) (*reporting.Report, error) {
	return nil, nil
}
func (m *mockReportingServer) ListSuggestions(context.Context, *reporting.SuggestionRequest) (*reporting.Suggestions, error) {
	return nil, nil
}
func (m *mockReportingServer) ListProfiles(context.Context, *reporting.Query) (*reporting.ProfileMins, error) {
	return nil, nil
}

func (m *mockReportingServer) ExportReportManager(context.Context, *reporting.Query) (*reporting.CustomReportResponse, error) {
	return nil, nil
}
func (m *mockReportingServer) ReadNode(context.Context, *reporting.Id) (*reporting.Node, error) {
	return nil, nil
}
func (m *mockReportingServer) ListNodes(context.Context, *reporting.Query) (*reporting.Nodes, error) {
	return nil, nil
}
func (m *mockReportingServer) ListControlItems(context.Context, *reporting.ControlItemRequest) (*reporting.ControlItems, error) {
	return nil, nil
}
func (m *mockReportingServer) ReadNodeHeader(context.Context, *reporting.Query) (*reporting.NodeHeaderInfo, error) {
	return nil, nil
}
func (m *mockReportingServer) ListControlInfo(context.Context, *reporting.Query) (*reporting.ControlElements, error) {
	return nil, nil
}
func (m *mockReportingServer) GetReportContent(context.Context, *reporting.ReportContentRequest) (*reporting.ReportContentResponse, error) {

	report := compliance.Report{
		ReportUuid: "r1",
		NodeUuid:   "nodeID",
		NodeName:   "TestNode",
		Profiles: []*inspec.Profile{
			{
				Name:   "r1p1",
				Title:  "r1p1",
				Sha256: "r1p1",
				Controls: []*inspec.Control{
					{
						Id:    "r1p1c1",
						Title: "r1p1c1",
					},
					{
						Id:    "r1p1c2",
						Title: "r1p1c2",
					},
				},
			},
			{
				Name:   "r1p2",
				Title:  "r1p2",
				Sha256: "r1p2",
				Controls: []*inspec.Control{
					{
						Id:    "r1p2c1",
						Title: "r1p2c1",
					},
					{
						Id:    "r1p2c2",
						Title: "r1p2c2",
					},
				},
			},
		},
	}
	jsonBytes, err := json.Marshal(report)
	assert.NoError(m.T, err)

	return &reporting.ReportContentResponse{
		Content: jsonBytes,
	}, nil
}
