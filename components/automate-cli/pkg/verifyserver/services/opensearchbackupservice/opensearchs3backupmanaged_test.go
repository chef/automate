package opensearchbackupservice

import (
	"encoding/json"
	"errors"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber"
	"github.com/stretchr/testify/assert"
)

var request = models.S3BackupDetails{
	Endpoint:   "",
	Username:   "",
	Password:   "",
	S3Bucket:   "",
	S3BasePath: "",
	AccessKey:  "",
	SecretKey:  "",
	AWSRegion:  "",
	AWSRoleArn: "",
}

func TestIndexCreationFailed(t *testing.T) {
	s := OSS3BackupService{
		OSClient:     SetupMockOpensearchClient(),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: false, Err: errors.New("Index Creation failed")}}),
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to create an index on the Opensearch Domain","resolution_msg":"Setup Opensearch with valid Configuration and provide the IAM user proper permissions to create an index."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestSnapshotRepoCreationFailed(t *testing.T) {
	s := OSS3BackupService{
		OSClient: SetupMockOpensearchClient(),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{
			TestCase: true,
			Err:      nil,
		}, {TestCase: false, Err: errors.New("Snapshot Repo Creation failed")}}),
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to create the Snapshot Repository on the Opensearch Domain","resolution_msg":"Setup Opensearch with valid Configuration and provide the IAM user proper permissions to create the Snapsshot Repository."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestSnapshotCreationFailed(t *testing.T) {
	s := OSS3BackupService{
		OSClient: SetupMockOpensearchClient(),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: true, Err: nil}, {TestCase: true, Err: nil},
			{TestCase: false, Err: errors.New("Snapshot Creation failed")}}),
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to create the Snapshot on the Opensearch Domain","resolution_msg":"Setup Opensearch with valid Configuration and provide the IAM user proper permissions to create a Snapshot."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestSnapshotStatusFailed(t *testing.T) {
	s := OSS3BackupService{
		OSClient: SetupMockOpensearchClient(),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil},
			{TestCase: false, Err: errors.New("Snapshot Creation Status is failed")}}),
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to create the Snapshot on the Opensearch Domain","resolution_msg":"Setup Opensearch with valid Configuration and provide the IAM user proper permissions to create a Snapshot."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestSnapshotDeletionFailed(t *testing.T) {
	s := OSS3BackupService{
		OSClient: SetupMockOpensearchClient(),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil},
			{TestCase: false, Err: errors.New("Snapshot Creation failed")}}),
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to delete the Snapshot on the Opensearch Domain","resolution_msg":"Provide the IAM user proper permissions to delete the Snapshot."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestSnapshotRepoDeletionFailed(t *testing.T) {
	s := OSS3BackupService{
		OSClient: SetupMockOpensearchClient(),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil},
			{TestCase: false, Err: errors.New("Snapshot Creation failed")}}),
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to delete the Snapshot Repository on the Opensearch Domain","resolution_msg":"Provide the IAM user proper permissions to delete the Snapshot Repository."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestIndexDeletionFailed(t *testing.T) {
	s := OSS3BackupService{
		OSClient: SetupMockOpensearchClient(),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil},
			{TestCase: false, Err: errors.New("Snapshot Creation failed")}}),
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to delete an index on the Opensearch Domain","resolution_msg":"Provide the IAM user proper permissions to delete an index."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestSuccessfulBackup(t *testing.T) {
	s := OSS3BackupService{
		OSClient: SetupMockOpensearchClient(),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil},
			{TestCase: true, Err: nil}}),
	}
	expectedResponse := `{"title":"Create test backup","passed":true,"success_msg":"OpenSearch is able to create backup to provided S3","error_msg":"","resolution_msg":""}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, true)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}
