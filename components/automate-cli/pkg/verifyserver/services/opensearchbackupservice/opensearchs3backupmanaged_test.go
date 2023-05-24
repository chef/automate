package opensearchbackupservice

import (
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
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
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := OSS3BackupService{
		OSClient:     SetupMockOpensearchClient(""),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: false, Err: errors.New("Index Creation failed")}}),
		Log:          log,
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to create an index on the Opensearch Domain","resolution_msg":"Setup Opensearch with valid Configuration and provide the IAM user proper permissions to create an index."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestSnapshotRepoCreationFailed(t *testing.T) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := OSS3BackupService{
		OSClient:     SetupMockOpensearchClient(""),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: true, Err: nil}, {TestCase: false, Err: errors.New("Snapshot Repo Creation failed")}}),
		Log:          log,
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to create the Snapshot Repository on the Opensearch Domain","resolution_msg":"Setup Opensearch with valid Configuration and provide the IAM user proper permissions to create the Snapsshot Repository."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestSnapshotCreationFailed(t *testing.T) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := OSS3BackupService{
		OSClient: SetupMockOpensearchClient(""),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: true, Err: nil}, {TestCase: true, Err: nil},
			{TestCase: false, Err: errors.New("Snapshot Creation failed")}}),
		Log: log,
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to create the Snapshot on the Opensearch Domain","resolution_msg":"Setup Opensearch with valid Configuration and provide the IAM user proper permissions to create a Snapshot."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestSnapshotStatusFailed(t *testing.T) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := OSS3BackupService{
		OSClient: SetupMockOpensearchClient(""),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil},
			{TestCase: false, Err: errors.New("Snapshot Creation Status is failed")}}),
		Log: log,
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to create the Snapshot on the Opensearch Domain","resolution_msg":"Setup Opensearch with valid Configuration and provide the IAM user proper permissions to create a Snapshot."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestSnapshotDeletionFailed(t *testing.T) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := OSS3BackupService{
		OSClient:     SetupMockOpensearchClient(""),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: false, Err: errors.New("Snapshot Creation failed")}}),
		Log:          log,
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to delete the Snapshot on the Opensearch Domain","resolution_msg":"Provide the IAM user proper permissions to delete the Snapshot."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestSnapshotRepoDeletionFailed(t *testing.T) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := OSS3BackupService{
		OSClient:     SetupMockOpensearchClient(""),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: false, Err: errors.New("Snapshot Creation failed")}}),
		Log:          log,
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to delete the Snapshot Repository on the Opensearch Domain","resolution_msg":"Provide the IAM user proper permissions to delete the Snapshot Repository."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestIndexDeletionFailed(t *testing.T) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := OSS3BackupService{
		OSClient:     SetupMockOpensearchClient(""),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: false, Err: errors.New("Snapshot Creation failed")}}),
		Log:          log,
	}
	expectedResponse := `{"title":"Create test backup","passed":false,"success_msg":"","error_msg":"Failed to delete an index on the Opensearch Domain","resolution_msg":"Provide the IAM user proper permissions to delete an index."}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, false)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestSuccessfulBackup(t *testing.T) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	s := OSS3BackupService{
		OSClient:     SetupMockOpensearchClient(""),
		OSOperations: SetupMockOSOperations([]TestMockFunc{{TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}, {TestCase: true, Err: nil}}),
		Log:          log,
	}
	expectedResponse := `{"title":"Create test backup","passed":true,"success_msg":"OpenSearch is able to create backup to provided S3","error_msg":"","resolution_msg":""}`
	resp, _ := s.OSS3BackupVerify(request, &fiber.Ctx{})
	assert.Equal(t, resp.Passed, true)
	response, _ := json.Marshal(resp.Checks[0])
	assert.Equal(t, string(response), expectedResponse)
}

func TestCreateIndex(t *testing.T) {

	response := `{
		"took" : 982,
		"timed_out" : false,
		"_shards" : {
		  "total" : 13,
		  "successful" : 13,
		  "skipped" : 0,
		  "failed" : 0
		},
		"hits" : {
		  "total" : {
			"value" : 10000,
			"relation" : "gte"
		  },
		  "max_score" : 1.0,
		  "hits" : [
			{
			  "_index" : "companies",
			  "_type" : "_doc",
			  "_id" : "2ds34f6w-43f5-2344-dsf4-kf9ekw9fke9w",
			  "_score" : 1.0,
			  "_source" : {
				"id" : "4ks9fks0-434f-s9fd-f9s9-9fjufus0ds9d",
				"organization_id" : "9fks8fdd-sfdf9-f8sd-fsdf-fidis9df7gnw",
				"created_at" : "2019-02-07T17:20:59.554Z",
				"name" : "Project Foo"
			  }
			}
		  ]
	  }
}`
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	os := OpensearchOperations{
		Log: log,
	}
	testServer := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {}))
	defer testServer.Close()
	request.Endpoint = testServer.URL

	awsClient := SetupMockOpensearchClient(response)
	client, err := awsClient.CreateAWSClient(request, "testing")
	assert.NoError(t, err)

	resp, err := os.CreateTestIndex(client, &fiber.Ctx{}, "abc")
	assert.NoError(t, err)

	fmt.Println(resp)
}

func TestCreateSnapShotRepo(t *testing.T) {

	response := `{}`
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	os := OpensearchOperations{
		Log: log,
	}
	testServer := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {}))
	defer testServer.Close()
	request.Endpoint = testServer.URL

	awsClient := SetupMockOpensearchClient(response)
	client, err := awsClient.CreateAWSClient(request, "testing")
	assert.NoError(t, err)

	s3Request := SnapshotRepoRequestS3{
		Bucket:   "",
		BasePath: "",
		RoleArn:  "",
		Region:   "",
	}

	resp, err := os.CreateSnapshotRepo(client, &fiber.Ctx{}, s3Request, "abc")
	assert.NoError(t, err)

	fmt.Println(resp)
}

func TestDeleteSnapShot(t *testing.T) {

	response := `{}`
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	os := OpensearchOperations{
		Log: log,
	}
	testServer := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {}))
	defer testServer.Close()
	request.Endpoint = testServer.URL

	awsClient := SetupMockOpensearchClient(response)
	client, err := awsClient.CreateAWSClient(request, "testing")
	assert.NoError(t, err)

	resp, err := os.DeleteTestSnapshot(client, &fiber.Ctx{}, "repo", "snapshot")
	assert.NoError(t, err)

	fmt.Println(resp)
}

func TestDeleteSnapShotRepo(t *testing.T) {

	response := `{}`
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	os := OpensearchOperations{
		Log: log,
	}
	testServer := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {}))
	defer testServer.Close()
	request.Endpoint = testServer.URL

	awsClient := SetupMockOpensearchClient(response)
	client, err := awsClient.CreateAWSClient(request, "testing")
	assert.NoError(t, err)

	resp, err := os.DeleteTestSnapshotRepo(client, &fiber.Ctx{}, "repo")
	assert.NoError(t, err)

	fmt.Println(resp)
}

func TestDeleteIndex(t *testing.T) {

	response := `{}`
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	os := OpensearchOperations{
		Log: log,
	}
	testServer := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {}))
	defer testServer.Close()
	request.Endpoint = testServer.URL

	awsClient := SetupMockOpensearchClient(response)
	client, err := awsClient.CreateAWSClient(request, "testing")
	assert.NoError(t, err)

	resp, err := os.DeleteTestIndex(client, &fiber.Ctx{}, "index")
	assert.NoError(t, err)

	fmt.Println(resp)
}

func TestCreateSnapShot(t *testing.T) {

	response := `{}`
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		assert.Error(t, err)
	}
	os := OpensearchOperations{
		Log: log,
	}
	testServer := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {}))
	defer testServer.Close()
	request.Endpoint = testServer.URL

	awsClient := SetupMockOpensearchClient(response)
	client, err := awsClient.CreateAWSClient(request, "testing")
	assert.NoError(t, err)

	resp, err := os.CreateSnapshot(client, &fiber.Ctx{}, "repo", "snapshot", "index")
	assert.NoError(t, err)

	fmt.Println(resp)
}
