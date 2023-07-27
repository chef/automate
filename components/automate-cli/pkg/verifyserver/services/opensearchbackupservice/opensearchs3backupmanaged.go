package opensearchbackupservice

import (
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/credentials"
	"github.com/aws/aws-sdk-go/aws/session"
	constant "github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	elastic "github.com/olivere/elastic/v7"
	elasticaws "github.com/olivere/elastic/v7/aws/v4"
	"github.com/pkg/errors"
)

type IOSS3BackupService interface {
	OSS3BackupVerify(models.S3BackupDetails, *fiber.Ctx) (models.S3BackupManagedResponse, error)
}

type IOpenSearchclient interface {
	CreateAWSClient(models.S3BackupDetails, string) (*elastic.Client, error)
}

type OSS3BackupService struct {
	OSClient     IOpenSearchclient
	OSOperations IOpensearchOperations
	Log          logger.Logger
}

type SnapshotRepoRequest struct {
	Type     string                 `json:"type"`
	Settings map[string]interface{} `json:"settings"`
}

type SnapshotRequest struct {
	Indices           string `json:"indices"`
	IgnoreUnavailable bool   `json:"ignore_unavailable"`
	AllowNoIndices    bool   `json:"allow_no_indices"`
}

type OpenSearchclient struct{}

func NewOpenSearchclient() *OpenSearchclient {
	return &OpenSearchclient{}
}

func NewOSS3BackupService(log logger.Logger) *OSS3BackupService {
	return &OSS3BackupService{
		OSClient:     NewOpenSearchclient(),
		OSOperations: NewOpensearchOperations(log),
		Log:          log,
	}
}

func (ss *OSS3BackupService) OSS3BackupVerify(request models.S3BackupDetails, ctx *fiber.Ctx) (models.S3BackupManagedResponse, error) {

	url := request.Endpoint

	client, err := ss.OSClient.CreateAWSClient(request, url)

	if err != nil {
		ss.Log.Error(err)
		return models.S3BackupManagedResponse{}, err
	}

	if _, err = ss.OSOperations.CreateTestIndex(client, ctx, constant.TEST_INDEX_NAME); err != nil {
		ss.Log.Error("Index creation failed: ", err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.Checks{createFailedResponse(constant.INDEX_CREATE_FAILED_MESSAGE, constant.INDEX_CREATE_FAILED_RESOLUTION)},
		}, err
	}

	snapshotCreateReq := SnapshotRepoRequestS3{
		Bucket:   request.S3Bucket,
		BasePath: request.S3BasePath,
		RoleArn:  request.AWSRoleArn,
		Region:   request.AWSRegion,
	}

	if _, err = ss.OSOperations.CreateSnapshotRepo(client, ctx, snapshotCreateReq, constant.TEST_REPO_NAME); err != nil {
		ss.Log.Error("Snapshot Repo creation failed: ", err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.Checks{createFailedResponse(constant.SNAPSHOT_REPO_CREATE_FAILED_MESSAGE, constant.SNAPSHOT_REPO_CREATE_FAILED_RESOLUTION)},
		}, err
	}

	if _, err = ss.OSOperations.CreateSnapshot(client, ctx, constant.TEST_REPO_NAME, constant.TEST_SNAPSHOT_NAME, constant.TEST_INDEX_NAME); err != nil {
		ss.Log.Error("Snapshot creation failed: ", err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.Checks{createFailedResponse(constant.SNAPSHOT_CREATE_FAILED_MESSAGE, constant.SNAPSHOT_CREATE_FAILED_RSOLUTION)},
		}, err
	}

	if status, err := ss.OSOperations.GetSnapshotStatus(client, ctx, constant.TEST_REPO_NAME, constant.TEST_SNAPSHOT_NAME); err != nil || status == "FAILED" {
		ss.Log.Error("Snapshot Status check failed: ", err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.Checks{createFailedResponse(constant.SNAPSHOT_CREATE_FAILED_MESSAGE, constant.SNAPSHOT_CREATE_FAILED_RSOLUTION)},
		}, err
	}

	if _, err = ss.OSOperations.DeleteTestSnapshot(client, ctx, constant.TEST_REPO_NAME, constant.TEST_SNAPSHOT_NAME); err != nil {
		ss.Log.Error("Snapshot deleteion failed: ", err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.Checks{createFailedResponse(constant.SNAPSHOT_DELETE_FAILED_MESSAGE, constant.SNAPSHOT_DELETE_FAILED_RESOLUTION)},
		}, err
	}

	if _, err = ss.OSOperations.DeleteTestSnapshotRepo(client, ctx, constant.TEST_REPO_NAME); err != nil {
		ss.Log.Error("Snapshot Repo deletion failed: ", err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.Checks{createFailedResponse(constant.SNAPSHOT_REPO_DELETE_FAILED_MESSAGE, constant.SNAPSHOT_REPO_DELETE_FAILED_RESOLUTION)},
		}, err
	}

	if _, err = ss.OSOperations.DeleteTestIndex(client, ctx, constant.TEST_INDEX_NAME); err != nil {
		ss.Log.Error("Index deletion failed: ", err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.Checks{createFailedResponse(constant.INDEX_DELETE_FAILED_MESSAGE, constant.INDEX_DELETE_FAILED_RESOLUTION)},
		}, err
	}

	return models.S3BackupManagedResponse{
		Passed: true,
		Checks: []models.Checks{createSuccessResponse()},
	}, nil
}

func (os *OpenSearchclient) CreateAWSClient(request models.S3BackupDetails, url string) (*elastic.Client, error) {
	var creds *credentials.Credentials
	if request.AccessKey != "" && request.SecretKey != "" {
		creds = credentials.NewStaticCredentials(request.AccessKey, request.SecretKey, "")
	} else {
		sess, err := session.NewSession(&aws.Config{})
		if err != nil {
			return nil, errors.Wrap(err, "Failed to initialize session object")
		}
		creds = sess.Config.Credentials
	}

	httpClient := elasticaws.NewV4SigningClient(creds, request.AWSRegion)
	var err error
	createRepoClient, err := elastic.NewClient(
		elastic.SetURL(request.Endpoint),
		elastic.SetSniff(false),
		elastic.SetHttpClient(httpClient),
		elastic.SetHealthcheck(false),
	)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to create ES client with aws signing")
	}

	return createRepoClient, nil
}

func createSuccessResponse() models.Checks {
	checkResp := models.Checks{
		Title:         "Create test backup",
		Passed:        true,
		SuccessMsg:    "OpenSearch is able to create backup to provided S3",
		ErrorMsg:      "",
		ResolutionMsg: "",
	}

	return checkResp
}

func createFailedResponse(errMessage string, resolution string) models.Checks {
	checkResp := models.Checks{
		Title:         "Create test backup",
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      errMessage,
		ResolutionMsg: resolution,
	}

	return checkResp
}
