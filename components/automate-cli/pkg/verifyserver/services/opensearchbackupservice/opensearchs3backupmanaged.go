package opensearchbackupservice

import (
	"fmt"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/credentials"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber"
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

func NewOpenSearchclient() IOpenSearchclient {
	return &OpenSearchclient{}
}

func NewOSS3BackupService() IOSS3BackupService {
	return &OSS3BackupService{
		OSClient:     NewOpenSearchclient(),
		OSOperations: NewOpensearchOperations(),
	}
}

func (ss *OSS3BackupService) OSS3BackupVerify(request models.S3BackupDetails, ctx *fiber.Ctx) (models.S3BackupManagedResponse, error) {

	url := request.Endpoint

	fmt.Println("URL: ", url)

	client, err := ss.OSClient.CreateAWSClient(request, url)

	if err != nil {
		fmt.Println(err)
		return models.S3BackupManagedResponse{}, err
	}

	if _, err = ss.OSOperations.CreateTestIndex(client, ctx, TestIndexName); err != nil {
		fmt.Println(err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.S3BackupChecks{createFailedResponse(IndexCreateFailedMessage, IndexCreateFailedResolution)},
		}, err
	}

	snapshotCreateReq := SnapshotRepoRequestS3{
		Bucket:   request.S3Bucket,
		BasePath: request.S3BasePath,
		RoleArn:  request.AWSRoleArn,
		Region:   request.AWSRegion,
	}

	if _, err = ss.OSOperations.CreateSnapshotRepo(client, ctx, snapshotCreateReq, TestRepoName); err != nil {
		fmt.Println(err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.S3BackupChecks{createFailedResponse(SnapShotRepoCreateFailedMessage, SnapShotRepoCreateFailedResolution)},
		}, err
	}

	if _, err = ss.OSOperations.CreateSnapshot(client, ctx, TestRepoName, TestSnapshotName, TestIndexName); err != nil {
		fmt.Println(err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.S3BackupChecks{createFailedResponse(SnapShotCreateFailedMessage, SnapShotCreateFailedResolution)},
		}, err
	}

	if status, err := ss.OSOperations.GetSnapshotStatus(client, ctx, TestRepoName, TestSnapshotName); err != nil || status != "SUCCESS" {
		fmt.Println(err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.S3BackupChecks{createFailedResponse(SnapShotCreateFailedMessage, SnapShotCreateFailedResolution)},
		}, err
	}

	if _, err = ss.OSOperations.DeleteTestSnapshot(client, ctx, TestRepoName, TestSnapshotName); err != nil {
		fmt.Println(err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.S3BackupChecks{createFailedResponse(SnapShotDeleteFailedMessage, SnapShotDeleteFailedResolution)},
		}, err
	}

	if _, err = ss.OSOperations.DeleteTestSnapshotRepo(client, ctx, TestRepoName); err != nil {
		fmt.Println(err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.S3BackupChecks{createFailedResponse(SnapShotRepoDeleteFailedMessage, SnapShotRepoDeleteFailedResolution)},
		}, err
	}

	if _, err = ss.OSOperations.DeleteTestIndex(client, ctx, TestIndexName); err != nil {
		fmt.Println(err)
		return models.S3BackupManagedResponse{
			Passed: false,
			Checks: []models.S3BackupChecks{createFailedResponse(IndexDeleteFailedMessage, IndexDeleteFailedResolution)},
		}, err
	}

	return models.S3BackupManagedResponse{
		Passed: true,
		Checks: []models.S3BackupChecks{createSuccessResponse()},
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

func createSuccessResponse() models.S3BackupChecks {
	checkResp := models.S3BackupChecks{
		Title:         "Create test backup",
		Passed:        true,
		SuccessMsg:    "OpenSearch is able to create backup to provided S3",
		ErrorMsg:      "",
		ResolutionMsg: "",
	}

	return checkResp
}

func createFailedResponse(errMessage string, resolution string) models.S3BackupChecks {
	checkResp := models.S3BackupChecks{
		Title:         "Create test backup",
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      errMessage,
		ResolutionMsg: resolution,
	}

	return checkResp
}
