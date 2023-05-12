package opensearchbackupservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber"
	elastic "github.com/olivere/elastic/v7"
)

type TestMockFunc struct {
	TestCase bool
	Err      error
}

type MockOSS3BackupService struct {
	OSS3BackupVerifyFunc func(models.S3BackupDetails, *fiber.Ctx) (models.S3BackupManagedResponse, error)
}

func (mos *MockOSS3BackupService) OSS3BackupVerify(request models.S3BackupDetails, ctx *fiber.Ctx) (models.S3BackupManagedResponse, error) {
	return mos.OSS3BackupVerifyFunc(request, ctx)
}

type MockOpensearchClient struct {
	CreateAWSClientFunc func(models.S3BackupDetails, string) (*elastic.Client, error)
}

func (osc *MockOpensearchClient) CreateAWSClient(req models.S3BackupDetails, url string) (*elastic.Client, error) {
	return osc.CreateAWSClientFunc(req, url)
}

func SetupMockOpensearchClient() IOpenSearchclient {
	return &MockOpensearchClient{
		CreateAWSClientFunc: func(req models.S3BackupDetails, url string) (*elastic.Client, error) {
			return &elastic.Client{}, nil
		},
	}
}

type MockOSOperations struct {
	CreateTestIndexFunc        func(*elastic.Client, *fiber.Ctx, string) (bool, error)
	CreateSnapshotRepoFunc     func(*elastic.Client, *fiber.Ctx, SnapshotRepoRequestS3, string) (bool, error)
	CreateSnapshotFunc         func(*elastic.Client, *fiber.Ctx, string, string, string) (bool, error)
	GetSnapshotStatusFunc      func(*elastic.Client, *fiber.Ctx, string, string) (string, error)
	DeleteTestSnapshotFunc     func(*elastic.Client, *fiber.Ctx, string, string) (bool, error)
	DeleteTestSnapshotRepoFunc func(*elastic.Client, *fiber.Ctx, string) (bool, error)
	DeleteTestIndexFunc        func(*elastic.Client, *fiber.Ctx, string) (bool, error)
}

func (mco *MockOSOperations) CreateTestIndex(client *elastic.Client, ctx *fiber.Ctx, index string) (bool, error) {
	return mco.CreateTestIndexFunc(client, ctx, index)
}

func (mco *MockOSOperations) CreateSnapshotRepo(client *elastic.Client, ctx *fiber.Ctx, req SnapshotRepoRequestS3, repo string) (bool, error) {
	return mco.CreateSnapshotRepoFunc(client, ctx, req, repo)
}

func (mco *MockOSOperations) CreateSnapshot(client *elastic.Client, ctx *fiber.Ctx, repoName string, snapshotName string, index string) (bool, error) {
	return mco.CreateSnapshotFunc(client, ctx, repoName, snapshotName, index)
}

func (mco *MockOSOperations) GetSnapshotStatus(client *elastic.Client, ctx *fiber.Ctx, repoName string, snapshotName string) (string, error) {
	return mco.GetSnapshotStatusFunc(client, ctx, repoName, snapshotName)
}

func (mco *MockOSOperations) DeleteTestSnapshot(client *elastic.Client, ctx *fiber.Ctx, repoName string, snapshotName string) (bool, error) {
	return mco.DeleteTestSnapshotFunc(client, ctx, repoName, snapshotName)
}

func (mco *MockOSOperations) DeleteTestSnapshotRepo(client *elastic.Client, ctx *fiber.Ctx, repoName string) (bool, error) {
	return mco.DeleteTestSnapshotRepoFunc(client, ctx, repoName)
}

func (mco *MockOSOperations) DeleteTestIndex(client *elastic.Client, ctx *fiber.Ctx, index string) (bool, error) {
	return mco.DeleteTestIndexFunc(client, ctx, index)
}

func SetupMockOSOperations(resp []TestMockFunc) IOpensearchOperations {

	return &MockOSOperations{
		CreateTestIndexFunc: func(client *elastic.Client, ctx *fiber.Ctx, index string) (bool, error) {
			return resp[0].TestCase, resp[0].Err
		},
		CreateSnapshotRepoFunc: func(client *elastic.Client, ctx *fiber.Ctx, req SnapshotRepoRequestS3, repo string) (bool, error) {
			return resp[1].TestCase, resp[1].Err
		},
		CreateSnapshotFunc: func(client *elastic.Client, ctx *fiber.Ctx, repoName string, snapshotName string, index string) (bool, error) {
			return resp[2].TestCase, resp[2].Err
		},
		GetSnapshotStatusFunc: func(client *elastic.Client, ctx *fiber.Ctx, repoName string, snapshotName string) (string, error) {
			if resp[3].TestCase {
				return "SUCCESS", resp[3].Err
			}
			return "FAILED", resp[3].Err
		},
		DeleteTestSnapshotFunc: func(client *elastic.Client, ctx *fiber.Ctx, repoName string, snapshotName string) (bool, error) {
			return resp[4].TestCase, resp[4].Err
		},
		DeleteTestSnapshotRepoFunc: func(client *elastic.Client, ctx *fiber.Ctx, repoName string) (bool, error) {
			return resp[5].TestCase, resp[5].Err
		},
		DeleteTestIndexFunc: func(client *elastic.Client, ctx *fiber.Ctx, index string) (bool, error) {
			return resp[6].TestCase, resp[6].Err
		},
	}

}
