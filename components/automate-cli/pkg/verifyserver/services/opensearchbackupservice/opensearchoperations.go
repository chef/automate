package opensearchbackupservice

import (
	"encoding/json"
	"fmt"
	"net/url"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	elastic "github.com/olivere/elastic/v7"
)

type IOpensearchOperations interface {
	CreateTestIndex(*elastic.Client, *fiber.Ctx, string) (bool, error)
	CreateSnapshotRepo(*elastic.Client, *fiber.Ctx, SnapshotRepoRequestS3, string) (bool, error)
	CreateSnapshot(*elastic.Client, *fiber.Ctx, string, string, string) (bool, error)
	GetSnapshotStatus(*elastic.Client, *fiber.Ctx, string, string) (string, error)
	DeleteTestSnapshot(*elastic.Client, *fiber.Ctx, string, string) (bool, error)
	DeleteTestSnapshotRepo(*elastic.Client, *fiber.Ctx, string) (bool, error)
	DeleteTestIndex(*elastic.Client, *fiber.Ctx, string) (bool, error)
}

type OpensearchOperations struct {
	Log logger.Logger
}

type SnapshotStatus struct {
	Snapshots []snapshotStatus `json:"snapshots"`
}

type snapshotStatus struct {
	Name               string `json:"snapshot"`
	Repository         string `json:"repository"`
	UUID               string `json:"uuid"`
	State              string `json:"state"`
	IncludeGlobalState bool   `json:"include_global_state"`
}

func NewOpensearchOperations(log logger.Logger) *OpensearchOperations {
	return &OpensearchOperations{
		Log: log,
	}
}

type SnapshotRepoRequestS3 struct {
	Bucket   string
	BasePath string
	RoleArn  string
	Region   string
}

const TestIndexMapping = `{
	"settings": {
		"number_of_shards": 1,
		"number_of_replicas": 0
	},
	"mappings": {
		"properties": {
			"end_time": {
				"type": "date"
			}
		}
	}
}`

func (s3Request *SnapshotRepoRequestS3) createSnapshotRepoRequest() SnapshotRepoRequest {
	req := SnapshotRepoRequest{
		Type: "s3",
		Settings: map[string]interface{}{
			"bucket":    s3Request.Bucket,
			"base_path": s3Request.BasePath,
			"role_arn":  s3Request.RoleArn,
			"region":    s3Request.Region,
		},
	}
	return req
}

func createSnapshotRequest(index string) SnapshotRequest {
	req := SnapshotRequest{
		Indices:           index,
		IgnoreUnavailable: true,
		AllowNoIndices:    true,
	}

	return req
}

func (os *OpensearchOperations) CreateTestIndex(client *elastic.Client, ctx *fiber.Ctx, index string) (bool, error) {

	createIndex := client.CreateIndex(index)
	createIndex.BodyString(TestIndexMapping)
	res, err := createIndex.Do(ctx.Context())

	if err != nil {
		return false, err
	}

	os.Log.Debug("Index creation status ", res.Acknowledged)

	return res.Acknowledged, nil
}

func (os *OpensearchOperations) CreateSnapshotRepo(client *elastic.Client, ctx *fiber.Ctx, req SnapshotRepoRequestS3, repoName string) (bool, error) {
	createRepo := client.SnapshotCreateRepository(repoName)
	createRepo.BodyJson(req.createSnapshotRepoRequest())
	res, err := createRepo.Do(ctx.Context())
	if err != nil {
		return false, err
	}

	os.Log.Debug("Snapshot repo creation: ", res.Acknowledged)

	return res.Acknowledged, nil
}

func (os *OpensearchOperations) CreateSnapshot(client *elastic.Client, ctx *fiber.Ctx, repoName string, snapshotName string, index string) (bool, error) {
	createSnapshot := client.SnapshotCreate(repoName, snapshotName)
	createSnapshot.BodyJson(createSnapshotRequest(index))
	res, err := createSnapshot.Do(ctx.Context())
	if err != nil {
		return false, err
	}

	os.Log.Debug("Snapshot creation: ", res.Accepted)
	return *res.Accepted, nil
}

func (os *OpensearchOperations) GetSnapshotStatus(client *elastic.Client, ctx *fiber.Ctx, repoName string, snapshotName string) (string, error) {

	snapshotStatusUrl := fmt.Sprintf(constants.SNAPSHOT_STATUS_API, repoName, snapshotName)
	res, err := client.PerformRequest(ctx.Context(), elastic.PerformRequestOptions{
		Method: "GET",
		Path:   snapshotStatusUrl,
		Params: url.Values{},
	})

	if err != nil {
		return "", err
	}

	snapshotStatusList := SnapshotStatus{}

	err = json.Unmarshal(res.Body, &snapshotStatusList)
	if err != nil {
		return "", err
	}

	os.Log.Debug("Snapshot Status: ", snapshotStatusList.Snapshots[0].State)

	return snapshotStatusList.Snapshots[0].State, nil
}

func (os *OpensearchOperations) DeleteTestSnapshot(client *elastic.Client, ctx *fiber.Ctx, repoName string, snapshotName string) (bool, error) {

	deleteSnapshot, err := client.SnapshotDelete(repoName, snapshotName).Do(ctx.Context())
	if err != nil {
		return false, err
	}

	os.Log.Debug("Snapshot Deletion status: ", deleteSnapshot.Acknowledged)
	return deleteSnapshot.Acknowledged, nil
}

func (os *OpensearchOperations) DeleteTestSnapshotRepo(client *elastic.Client, ctx *fiber.Ctx, repoName string) (bool, error) {

	deleteRepo, err := client.SnapshotDeleteRepository(repoName).Do(ctx.Context())
	if err != nil {
		return false, err
	}

	os.Log.Debug("Snapshot Repo Deletion status: ", deleteRepo.Acknowledged)
	return deleteRepo.Acknowledged, nil
}

func (os *OpensearchOperations) DeleteTestIndex(client *elastic.Client, ctx *fiber.Ctx, index string) (bool, error) {
	deleteIndex, err := client.DeleteIndex(index).Do(ctx.Context())
	if err != nil {
		return false, err
	}

	os.Log.Debug("Index Deletion: ", deleteIndex.Acknowledged)
	return deleteIndex.Acknowledged, nil
}
