package gcpcloudstorageservice

import (
	"context"
	"fmt"

	"cloud.google.com/go/storage"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/gcputils"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/api/iterator"
)

type GCPCloudStorageConfig interface {
	GetGCPConnection(*models.GCPCloudStorageConfigRequest) *models.Checks
	GetBucketAccess(*models.GCPCloudStorageConfigRequest) *models.Checks
}

type GCPConfigService struct {
	Logger   logger.Logger
	Req      *models.GCPCloudStorageConfigRequest
	GCPUtils gcputils.GCPUtils
}

func NewGCPCloudStorageConfig(logger logger.Logger, gcpUtils gcputils.GCPUtils) GCPCloudStorageConfig {
	return &GCPConfigService{
		Logger:   logger,
		GCPUtils: gcpUtils,
	}
}

func (ss *GCPConfigService) GetGCPConnection(req *models.GCPCloudStorageConfigRequest) *models.Checks {
	ss.Req = req
	ctx := context.Background()
	client, err := ss.GcpConnection(ctx, ss.Req.GoogleServiceAccountFile)
	if err != nil {
		logrus.Errorf("error while creating a client: %v", err)
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	defer client.Close()
	bucket := client.Bucket(ss.Req.BucketName)
	_, err = bucket.Attrs(ctx)
	if err != nil {
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_BUCKET_NOT_FOUND).Error(), constants.GCP_BUCKET_NOT_FOUND_RESOLUTION_MSG, false)
	}

	return ss.Response(constants.GCP_CONNECTION_TITLE, constants.GCP_CONNECTION_SUCCESS_MSG, "", "", true)
}

func (ss *GCPConfigService) GetBucketAccess(req *models.GCPCloudStorageConfigRequest) *models.Checks {
	ss.Req = req
	ctx := context.Background()
	client, err := ss.GcpConnection(ctx, ss.Req.GoogleServiceAccountFile)
	if err != nil {
		logrus.Errorf("error while creating a client: %v", err)
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	// Upload data in GCP bucket
	fileName := "test"
	bucket := client.Bucket(ss.Req.BucketName)
	if err := ss.UploadObject(ctx, bucket, fileName); err != nil {
		logrus.Errorf("Error uploading the objects")
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	// read/list data in GCP bucket
	if err := ss.ListObjects(ctx, client, bucket); err != nil {
		logrus.Errorf("Error listing the objects")
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	// Delete data in GCP bucket
	if err := bucket.Delete(ctx); err != nil {
		logrus.Errorf("Error deleting the objects")
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	return ss.Response(constants.GCP_BUCKET_ACCESS_TITLE, constants.GCP_CONNECTION_SUCCESS_MSG, "", "", true)
}

func (ss *GCPConfigService) GcpConnection(ctx context.Context, filePath string) (*storage.Client, error) {
	return ss.GCPUtils.NewSessionWithOptions(ctx, filePath)
}

func (ss *GCPConfigService) UploadObject(ctx context.Context, bucket *storage.BucketHandle, file string) error {
	_, err := ss.GCPUtils.NewUploader(ctx, bucket, file)
	if err != nil {
		return err
	}
	return nil
}

func (ss *GCPConfigService) ListObjects(ctx context.Context, client *storage.Client, bucket *storage.BucketHandle) error {
	query := &storage.Query{Prefix: "test"}

	it := bucket.Objects(ctx, query)
	for {
		attrs, err := it.Next()
		if err == iterator.Done {
			break
		}

		if err != nil {
			logrus.Errorf("Error listing the objects")
			return err
		}

		fmt.Println("The bucket", attrs.Name)
	}
	return nil
}

func (ss *GCPConfigService) Response(Title, SuccessMsg, ErrorMsg, ResolutionMsg string, Passed bool) *models.Checks {
	return &models.Checks{
		Title:         Title,
		Passed:        Passed,
		SuccessMsg:    SuccessMsg,
		ErrorMsg:      ErrorMsg,
		ResolutionMsg: ResolutionMsg,
	}
}
