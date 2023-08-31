package gcpcloudstorageservice

import (
	"context"

	"cloud.google.com/go/storage"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/google/uuid"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type GCPCloudStorageConfig interface {
	GetGCPConnection(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks
	GetBucketAccess(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks
}

type GCPConfigService struct {
	Logger   logger.Logger
	Req      *models.GCPCloudStorageConfigRequest
	GCPUtils GCPUtils
}

func NewGCPCloudStorageConfig(logger logger.Logger, gcpUtils GCPUtils) GCPCloudStorageConfig {
	return &GCPConfigService{
		Logger:   logger,
		GCPUtils: gcpUtils,
	}
}

func (ss *GCPConfigService) GetGCPConnection(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks {
	ss.Req = req
	client, err := ss.GcpConnection(ctx, ss.Req.GcpServiceAccount)
	if err != nil {
		logrus.Errorf("error while creating a client: %v", err)
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	defer client.Close()
	bucket := client.Bucket(ss.Req.BucketName)
	err = ss.BucketAttributes(ctx, bucket)
	if err != nil {
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_BUCKET_NOT_FOUND).Error(), constants.GCP_BUCKET_NOT_FOUND_RESOLUTION_MSG, false)
	}

	return ss.Response(constants.GCP_CONNECTION_TITLE, constants.GCP_CONNECTION_SUCCESS_MSG, "", "", true)
}

func (ss *GCPConfigService) GetBucketAccess(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks {
	ss.Req = req
	client, err := ss.GcpConnection(ctx, ss.Req.GcpServiceAccount)
	if err != nil {
		logrus.Errorf("error while creating a client: %v", err)
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	// Upload data in GCP bucket
	uniqueID := uuid.New().String()
	fileName := constants.GCP_CHECK_FILE_PREFIX + uniqueID + ".txt"
	bucket := client.Bucket(ss.Req.BucketName)
	obj := bucket.Object(fileName)

	if err := ss.UploadObject(ctx, obj); err != nil {
		logrus.Errorf("Error uploading the objects")
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	// read/list data in GCP bucket
	if err := ss.ListObjects(ctx, client, bucket); err != nil {
		logrus.Errorf("Error listing the objects")
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	// Delete data in GCP bucket
	if err := ss.DeleteObject(ctx, obj); err != nil {
		logrus.Errorf("Error deleting the objects")
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	return ss.Response(constants.GCP_BUCKET_ACCESS_TITLE, constants.GCP_CONNECTION_SUCCESS_MSG, "", "", true)
}

func (ss *GCPConfigService) GcpConnection(ctx context.Context, gsa *models.GcpServiceAccount) (*storage.Client, error) {
	return ss.GCPUtils.NewSessionWithOptions(ctx, gsa)
}

func (ss *GCPConfigService) BucketAttributes(ctx context.Context, bucket *storage.BucketHandle) error {
	return ss.GCPUtils.BucketAttributes(ctx, bucket)
}

func (ss *GCPConfigService) UploadObject(ctx context.Context, obj *storage.ObjectHandle) error {
	err := ss.GCPUtils.NewUploader(ctx, obj)
	if err != nil {
		return err
	}
	return nil
}

func (ss *GCPConfigService) ListObjects(ctx context.Context, client *storage.Client, bucket *storage.BucketHandle) error {
	query := &storage.Query{Prefix: constants.GCP_CHECK_FILE_PREFIX}
	err := ss.GCPUtils.ListObjects(ctx, bucket, query)
	if err != nil {
		return err
	}
	return nil
}

func (ss *GCPConfigService) DeleteObject(ctx context.Context, obj *storage.ObjectHandle) error {
	err := ss.GCPUtils.DeleteObject(ctx, obj)
	if err != nil {
		logrus.Errorf("ERROR DELETE: %+v", err)
		return err
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
