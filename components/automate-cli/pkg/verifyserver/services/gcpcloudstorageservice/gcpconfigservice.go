package gcpcloudstorageservice

import (
	"context"
	"encoding/json"
	"fmt"

	"google.golang.org/api/iterator"
	"google.golang.org/api/option"

	"google.golang.org/api/iterator"
	"google.golang.org/api/option"

	"cloud.google.com/go/storage"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/google/uuid"
	"github.com/pkg/errors"
)

type GCPCloudStorageConfig interface {
	GetGCPConnection(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks
	GetBucketAccess(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks
}

type GCPConfigService struct {
	Logger   logger.Logger
	Req      *models.GCPCloudStorageConfigRequest
	GCPUtils GCPUtils
		GCPUtils: NewGCPUtils(logger),
	}
}

func (ss *GCPConfigService) GetGCPConnection(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks {
	ss.Req = req
	client, err := ss.GCPUtils.NewSessionWithOptions(ctx, ss.Req.GcpServiceAccount)
	if err != nil {
		ss.Logger.Errorf("error while creating a client: %v", err)
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	defer client.Close()
	bucket := client.Bucket(ss.Req.BucketName)
	err = ss.GCPUtils.BucketAttributes(ctx, bucket)
	if err != nil {
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_BUCKET_NOT_FOUND).Error(), constants.GCP_CONNECTION_RESOLUTION_GENERAL_MSG, false)
	}

	return ss.Response(constants.GCP_CONNECTION_TITLE, constants.GCP_CONNECTION_SUCCESS_MSG, "", "", true)
}

func (ss *GCPConfigService) GetBucketAccess(ctx context.Context, req *models.GCPCloudStorageConfigRequest) *models.Checks {
	ss.Req = req
	client, err := ss.GCPUtils.NewSessionWithOptions(ctx, ss.Req.GcpServiceAccount)
	if err != nil {
		ss.Logger.Errorf("error while creating a client: %v", err)
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	// Upload data in GCP bucket
	uniqueID := uuid.New().String()
	fileName := constants.GCP_CHECK_FILE_PREFIX + uniqueID + ".txt"
	bucket := client.Bucket(ss.Req.BucketName)
	obj := bucket.Object(fileName)
	if err := ss.GCPUtils.NewUploader(ctx, obj); err != nil {
		ss.Logger.Errorf("Error uploading the objects")
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_BUCKET_UPLOAD_ERROR_MSG).Error(), constants.GCP_BUCKET_UPLOAD_RESOLUTION_MSG, false)
	}

	// read/list data in GCP bucket
	query := &storage.Query{Prefix: constants.GCP_CHECK_FILE_PREFIX}
	if err := ss.GCPUtils.ListObjects(ctx, bucket, query); err != nil {
		ss.Logger.Errorf("Error listing the objects")
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_BUCKET_LIST_ERROR_MSG).Error(), constants.GCP_BUCKET_LIST_RESOLUTION_MSG, false)
	}

	// Delete data in GCP bucket
	if err := ss.GCPUtils.DeleteObject(ctx, obj); err != nil {
		ss.Logger.Errorf("Error deleting the objects")
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_BUCKET_DELETE_ERROR_MSG).Error(), constants.GCP_BUCKET_DELETE_RESOLUTION_MSG, false)
	}

	return ss.Response(constants.GCP_BUCKET_ACCESS_TITLE, constants.GCP_CONNECTION_SUCCESS_MSG, "", "", true)
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

type GCPUtils interface {
	NewSessionWithOptions(ctx context.Context, gsa *models.GcpServiceAccount) (*storage.Client, error)
	DeleteObject(ctx context.Context, obj *storage.ObjectHandle) error
	ListObjects(ctx context.Context, bucket *storage.BucketHandle, query *storage.Query) error
	NewUploader(ctx context.Context, obj *storage.ObjectHandle) error
	BucketAttributes(ctx context.Context, bucket *storage.BucketHandle) error
}

func NewGCPUtils(logger logger.Logger) *GCPUtilsImpl {
	return &GCPUtilsImpl{
		Logger: logger,
	}
}

type GCPUtilsImpl struct {
	Logger logger.Logger
}

func (au *GCPUtilsImpl) NewSessionWithOptions(ctx context.Context, gsa *models.GcpServiceAccount) (*storage.Client, error) {
	bx, _ := json.Marshal(gsa)
	return storage.NewClient(ctx, option.WithCredentialsJSON(bx))
}

func (au *GCPUtilsImpl) NewUploader(ctx context.Context, uploadObject *storage.ObjectHandle) error {
	wc := uploadObject.NewWriter(ctx)
	_, err := fmt.Fprintf(wc, "test message")
	if err != nil {
		return err
	}
	return wc.Close()
}

func (au *GCPUtilsImpl) DeleteObject(ctx context.Context, obj *storage.ObjectHandle) error {
	return obj.Delete(ctx)
}

func (au *GCPUtilsImpl) ListObjects(ctx context.Context, bucket *storage.BucketHandle, query *storage.Query) error {
	it := bucket.Objects(ctx, query)
	for attrs, err := it.Next(); err != iterator.Done; attrs, err = it.Next() {
		if err != nil {
			au.Logger.Errorf("Error listing the objects")
			return err
		}
		au.Logger.Debug("Objects in the bucket", attrs.Name)
	}
	return nil
}

func (au *GCPUtilsImpl) BucketAttributes(ctx context.Context, bucket *storage.BucketHandle) error {
	if _, err := bucket.Attrs(ctx); err != nil {
		return err
	}
	return nil
}
