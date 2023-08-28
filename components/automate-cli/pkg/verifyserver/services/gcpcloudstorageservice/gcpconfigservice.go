package gcpcloudstorageservice

import (
	"context"
	"encoding/json"
	"fmt"

	"cloud.google.com/go/storage"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/gcputils"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/api/iterator"
	"google.golang.org/api/option"

	"github.com/chef/automate/lib/logger"
)

type GCPCloudStorageConfig interface {
	GetGCPConnection(*models.GCPCloudStorageConfigRequest) *models.Checks
	GetBucketAccess(*models.GCPCloudStorageConfigRequest) *models.Checks
}

type GCPConfigService struct {
	Logger   logger.Logger
	Req      *models.GCPCloudStorageConfigRequest
	GcpUtils gcputils.GCPUtils
}

func NewGCPCloudStorageConfig(logger logger.Logger) GCPCloudStorageConfig {
	return &GCPConfigService{
		Logger: logger,
	}
}

func (ss *GCPConfigService) GetGCPConnection(req *models.GCPCloudStorageConfigRequest) *models.Checks {
	ss.Req = req
	ctx := context.Background()
	bx, err := json.Marshal(ss.Req.GcpServiceAccount)
	if err != nil {
		logrus.Errorf("error while marshling to binary: %v", err)
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	client, err := storage.NewClient(ctx, option.WithCredentialsJSON(bx))
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

	// GCP connection
	ctx := context.Background()
	bx, err := json.Marshal(ss.Req.GcpServiceAccount)
	if err != nil {
		logrus.Errorf("error while marshling to binary: %v", err)
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}
	client, err := storage.NewClient(ctx, option.WithCredentialsJSON(bx))
	if err != nil {
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
	}

	// Upload data in GCP bucket
	fileName := "test.txt"
	bucket := client.Bucket(ss.Req.BucketName)
	uploadObject := bucket.Object(fileName)

	wc := uploadObject.NewWriter(ctx)
	if _, err := fmt.Fprintf(wc, "Heute ist ein schöner Tag."); err != nil {
		return nil
	}
	if err := wc.Close(); err != nil {
		return nil
	}
	fmt.Printf("Created and uploaded content to gs://%s/%s\n", ss.Req.BucketName, fileName)

	// read/list data in GCP bucket
	query := &storage.Query{Prefix: "test"}

	it := bucket.Objects(ctx, query)
	for {
		attrs, err := it.Next()
		if err == iterator.Done {
			break
		}

		if err != nil {
			logrus.Errorf("Error listing the objects")
			return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
		}

		fmt.Println("The bucket", attrs.Name)
	}

	// Delete data in GCP bucket
	if err := uploadObject.Delete(ctx); err != nil {
		logrus.Errorf("Error deleting the objects")
		return ss.Response(constants.GCP_CONNECTION_TITLE, "", errors.Wrap(err, constants.GCP_CONNECTION_ERROR_MSG).Error(), constants.GCP_CONNECTION_RESOLUTION_MSG, false)
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
