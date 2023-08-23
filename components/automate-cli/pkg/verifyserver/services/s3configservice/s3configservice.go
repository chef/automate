package s3configservice

import (
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/awsutils"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
)

type IS3Config interface {
	GetS3Connection(*models.S3ConfigRequest) *models.Checks
	GetBucketAccess(*models.S3ConfigRequest) *models.Checks
}

type S3ConfigService struct {
	Logger   logger.Logger
	Req      *models.S3ConfigRequest
	AwsUtils awsutils.AwsUtils
}

func NewS3ConfigService(logger logger.Logger, awsUtils awsutils.AwsUtils) IS3Config {
	return &S3ConfigService{
		Logger:   logger,
		AwsUtils: awsUtils,
	}
}

func (ss *S3ConfigService) GetS3Connection(req *models.S3ConfigRequest) *models.Checks {
	ss.Req = req
	sess, err := ss.AwsConnection(ss.Req.Endpoint, ss.Req.AccessKey, ss.Req.SecretKey, ss.Req.Region)
	if err != nil {
		return ss.Response(constants.S3_CONNECTION_TITLE, "", errors.Wrap(err, constants.S3_CONNECTION_ERROR_MSG).Error(), constants.S3_CONNECTION_RESOLUTION_MSG, false)
	}
	s3Client := ss.AwsUtils.New(sess)
	err = ss.ListBuckets(s3Client)
	if err != nil {
		return ss.Response(constants.S3_CONNECTION_TITLE, "", errors.Wrap(err, constants.S3_CONNECTION_ERROR_MSG).Error(), constants.S3_CONNECTION_RESOLUTION_MSG, false)
	}
	return ss.Response(constants.S3_CONNECTION_TITLE, constants.S3_CONNECTION_SUCCESS_MSG, "", "", true)
}

func (ss *S3ConfigService) GetBucketAccess(req *models.S3ConfigRequest) *models.Checks {
	ss.Req = req
	// S3 connection
	sess, err := ss.AwsConnection(ss.Req.Endpoint, ss.Req.AccessKey, ss.Req.SecretKey, ss.Req.Region)
	if err != nil {
		return ss.Response(constants.S3_BUCKET_ACCESS_TITLE, "", errors.Wrap(err, constants.S3_BUCKET_ACCESS_ERROR_MSG).Error(), constants.S3_BUCKET_ACCESS_RESOLUTION_MSG, false)
	}

	s3Client := ss.AwsUtils.New(sess)
	// upload data in s3 bucket
	err = ss.UploadObject(sess)
	if err != nil {
		return ss.Response(constants.S3_BUCKET_ACCESS_TITLE, "", errors.Wrap(err, constants.S3_BUCKET_ACCESS_ERROR_MSG).Error(), constants.S3_BUCKET_ACCESS_RESOLUTION_MSG, false)
	}

	// read/list data in s3 bucket
	err = ss.ListObjects(s3Client)
	if err != nil {
		return ss.Response(constants.S3_BUCKET_ACCESS_TITLE, "", errors.Wrap(err, constants.S3_BUCKET_ACCESS_ERROR_MSG).Error(), constants.S3_BUCKET_ACCESS_RESOLUTION_MSG, false)
	}

	// delete data in s3 bucket
	err = ss.DeleteObjects(s3Client)
	if err != nil {
		return ss.Response(constants.S3_BUCKET_ACCESS_TITLE, "", errors.Wrap(err, constants.S3_BUCKET_ACCESS_ERROR_MSG).Error(), constants.S3_BUCKET_ACCESS_RESOLUTION_MSG, false)
	}
	return ss.Response(constants.S3_BUCKET_ACCESS_TITLE, constants.S3_BUCKET_ACCESS_SUCCESS_MSG, "", "", true)
}

func (ss *S3ConfigService) AwsConnection(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
	sess, err := ss.AwsUtils.NewSessionWithOptions(endpoint, accessKey, secretKey, region)
	if err != nil {
		ss.Logger.Error("s3 config aws connection failed: ", err.Error())
		return nil, err
	}
	ss.Logger.Debug("s3 config aws connection success")
	return sess, nil
}

func (ss *S3ConfigService) ListBuckets(s3Client *s3.S3) error {
	// list buckets in s3 to verify secrete and access key
	_, err := ss.AwsUtils.ListBuckets(s3Client)
	if err != nil {
		ss.Logger.Error("s3 config list bucket failed: ", err.Error())
		return err
	}
	ss.Logger.Debug("s3 config list object success")
	return nil
}

func (ss *S3ConfigService) DeleteObjects(s3Client *s3.S3) error {
	_, err := ss.AwsUtils.DeleteObject(s3Client, ss.Req.BucketName, ss.Req.BasePath)
	if err != nil {
		ss.Logger.Error("s3 config delete object failed: ", err.Error())
		return err
	}
	ss.Logger.Debug("s3 config delete object success")
	return nil
}

func (ss *S3ConfigService) ListObjects(s3Client *s3.S3) error {
	_, err := ss.AwsUtils.ListObjectsV2(s3Client, ss.Req.BucketName, ss.Req.BasePath)
	if err != nil {
		ss.Logger.Error("s3 config list object failed: ", err.Error())
		return err
	}
	ss.Logger.Debug("s3 config list object success")
	return nil
}

func (ss *S3ConfigService) UploadObject(sess *session.Session) error {
	_, err := ss.AwsUtils.NewUploader(sess, ss.Req.BucketName, ss.Req.BasePath)
	if err != nil {
		ss.Logger.Error("s3 config upload object failed: ", err.Error())
		return err
	}
	ss.Logger.Debug("s3 config upload object success")
	return nil
}

func (ss *S3ConfigService) Response(Title, SuccessMsg, ErrorMsg, ResolutionMsg string, Passed bool) *models.Checks {
	return &models.Checks{
		Title:         Title,
		Passed:        Passed,
		SuccessMsg:    SuccessMsg,
		ErrorMsg:      ErrorMsg,
		ResolutionMsg: ResolutionMsg,
	}
}
