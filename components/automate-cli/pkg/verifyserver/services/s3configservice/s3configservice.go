package s3configservice

import (
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/awsutils"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
)

var (
	s3ConnectionTitle           = "S3 connection test"
	s3ConnectionErrorMsg        = "Machine is not able to connect with S3 using the provided access key and secret key"
	s3ConnectionResolutionMsg   = "Provide the correct S3 url or access or secret keys"
	s3ConnectionSuccessMsg      = "Machine is able to connect with S3 using the provided access key and secret key"
	s3BucketAccessTitle         = "S3 bucket access test"
	s3BucketAccessErrorMsg      = "Machine is not able to access the S3 bucket using the provided access key and secret key"
	s3BucketAccessResolutionMsg = "Provide the necessary access to the S3 bucket"
	s3BucketAccessSuccessMsg    = "Machine is able to access the S3 bucket using the provided access key and secret key"
)

type S3Config interface {
	GetS3Connection(models.S3ConfigRequest) models.S3ServiceCheck
	GetBucketAccess(models.S3ConfigRequest) models.S3ServiceCheck
}

type S3ConfigService struct {
	logger   logger.Logger
	req      models.S3ConfigRequest
	awsUtils awsutils.AwsUtils
}

func NewS3ConfigService(logger logger.Logger, awsUtils awsutils.AwsUtils) S3Config {
	return &S3ConfigService{
		logger:   logger,
		awsUtils: awsUtils,
	}
}

func (ss *S3ConfigService) GetS3Connection(req models.S3ConfigRequest) models.S3ServiceCheck {
	ss.req = req
	sess, err := ss.AwsConnection()
	if err != nil {
		return ss.Response(s3ConnectionTitle, "", errors.Wrap(err, s3ConnectionErrorMsg).Error(), s3ConnectionResolutionMsg, false)
	}
	s3Client := ss.awsUtils.New(sess)
	err = ss.ListBuckets(s3Client)
	if err != nil {
		return ss.Response(s3ConnectionTitle, "", errors.Wrap(err, s3ConnectionErrorMsg).Error(), s3ConnectionResolutionMsg, false)
	}
	return ss.Response(s3ConnectionTitle, s3ConnectionSuccessMsg, "", "", true)
}

func (ss *S3ConfigService) GetBucketAccess(req models.S3ConfigRequest) models.S3ServiceCheck {
	ss.req = req

	// S3 connection
	sess, err := ss.AwsConnection()
	s3Client := ss.awsUtils.New(sess)
	if err != nil {
		return ss.Response(s3BucketAccessTitle, "", errors.Wrap(err, s3BucketAccessErrorMsg).Error(), s3BucketAccessResolutionMsg, false)
	}

	// upload data in s3 bucket
	err = ss.UploadObject(sess)
	if err != nil {
		return ss.Response(s3BucketAccessTitle, "", errors.Wrap(err, s3BucketAccessErrorMsg).Error(), s3BucketAccessResolutionMsg, false)
	}

	// read/list data in s3 bucket
	err = ss.ListObjects(s3Client)
	if err != nil {
		return ss.Response(s3BucketAccessTitle, "", errors.Wrap(err, s3BucketAccessErrorMsg).Error(), s3BucketAccessResolutionMsg, false)
	}

	// delete data in s3 bucket
	err = ss.DeleteObjects(s3Client)
	if err != nil {
		return ss.Response(s3BucketAccessTitle, "", errors.Wrap(err, s3BucketAccessErrorMsg).Error(), s3BucketAccessResolutionMsg, false)
	}
	return ss.Response(s3BucketAccessTitle, s3BucketAccessSuccessMsg, "", "", true)
}

func (ss *S3ConfigService) AwsConnection() (*session.Session, error) {
	sess, err := ss.awsUtils.NewSessionWithOptions(ss.req.Endpoint, ss.req.AccessKey, ss.req.SecretKey)
	if err != nil {
		ss.logger.Error("s3 config aws connection failed: ", err.Error())
		return nil, err
	}
	ss.logger.Info("s3 config aws connection success")
	return sess, nil
}

func (ss *S3ConfigService) ListBuckets(s3Client *s3.S3) error {
	// list buckets in s3 to verify secrete and access key
	_, err := ss.awsUtils.ListBuckets(s3Client)
	if err != nil {
		ss.logger.Error("s3 config list bucket failed: ", err.Error())
		return err
	}
	ss.logger.Info("s3 config list object success")
	return nil
}

func (ss *S3ConfigService) DeleteObjects(s3Client *s3.S3) error {
	_, err := ss.awsUtils.DeleteObject(s3Client, ss.req.BucketName, ss.req.BasePath)
	if err != nil {
		ss.logger.Error("s3 config delete object failed: ", err.Error())
		return err
	}
	ss.logger.Info("s3 config delete object success")
	return nil
}

func (ss *S3ConfigService) ListObjects(s3Client *s3.S3) error {
	_, err := ss.awsUtils.ListObjectsV2(s3Client, ss.req.BucketName, ss.req.BasePath)
	if err != nil {
		ss.logger.Error("s3 config list object failed: ", err.Error())
		return err
	}
	ss.logger.Info("s3 config list object success")
	return nil
}

func (ss *S3ConfigService) UploadObject(sess *session.Session) error {
	_, err := ss.awsUtils.NewUploader(sess, ss.req.BucketName, ss.req.BasePath)
	if err != nil {
		ss.logger.Error("s3 config upload object failed: ", err.Error())
		return err
	}
	ss.logger.Info("s3 config upload object success")
	return nil
}

func (ss *S3ConfigService) Response(Title, SuccessMsg, ErrorMsg, ResolutionMsg string, Passed bool) models.S3ServiceCheck {
	return models.S3ServiceCheck{
		Title:         Title,
		Passed:        Passed,
		SuccessMsg:    SuccessMsg,
		ErrorMsg:      ErrorMsg,
		ResolutionMsg: ResolutionMsg,
	}
}
