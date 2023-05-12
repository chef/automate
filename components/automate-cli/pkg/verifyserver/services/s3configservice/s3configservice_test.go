package s3configservice_test

import (
	"testing"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/client"
	"github.com/aws/aws-sdk-go/aws/client/metadata"
	"github.com/aws/aws-sdk-go/aws/request"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3"
	"github.com/aws/aws-sdk-go/service/s3/s3manager"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/s3configservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/awsutils"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

var (
	s3ConnectionTitle           = "S3 connection test"
	s3ConnectionErrorMsg        = "Machine is not able to connect with S3 using the provided access key and secret key: RequestError: send request failed\ncaused by: Get \"s3://example-s3.aws.region.com/\": unsupported protocol scheme \"s3\""
	s3ConnectionResolutionMsg   = "Provide the correct S3 url or access or secret keys"
	s3BucketAccessTitle         = "S3 bucket access test"
	s3BucketAccessErrorMsg      = "Machine is not able to access the S3 bucket using the provided access key and secret key: RequestError: send request failed\ncaused by: Put \"s3://example-s3.aws.region.com/backups/automate\": unsupported protocol scheme \"s3\""
	s3BucketAccessResolutionMsg = "Provide the necessary access to the S3 bucket"
)

func TestGetS3Connection(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, awsutils.NewAwsUtils())
	services := cs.GetS3Connection(models.S3ConfigRequest{
		Endpoint:   "s3://example-s3.aws.region.com",
		BucketName: "backups",
		BasePath:   "automate",
		AccessKey:  "VALID-ACCESS-KEY",
		SecretKey:  "SECRET-KEY",
	})
	assert.Equal(t, models.S3ServiceCheck{
		Title:         s3ConnectionTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      s3ConnectionErrorMsg,
		ResolutionMsg: s3ConnectionResolutionMsg,
	}, services)
}

func TestGetBucketAccessFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, awsutils.NewAwsUtils())
	services := cs.GetBucketAccess(models.S3ConfigRequest{
		Endpoint:   "s3://example-s3.aws.region.com",
		BucketName: "backups",
		BasePath:   "automate",
		AccessKey:  "VALID-ACCESS-KEY",
		SecretKey:  "SECRET-KEY",
	})
	assert.Equal(t, models.S3ServiceCheck{
		Title:         s3BucketAccessTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      s3BucketAccessErrorMsg,
		ResolutionMsg: s3BucketAccessResolutionMsg,
	}, services)
}

func TestAwsConnection(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, awsutils.NewAwsUtils())
	_, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
}

func TestListBucketsFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, awsutils.NewAwsUtils())
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
	s3Client := s3.New(sess)
	err = cs.(*s3configservice.S3ConfigService).ListBuckets(s3Client)
	assert.Error(t, err)
}

func TestUploadObjectFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, awsutils.NewAwsUtils())
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
	// s3Client := s3.New(sess)
	err = cs.(*s3configservice.S3ConfigService).UploadObject(sess)
	assert.Error(t, err)
}

func TestListObjectsFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, awsutils.NewAwsUtils())
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
	s3Client := s3.New(sess)
	err = cs.(*s3configservice.S3ConfigService).ListObjects(s3Client)
	assert.Error(t, err)
}

func TestDeleteObjectsFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, awsutils.NewAwsUtils())
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
	s3Client := s3.New(sess)
	err = cs.(*s3configservice.S3ConfigService).DeleteObjects(s3Client)
	assert.Error(t, err)
}

func TestListBucketsSuccess(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey string) (*session.Session, error) {
			return &session.Session{}, nil
		},
		ListBucketsFunc: func(s3Client *s3.S3) (*s3.ListBucketsOutput, error) {
			return &s3.ListBucketsOutput{
				Buckets: []*s3.Bucket{},
			}, nil
		},
		NewFunc: func(sess *session.Session) *s3.S3 {
			return &s3.S3{
				Client: client.New(
					aws.Config{},
					metadata.ClientInfo{},
					request.Handlers{},
				),
			}
		},
	})
	_, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
	err = cs.(*s3configservice.S3ConfigService).ListBuckets(&s3.S3{
		Client: client.New(
			aws.Config{},
			metadata.ClientInfo{},
			request.Handlers{},
		),
	})
	assert.NoError(t, err)
}

func TestUploadObjectSuccess(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey string) (*session.Session, error) {
			return &session.Session{}, nil
		},
		NewUploaderFunc: func(sess *session.Session, BucketName, BasePath string) (*s3manager.UploadOutput, error) {
			return &s3manager.UploadOutput{}, nil
		},
		NewFunc: func(sess *session.Session) *s3.S3 {
			return &s3.S3{
				Client: client.New(
					aws.Config{},
					metadata.ClientInfo{},
					request.Handlers{},
				),
			}
		},
	})
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
	err = cs.(*s3configservice.S3ConfigService).UploadObject(sess)
	assert.NoError(t, err)
}

func TestListObjectsSuccess(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey string) (*session.Session, error) {
			return &session.Session{}, nil
		},
		ListObjectsV2Func: func(s3Client *s3.S3, BucketName, BasePath string) (*s3.ListObjectsV2Output, error) {
			return &s3.ListObjectsV2Output{}, nil
		},
		NewFunc: func(sess *session.Session) *s3.S3 {
			return &s3.S3{
				Client: client.New(
					aws.Config{},
					metadata.ClientInfo{},
					request.Handlers{},
				),
			}
		},
	})
	_, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
	err = cs.(*s3configservice.S3ConfigService).ListObjects(&s3.S3{
		Client: client.New(
			aws.Config{},
			metadata.ClientInfo{},
			request.Handlers{},
		),
	})
	assert.NoError(t, err)
}

func TestDeleteObjectsSuccess(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey string) (*session.Session, error) {
			return &session.Session{}, nil
		},
		DeleteObjectFunc: func(s3Client *s3.S3, BucketName, BasePath string) (*s3.DeleteObjectOutput, error) {
			return &s3.DeleteObjectOutput{}, nil
		},
		NewFunc: func(sess *session.Session) *s3.S3 {
			return &s3.S3{
				Client: client.New(
					aws.Config{},
					metadata.ClientInfo{},
					request.Handlers{},
				),
			}
		},
	})
	_, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
	err = cs.(*s3configservice.S3ConfigService).DeleteObjects(&s3.S3{
		Client: client.New(
			aws.Config{},
			metadata.ClientInfo{},
			request.Handlers{},
		),
	})
	assert.NoError(t, err)
}
