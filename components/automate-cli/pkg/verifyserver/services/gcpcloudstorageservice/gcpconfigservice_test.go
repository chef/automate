package gcpcloudstorageservice

import (
	"testing"

	"errors"

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
	s3ConnectionTitle = "S3 connection test"
	// s3ConnectionErrorMsg        = "Machine is not able to connect with S3 using the provided access key and secret key: RequestError: send request failed\ncaused by: Get \"s3://example-s3.aws.region.com/\": unsupported protocol scheme \"s3\""
	s3ConnectionErrorMsg        = "Machine is not able to connect with S3 using the provided access key and secret key: "
	s3ConnectionResolutionMsg   = "Provide the correct S3 url or access or secret keys"
	s3ConnectionSuccessMsg      = "Machine is able to connect with S3 using the provided access key and secret key"
	s3BucketAccessTitle         = "S3 bucket access test"
	s3BucketAccessErrorMsg      = "Machine is not able to access the S3 bucket using the provided access key and secret key: "
	s3BucketAccessResolutionMsg = "Please check if the provided S3 bucket exists or not. If it exists then provide the bucket access to the snapshot user."
	s3BucketAccessSuccessMsg    = "Machine is able to access the S3 bucket using the provided access key and secret key"
	req                         = models.S3ConfigRequest{
		Endpoint:   "s3://example-s3.aws.region.com",
		BucketName: "backups",
		BasePath:   "automate",
		AccessKey:  "VALID-ACCESS-KEY",
		SecretKey:  "SECRET-KEY",
	}
)

func TestGetS3Connection(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
			return &session.Session{}, errors.New("")
		},
	})
	services := cs.GetS3Connection(&models.S3ConfigRequest{
		Endpoint:   "s3://example-s3.aws.region.com",
		BucketName: "backups",
		BasePath:   "automate",
		AccessKey:  "VALID-ACCESS-KEY",
		SecretKey:  "SECRET-KEY",
		Region:     "ap-south-1",
	})
	assert.Equal(t, &models.Checks{
		Title:         s3ConnectionTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      s3ConnectionErrorMsg,
		ResolutionMsg: s3ConnectionResolutionMsg,
	}, services)
}

func TestGetS3ConnectionSuccess(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
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
	services := cs.GetS3Connection(&models.S3ConfigRequest{
		Endpoint:   "s3://example-s3.aws.region.com",
		BucketName: "backups",
		BasePath:   "automate",
		AccessKey:  "VALID-ACCESS-KEY",
		SecretKey:  "SECRET-KEY",
		Region:     "ap-south-1",
	})
	assert.Equal(t, &models.Checks{
		Title:         s3ConnectionTitle,
		Passed:        true,
		SuccessMsg:    s3ConnectionSuccessMsg,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}, services)
}

func TestGetBucketAccessAwsConnection(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
			return &session.Session{}, errors.New("")
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
	services := cs.GetBucketAccess(&models.S3ConfigRequest{
		Endpoint:   "s3://example-s3.aws.region.com",
		BucketName: "backups",
		BasePath:   "automate",
		AccessKey:  "VALID-ACCESS-KEY",
		SecretKey:  "SECRET-KEY",
		Region:     "ap-south-1",
	})
	assert.Equal(t, &models.Checks{
		Title:         s3BucketAccessTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      s3BucketAccessErrorMsg,
		ResolutionMsg: s3BucketAccessResolutionMsg,
	}, services)
}

func TestGetS3ConnectionListBucketsFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
			return &session.Session{}, nil
		},
		ListBucketsFunc: func(s3Client *s3.S3) (*s3.ListBucketsOutput, error) {
			return &s3.ListBucketsOutput{
				Buckets: []*s3.Bucket{},
			}, errors.New("")
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
	services := cs.GetS3Connection(&models.S3ConfigRequest{
		Endpoint:   "s3://example-s3.aws.region.com",
		BucketName: "backups",
		BasePath:   "automate",
		AccessKey:  "VALID-ACCESS-KEY",
		SecretKey:  "SECRET-KEY",
		Region:     "ap-south-1",
	})
	assert.Equal(t, &models.Checks{
		Title:         s3ConnectionTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      s3ConnectionErrorMsg,
		ResolutionMsg: s3ConnectionResolutionMsg,
	}, services)
}

func TestGetBucketAccessUploadObjectFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
			return &session.Session{}, nil
		},
		NewUploaderFunc: func(sess *session.Session, BucketName, BasePath string) (*s3manager.UploadOutput, error) {
			return &s3manager.UploadOutput{}, errors.New("")
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
	services := cs.GetBucketAccess(&models.S3ConfigRequest{
		Endpoint:   "s3://example-s3.aws.region.com",
		BucketName: "backups",
		BasePath:   "automate",
		AccessKey:  "VALID-ACCESS-KEY",
		SecretKey:  "SECRET-KEY",
		Region:     "ap-south-1",
	})
	assert.Equal(t, &models.Checks{
		Title:         s3BucketAccessTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      s3BucketAccessErrorMsg,
		ResolutionMsg: s3BucketAccessResolutionMsg,
	}, services)
}

func TestGetBucketAccessListObjectsFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
			return &session.Session{}, nil
		},
		NewUploaderFunc: func(sess *session.Session, BucketName, BasePath string) (*s3manager.UploadOutput, error) {
			return &s3manager.UploadOutput{}, nil
		},
		ListObjectsV2Func: func(s3Client *s3.S3, BucketName, BasePath string) (*s3.ListObjectsV2Output, error) {
			return &s3.ListObjectsV2Output{}, errors.New("")
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
	services := cs.GetBucketAccess(&models.S3ConfigRequest{
		Endpoint:   "s3://example-s3.aws.region.com",
		BucketName: "backups",
		BasePath:   "automate",
		AccessKey:  "VALID-ACCESS-KEY",
		SecretKey:  "SECRET-KEY",
		Region:     "ap-south-1",
	})
	assert.Equal(t, &models.Checks{
		Title:         s3BucketAccessTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      s3BucketAccessErrorMsg,
		ResolutionMsg: s3BucketAccessResolutionMsg,
	}, services)
}

func TestGetBucketAccessDeleteObjectFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
			return &session.Session{}, nil
		},
		NewUploaderFunc: func(sess *session.Session, BucketName, BasePath string) (*s3manager.UploadOutput, error) {
			return &s3manager.UploadOutput{}, nil
		},
		ListObjectsV2Func: func(s3Client *s3.S3, BucketName, BasePath string) (*s3.ListObjectsV2Output, error) {
			return &s3.ListObjectsV2Output{}, nil
		},
		DeleteObjectFunc: func(s3Client *s3.S3, BucketName, BasePath string) (*s3.DeleteObjectOutput, error) {
			return &s3.DeleteObjectOutput{}, errors.New("")
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
	services := cs.GetBucketAccess(&models.S3ConfigRequest{
		Endpoint:   "s3://example-s3.aws.region.com",
		BucketName: "backups",
		BasePath:   "automate",
		AccessKey:  "VALID-ACCESS-KEY",
		SecretKey:  "SECRET-KEY",
		Region:     "ap-south-1",
	})
	assert.Equal(t, &models.Checks{
		Title:         s3BucketAccessTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      s3BucketAccessErrorMsg,
		ResolutionMsg: s3BucketAccessResolutionMsg,
	}, services)
}

func TestGetBucketAccessSuccess(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
			return &session.Session{}, nil
		},
		ListObjectsV2Func: func(s3Client *s3.S3, BucketName, BasePath string) (*s3.ListObjectsV2Output, error) {
			return &s3.ListObjectsV2Output{}, nil
		},
		NewUploaderFunc: func(sess *session.Session, BucketName, BasePath string) (*s3manager.UploadOutput, error) {
			return &s3manager.UploadOutput{}, nil
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
	services := cs.GetBucketAccess(&models.S3ConfigRequest{
		Endpoint:   "s3://example-s3.aws.region.com",
		BucketName: "backups",
		BasePath:   "automate",
		AccessKey:  "VALID-ACCESS-KEY",
		SecretKey:  "SECRET-KEY",
		Region:     "ap-south-1",
	})
	assert.Equal(t, &models.Checks{
		Title:         s3BucketAccessTitle,
		Passed:        true,
		SuccessMsg:    s3BucketAccessSuccessMsg,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}, services)
}
func TestAwsConnection(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, awsutils.NewAwsUtils())
	_, err := cs.(*s3configservice.S3ConfigService).AwsConnection(req.Endpoint, req.AccessKey, req.SecretKey, "")
	assert.NoError(t, err)
}

func TestListBucketsFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, awsutils.NewAwsUtils())
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection(req.Endpoint, req.AccessKey, req.SecretKey, "")
	assert.NoError(t, err)
	s3Client := s3.New(sess)
	err = cs.(*s3configservice.S3ConfigService).ListBuckets(s3Client)
	assert.Error(t, err)
}

func TestUploadObjectFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, awsutils.NewAwsUtils())
	cs.(*s3configservice.S3ConfigService).Req = &req
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection(req.Endpoint, req.AccessKey, req.SecretKey, req.Region)
	assert.NoError(t, err)
	// s3Client := s3.New(sess)
	err = cs.(*s3configservice.S3ConfigService).UploadObject(sess)
	assert.Error(t, err)
}

func TestListObjectsFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, awsutils.NewAwsUtils())
	cs.(*s3configservice.S3ConfigService).Req = &req
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection(req.Endpoint, req.AccessKey, req.SecretKey, req.Region)
	assert.NoError(t, err)
	s3Client := s3.New(sess)
	err = cs.(*s3configservice.S3ConfigService).ListObjects(s3Client)
	assert.Error(t, err)
}

func TestDeleteObjectsFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, awsutils.NewAwsUtils())
	cs.(*s3configservice.S3ConfigService).Req = &req
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection(req.Endpoint, req.AccessKey, req.SecretKey, req.Region)
	assert.NoError(t, err)
	s3Client := s3.New(sess)
	err = cs.(*s3configservice.S3ConfigService).DeleteObjects(s3Client)
	assert.Error(t, err)
}

func TestListBucketsSuccess(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
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
	cs.(*s3configservice.S3ConfigService).Req = &req
	_, err := cs.(*s3configservice.S3ConfigService).AwsConnection(req.Endpoint, req.AccessKey, req.SecretKey, "")
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
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
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
	cs.(*s3configservice.S3ConfigService).Req = &req
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection(req.Endpoint, req.AccessKey, req.SecretKey, req.Region)
	assert.NoError(t, err)
	err = cs.(*s3configservice.S3ConfigService).UploadObject(sess)
	assert.NoError(t, err)
}

func TestListObjectsSuccess(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log, &awsutils.MockAwsUtils{
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
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
	cs.(*s3configservice.S3ConfigService).Req = &req
	_, err := cs.(*s3configservice.S3ConfigService).AwsConnection(req.Endpoint, req.AccessKey, req.SecretKey, req.Region)
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
		NewSessionWithOptionsFunc: func(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
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
	cs.(*s3configservice.S3ConfigService).Req = &req
	_, err := cs.(*s3configservice.S3ConfigService).AwsConnection(req.Endpoint, req.AccessKey, req.SecretKey, req.Region)
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
