package s3configservice_test

import (
	"testing"

	"github.com/aws/aws-sdk-go/service/s3"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/s3configservice"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

var (
	s3ConnectionTitle           = "S3 connection test"
	s3ConnectionErrorMsg        = "Machine is not able to connect with S3 using the provided access key and secret key: RequestError: send request failed\ncaused by: Get \"s3://example-s3.aws.region.com/\": unsupported protocol scheme \"s3\""
	s3ConnectionResolutionMsg   = "Provide the correct S3 url or access or secret keys"
	s3BucketAccessTitle         = "S3 bucket access test"
	s3BucketAccessErrorMsg      = "Machine is not able to access the S3 bucket using the provided access key and secret key: RequestError: send request failed\ncaused by: Put \"s3://example-s3.aws.region.com/backups/automate\": unsupported protocol scheme \"s3\""
	s3BucketAccessResolutionMsg = "Provide the necessary acess to the S3 bucket"
)

func TestGetS3Connection(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log)
	services := cs.GetS3Connection(models.S3ConfigRequest{
		Endpoint:   "s3://example-s3.aws.region.com",
		BucketName: "backups",
		BasePath:   "automate",
		AccessKey:  "VALID-ACCESS-KEY",
		SecretKey:  "SECRET-KEY",
	})
	assert.Equal(t, models.ServiceCheck{
		Title:         s3ConnectionTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      s3ConnectionErrorMsg,
		ResolutionMsg: s3ConnectionResolutionMsg,
	}, services)
}

func TestGetBucketAccess(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log)
	services := cs.GetBucketAccess(models.S3ConfigRequest{
		Endpoint:   "s3://example-s3.aws.region.com",
		BucketName: "backups",
		BasePath:   "automate",
		AccessKey:  "VALID-ACCESS-KEY",
		SecretKey:  "SECRET-KEY",
	})
	assert.Equal(t, models.ServiceCheck{
		Title:         s3BucketAccessTitle,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      s3BucketAccessErrorMsg,
		ResolutionMsg: s3BucketAccessResolutionMsg,
	}, services)
}

func TestAwsConnection(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log)
	_, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
}

func TestListBuckets(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log)
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
	s3Client := s3.New(sess)
	err = cs.(*s3configservice.S3ConfigService).ListBuckets(s3Client)
	assert.Error(t, err)
}

func TestUploadObject(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log)
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
	// s3Client := s3.New(sess)
	err = cs.(*s3configservice.S3ConfigService).UploadObject(sess)
	assert.Error(t, err)
}

func TestListObjects(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log)
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
	s3Client := s3.New(sess)
	err = cs.(*s3configservice.S3ConfigService).ListObjects(s3Client)
	assert.Error(t, err)
}

func TestDeleteObjects(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := s3configservice.NewS3ConfigService(log)
	sess, err := cs.(*s3configservice.S3ConfigService).AwsConnection()
	assert.NoError(t, err)
	s3Client := s3.New(sess)
	err = cs.(*s3configservice.S3ConfigService).DeleteObjects(s3Client)
	assert.Error(t, err)
}
