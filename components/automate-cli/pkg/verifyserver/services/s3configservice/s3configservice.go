package s3configservice

import (
	"strings"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/credentials"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3"
	"github.com/aws/aws-sdk-go/service/s3/s3manager"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

var (
	s3ConnectionTitle           = "S3 connection test"
	s3ConnectionErrorMsg        = "Machine is not able to connect with S3 using the provided access key and secret key"
	s3ConnectionResolutionMsg   = "Provide the correct S3 url or access or secret keys"
	s3ConnectionSuccessMsg      = "Machine is able to connect with S3 using the provided access key and secret key"
	s3BucketAccessTitle         = "S3 bucket access test"
	s3BucketAccessErrorMsg      = "Machine is not able to access the S3 bucket using the provided access key and secret key"
	s3BucketAccessResolutionMsg = "Provide the necessary acess to the S3 bucket"
	s3BucketAccessSuccessMsg    = "Machine is able to access the S3 bucket using the provided access key and secret key"
)

type S3Config interface {
	GetS3Connection(models.S3ConfigRequest) models.ServiceCheck
	GetBucketAccess(models.S3ConfigRequest) models.ServiceCheck
}

type S3ConfigService struct {
	req models.S3ConfigRequest
}

func NewS3ConfigService() S3Config {
	return &S3ConfigService{}
}

func (ss *S3ConfigService) GetS3Connection(req models.S3ConfigRequest) models.ServiceCheck {
	ss.req = req
	_, err := ss.AwsConnection()
	if err != nil {
		return ss.Response(s3ConnectionTitle, "", s3ConnectionErrorMsg, s3ConnectionResolutionMsg, false)
	}
	return ss.Response(s3ConnectionTitle, s3ConnectionSuccessMsg, "", "", true)
}

func (ss *S3ConfigService) GetBucketAccess(req models.S3ConfigRequest) models.ServiceCheck {
	ss.req = req

	// S3 connection
	sess, err := ss.AwsConnection()
	if err != nil {
		return ss.Response(s3BucketAccessTitle, "", s3BucketAccessErrorMsg, s3BucketAccessResolutionMsg, false)
	}

	// upload data in s3 bucket
	uploader := s3manager.NewUploader(sess)
	_, err = uploader.Upload(&s3manager.UploadInput{
		Bucket: aws.String(ss.req.BucketName),   // Bucket to be used
		Key:    aws.String(ss.req.BasePath),     // Name of the file to be saved
		Body:   strings.NewReader("my request"), // File
	})
	if err != nil {
		return ss.Response(s3BucketAccessTitle, "", s3BucketAccessErrorMsg, s3BucketAccessResolutionMsg, false)
	}

	s3Client := s3.New(sess)

	// read/list data in s3 bucket
	_, err = s3Client.ListObjectsV2(&s3.ListObjectsV2Input{
		Bucket: aws.String(ss.req.BucketName),
		Prefix: aws.String(ss.req.BasePath),
	})
	if err != nil {
		return ss.Response(s3BucketAccessTitle, "", s3BucketAccessErrorMsg, s3BucketAccessResolutionMsg, false)
	}

	// delete data in s3 bucket
	_, err = s3Client.DeleteObject(
		&s3.DeleteObjectInput{
			Bucket: aws.String(ss.req.BucketName),
			Key:    aws.String(ss.req.BasePath),
		})
	if err != nil {
		return ss.Response(s3BucketAccessTitle, "", s3BucketAccessErrorMsg, s3BucketAccessResolutionMsg, false)
	}
	return ss.Response(s3BucketAccessTitle, s3BucketAccessSuccessMsg, "", "", true)
}

func (ss *S3ConfigService) AwsConnection() (*session.Session, error) {
	config := aws.Config{
		Region:      aws.String("us-east-1"),
		Credentials: credentials.NewStaticCredentials(ss.req.AccessKey, ss.req.SecretKey, ""),
	}
	sess, err := session.NewSessionWithOptions(session.Options{
		Config: config,
	})
	if err != nil {
		return nil, err
	}

	s3Client := s3.New(sess)

	// list buckets in s3 to verify secrete and access key
	_, err = s3Client.ListBuckets(nil)
	if err != nil {
		return nil, err
	}
	return sess, nil
}

func (ss *S3ConfigService) Response(Title, SuccessMsg, ErrorMsg, ResolutionMsg string, Passed bool) models.ServiceCheck {
	return models.ServiceCheck{
		Title:         Title,
		Passed:        Passed,
		SuccessMsg:    SuccessMsg,
		ErrorMsg:      ErrorMsg,
		ResolutionMsg: ResolutionMsg,
	}
}
