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
		return models.ServiceCheck{
			Title:         "S3 connection test",
			Passed:        false,
			SuccessMsg:    "",
			ErrorMsg:      "Machine is not able to connect with S3 using the provided access key and secret key",
			ResolutionMsg: "Provide the correct S3 url or access or secret keys",
		}
	}

	return models.ServiceCheck{
		Title:         "S3 connection test",
		Passed:        true,
		SuccessMsg:    "Machine is able to connect with S3 using the provided access key and secret key",
		ErrorMsg:      "",
		ResolutionMsg: "",
	}
}

func (ss *S3ConfigService) GetBucketAccess(req models.S3ConfigRequest) models.ServiceCheck {
	ss.req = req
	sess, err := ss.AwsConnection()
	returnError := models.ServiceCheck{
		Title:         "S3 connection test",
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      "Machine is not able to access the S3 bucket using the provided access key and secret key",
		ResolutionMsg: "Provide the necessary acess to the S3 bucket",
	}
	if err != nil {
		return returnError
	}
	uploader := s3manager.NewUploader(sess)
	_, err = uploader.Upload(&s3manager.UploadInput{
		Bucket: aws.String(ss.req.BucketName),   // Bucket to be used
		Key:    aws.String(ss.req.BasePath),     // Name of the file to be saved
		Body:   strings.NewReader("my request"), // File
	})
	if err != nil {
		return returnError
	}
	return models.ServiceCheck{
		Title:         "S3 bucket access test",
		Passed:        true,
		SuccessMsg:    "Machine is able to access the S3 bucket using the provided access key and secret key",
		ErrorMsg:      "",
		ResolutionMsg: "",
	}
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

	_, err = s3Client.ListBuckets(nil)
	if err != nil {
		return nil, err
	}
	return sess, nil
}
