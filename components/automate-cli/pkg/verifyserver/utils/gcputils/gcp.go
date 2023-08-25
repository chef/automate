package gcputils

import (
	"strings"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/credentials"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3"
	"github.com/aws/aws-sdk-go/service/s3/s3manager"
)

type GCPUtilsImpl struct {
}

type GCPUtils interface {
	NewSessionWithOptions(endpoint, accessKey, secretKey, region string) (*session.Session, error)
	DeleteObject(s3Client *s3.S3, BucketName, BasePath string) (*s3.DeleteObjectOutput, error)
	ListObjectsV2(s3Client *s3.S3, BucketName, BasePath string) (*s3.ListObjectsV2Output, error)
	ListBuckets(s3Client *s3.S3) (*s3.ListBucketsOutput, error)
	NewUploader(sess *session.Session, BucketName, BasePath string) (*s3manager.UploadOutput, error)
	New(sess *session.Session) *s3.S3
}

func NewAwsUtils() GCPUtils {
	return &GCPUtilsImpl{}
}

func (au *GCPUtilsImpl) NewSessionWithOptions(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
	var config aws.Config
	if region == "" {
		region = "us-east-1"
		config.S3ForcePathStyle = aws.Bool(true)
		config.DisableSSL = aws.Bool(true)
	}
	config.Endpoint = &endpoint
	config.Region = aws.String(region)
	if accessKey != "" && secretKey != "" {
		config.Credentials = credentials.NewStaticCredentials(accessKey, secretKey, "")
	}
	return session.NewSessionWithOptions(session.Options{
		Config: config,
	})
}

func (au *GCPUtilsImpl) DeleteObject(s3Client *s3.S3, BucketName, BasePath string) (*s3.DeleteObjectOutput, error) {
	return s3Client.DeleteObject(
		&s3.DeleteObjectInput{
			Bucket: aws.String(BucketName),
			Key:    aws.String(BasePath),
		},
	)
}

func (au *GCPUtilsImpl) ListObjectsV2(s3Client *s3.S3, BucketName, BasePath string) (*s3.ListObjectsV2Output, error) {
	return s3Client.ListObjectsV2(&s3.ListObjectsV2Input{
		Bucket: aws.String(BucketName),
		Prefix: aws.String(BasePath),
	})
}

func (au *GCPUtilsImpl) NewUploader(sess *session.Session, BucketName, BasePath string) (*s3manager.UploadOutput, error) {
	return s3manager.NewUploader(sess).Upload(&s3manager.UploadInput{
		Bucket: aws.String(BucketName),          // Bucket to be used
		Key:    aws.String(BasePath),            // Name of the file to be saved
		Body:   strings.NewReader("my request"), // File
	})
}

func (au *GCPUtilsImpl) ListBuckets(s3Client *s3.S3) (*s3.ListBucketsOutput, error) {
	return s3Client.ListBuckets(nil)
}

func (au *GCPUtilsImpl) New(sess *session.Session) *s3.S3 {
	return s3.New(sess)
}
