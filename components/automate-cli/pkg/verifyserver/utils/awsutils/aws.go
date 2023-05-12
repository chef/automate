package awsutils

import (
	"strings"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/credentials"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3"
	"github.com/aws/aws-sdk-go/service/s3/s3manager"
)

type AwsUtilsImpl struct {
}

type AwsUtils interface {
	NewSessionWithOptions(endpoint, accessKey, secretKey string) (*session.Session, error)
	ListBuckets(s3Client *s3.S3) (*s3.ListBucketsOutput, error)
	DeleteObject(s3Client *s3.S3, BucketName, BasePath string) (*s3.DeleteObjectOutput, error)
	ListObjectsV2(s3Client *s3.S3, BucketName, BasePath string) (*s3.ListObjectsV2Output, error)
	NewUploader(sess *session.Session, BucketName, BasePath string) (*s3manager.UploadOutput, error)
	New(sess *session.Session) *s3.S3
}

func NewAwsUtils() AwsUtils {
	return &AwsUtilsImpl{}
}

func (au *AwsUtilsImpl) NewSessionWithOptions(endpoint, accessKey, secretKey string) (*session.Session, error) {
	config := aws.Config{
		Endpoint:         &endpoint,
		Region:           aws.String("us-east-1"),
		DisableSSL:       aws.Bool(true),
		S3ForcePathStyle: aws.Bool(true),
		Credentials:      credentials.NewStaticCredentials(accessKey, secretKey, ""),
	}
	return session.NewSessionWithOptions(session.Options{
		Config: config,
	})
}

func (au *AwsUtilsImpl) ListBuckets(s3Client *s3.S3) (*s3.ListBucketsOutput, error) {
	return s3Client.ListBuckets(nil)
}

func (au *AwsUtilsImpl) DeleteObject(s3Client *s3.S3, BucketName, BasePath string) (*s3.DeleteObjectOutput, error) {
	return s3Client.DeleteObject(
		&s3.DeleteObjectInput{
			Bucket: aws.String(BucketName),
			Key:    aws.String(BasePath),
		},
	)
}

func (au *AwsUtilsImpl) ListObjectsV2(s3Client *s3.S3, BucketName, BasePath string) (*s3.ListObjectsV2Output, error) {
	return s3Client.ListObjectsV2(&s3.ListObjectsV2Input{
		Bucket: aws.String(BucketName),
		Prefix: aws.String(BasePath),
	})
}

func (au *AwsUtilsImpl) NewUploader(sess *session.Session, BucketName, BasePath string) (*s3manager.UploadOutput, error) {
	return s3manager.NewUploader(sess).Upload(&s3manager.UploadInput{
		Bucket: aws.String(BucketName),          // Bucket to be used
		Key:    aws.String(BasePath),            // Name of the file to be saved
		Body:   strings.NewReader("my request"), // File
	})
}

func (au *AwsUtilsImpl) New(sess *session.Session) *s3.S3 {
	return s3.New(sess)
}
