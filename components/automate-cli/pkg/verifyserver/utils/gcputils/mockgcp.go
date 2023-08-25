package gcputils

import (
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3"
	"github.com/aws/aws-sdk-go/service/s3/s3manager"
)

type MockAwsUtils struct {
	NewSessionWithOptionsFunc func(endpoint, accessKey, secretKey, region string) (*session.Session, error)
	ListBucketsFunc           func(s3Client *s3.S3) (*s3.ListBucketsOutput, error)
	DeleteObjectFunc          func(s3Client *s3.S3, BucketName, BasePath string) (*s3.DeleteObjectOutput, error)
	ListObjectsV2Func         func(s3Client *s3.S3, BucketName, BasePath string) (*s3.ListObjectsV2Output, error)
	NewUploaderFunc           func(sess *session.Session, BucketName, BasePath string) (*s3manager.UploadOutput, error)
	NewFunc                   func(sess *session.Session) *s3.S3
}

func (mau *MockAwsUtils) NewSessionWithOptions(endpoint, accessKey, secretKey, region string) (*session.Session, error) {
	return mau.NewSessionWithOptionsFunc(endpoint, accessKey, secretKey, region)
}

func (mau *MockAwsUtils) ListBuckets(s3Client *s3.S3) (*s3.ListBucketsOutput, error) {
	return mau.ListBucketsFunc(s3Client)
}

func (mau *MockAwsUtils) DeleteObject(s3Client *s3.S3, BucketName, BasePath string) (*s3.DeleteObjectOutput, error) {
	return mau.DeleteObjectFunc(s3Client, BucketName, BasePath)
}

func (mau *MockAwsUtils) ListObjectsV2(s3Client *s3.S3, BucketName, BasePath string) (*s3.ListObjectsV2Output, error) {
	return mau.ListObjectsV2Func(s3Client, BucketName, BasePath)
}

func (mau *MockAwsUtils) NewUploader(sess *session.Session, BucketName, BasePath string) (*s3manager.UploadOutput, error) {
	return mau.NewUploaderFunc(sess, BucketName, BasePath)
}

func (mau *MockAwsUtils) New(sess *session.Session) *s3.S3 {
	return mau.NewFunc(sess)
}
