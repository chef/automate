package s3configservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockS3Config struct {
	GetS3ConnectionFunc func(models.S3ConfigRequest) models.ServiceCheck
	GetBucketAccessFunc func(models.S3ConfigRequest) models.ServiceCheck
}

func (msc *MockS3Config) GetS3Connection(req models.S3ConfigRequest) models.ServiceCheck {
	return msc.GetS3ConnectionFunc(req)
}

func (msc *MockS3Config) GetBucketAccess(req models.S3ConfigRequest) models.ServiceCheck {
	return msc.GetBucketAccessFunc(req)
}
