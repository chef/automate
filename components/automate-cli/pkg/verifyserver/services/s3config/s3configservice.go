package s3configservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type S3Config interface {
	GetS3Connection() models.ServiceCheck
	GetBucketAccess() models.ServiceCheck
}

type S3ConfigService struct {
}

func NewS3ConfigService() S3Config {
	return &S3ConfigService{}
}

func (ss *S3ConfigService) GetS3Connection() models.ServiceCheck {
	return models.ServiceCheck{}
}

func (ss *S3ConfigService) GetBucketAccess() models.ServiceCheck {
	return models.ServiceCheck{}
}
