package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type OpensearchS3BucketAccessCheck struct{}

func NewOpensearchS3BucketAccessCheck() *OpensearchS3BucketAccessCheck {
	return &OpensearchS3BucketAccessCheck{}
}

func (osb *OpensearchS3BucketAccessCheck) Run(config models.Config) []models.CheckTriggerResponse {
	m := []models.CheckTriggerResponse{
		models.CheckTriggerResponse{},
	}
	return m
}
