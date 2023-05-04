package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type OpensearchS3BucketAccessCheck struct{}

// func (hrc *CheckTrigger) OpensearchS3BucketAccessCheck(config models.Config) map[string]models.CheckTriggerResponse {
// 	return models.CheckTriggerResponse{}
// }

func (ss *OpensearchS3BucketAccessCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
        "f": models.CheckTriggerResponse{},
    }
	return m
}