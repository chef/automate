package s3configservice_test

import (
	"testing"

	"github.com/bmizerany/assert"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/s3configservice"
)

func TestS3ConfigService(t *testing.T) {
	cs := s3configservice.NewS3ConfigService()
	services := cs.GetS3Connection(models.S3ConfigRequest{})
	assert.Equal(t, models.ServiceCheck{}, services)
}
