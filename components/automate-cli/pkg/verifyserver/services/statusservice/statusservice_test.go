package statusservice_test

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/statusservice"
	"github.com/stretchr/testify/assert"
)

func TestStatusService(t *testing.T) {
	ss := statusservice.NewStatusService()
	services := ss.GetServices()
	assert.Equal(t, []models.ServiceDetails{}, services)
}
