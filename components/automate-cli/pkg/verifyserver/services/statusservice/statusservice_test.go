package statusservice

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/stretchr/testify/assert"
)

func TestStatusService(t *testing.T) {
	ss := NewStatusService()
	services := ss.GetServices()
	assert.Equal(t, []models.ServiceDetails{}, services)
}
