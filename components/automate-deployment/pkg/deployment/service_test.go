package deployment

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

func TestNewServiceFromHabPackage(t *testing.T) {
	sp, _ := habpkg.FromString("chef/ingest-service")
	svc := NewServiceFromHabPackage(sp)
	assert.Equal(t, "ingest-service", svc.Name())
	assert.Equal(t, "chef", svc.Origin())
}
