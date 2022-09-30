package migratorv4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

const ENSURE_STATUS_MSG = "[Error] Please make sure all services are healthy by running chef-automate status\n"

func TestEnsureStatus(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		GetServicesStatusFunc: func() (bool, error) { return true, nil },
	}
	ms := NewEnsureStatus(cw.CliWriter, mmu)
	err := ms.Run()
	assert.NoError(t, err)
	ms.ErrorHandler()
	assert.Equal(t, cw.Output(), "")
}

func TestEnsureStatusServicesDown(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		GetServicesStatusFunc: func() (bool, error) { return false, nil },
	}
	ms := NewEnsureStatus(cw.CliWriter, mmu)
	err := ms.Run()
	assert.Error(t, err)
	ms.ErrorHandler()
	assert.Equal(t, cw.Output(), ENSURE_STATUS_MSG)
}

func TestEnsureStatusWithError(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		GetServicesStatusFunc: func() (bool, error) { return false, errors.New("unexpected") },
	}
	ms := NewEnsureStatus(cw.CliWriter, mmu)
	err := ms.Run()
	assert.Error(t, err)
	ms.ErrorHandler()
	assert.Equal(t, cw.Output(), "[Error] unexpected\n")
}
