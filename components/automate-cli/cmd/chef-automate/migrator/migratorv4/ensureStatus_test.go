package migratorV4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestEnsureStatus(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		GetServicesStatusFunc: func() (bool, error) { return true, nil },
	}
	ms := NewEnsureStatus(cw.CliWriter, mmu)
	err := ms.Run()
	assert.NoError(t, err)
}

func TestEnsureStatusWithError(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		GetServicesStatusFunc: func() (bool, error) { return false, errors.New("unexpected") },
	}
	ms := NewEnsureStatus(cw.CliWriter, mmu)
	err := ms.Run()
	assert.Error(t, err)
}
