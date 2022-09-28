package migratorV4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestPatchOsSuccess(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		GetEsTotalShardSettingsFunc: func() (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
	}
	poc := NewPatchOpensearchConfig(cw.CliWriter, mmu)
	poc.Run()
	poc.ErrorHandler()
	expected1 := "Updating OpenSearch configurations"
	expected2 := "✔  OpenSearch configurations updated successfully"
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
}

func TestPatchOsErrorGetShardSettings(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		GetEsTotalShardSettingsFunc: func() (int32, error) { return -1, errors.New("unexpected") },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
	}
	poc := NewPatchOpensearchConfig(cw.CliWriter, mmu)
	poc.Run()
	poc.ErrorHandler()
	expected1 := "Updating OpenSearch configurations"
	expected2 := "✔  OpenSearch configurations updated successfully"
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
}

func TestPatchOsErrorPatchingShardSettings(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		GetEsTotalShardSettingsFunc: func() (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", errors.New("unexpected error") },
	}
	poc := NewPatchOpensearchConfig(cw.CliWriter, mmu)
	poc.Run()
	poc.ErrorHandler()
	expected1 := "Updating OpenSearch configurations"
	expected2 := "✖  Failed to update OpenSearch configurations"
	expected3 := "unexpected error"
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
	assert.Contains(t, cw.Output(), expected3)
}
