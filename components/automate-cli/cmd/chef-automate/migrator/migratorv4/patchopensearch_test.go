package migratorv4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestPatchOsSuccess(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
	}
	poc := NewPatchOpensearchConfig(cw.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return majorupgrade_utils.HAB_DIR },
	}, SPINNER_TEST_DURATION)
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
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return -1, errors.New("unexpected") },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
	}
	poc := NewPatchOpensearchConfig(cw.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return majorupgrade_utils.HAB_DIR },
	}, SPINNER_TEST_DURATION)
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
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", errors.New("unexpected error") },
	}
	poc := NewPatchOpensearchConfig(cw.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return majorupgrade_utils.HAB_DIR },
	}, SPINNER_TEST_DURATION)
	poc.Run()
	poc.ErrorHandler()
	expected1 := "Updating OpenSearch configurations"
	expected2 := "✖  Failed to update OpenSearch configurations"
	expected3 := "unexpected error"
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
	assert.Contains(t, cw.Output(), expected3)
}

func TestCalculateMaxTotalShardsLessThan1500(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	poc := NewPatchOpensearchConfig(cw.CliWriter, &MockMigratorV4UtilsImpl{}, &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return majorupgrade_utils.HAB_DIR },
	}, SPINNER_TEST_DURATION)
	var shardsUsed int32 = 1200
	var minShardVal int32 = MINIMUM_SHARD_VALUE
	var incrementShardValue int32 = INDICES_TOTAL_SHARD_INCREMENT_DEFAULT
	maxTotalShards := poc.calculateMaxTotalShards(shardsUsed, minShardVal, incrementShardValue)
	var expectedCount int32 = 2500
	assert.Equal(t, expectedCount, maxTotalShards)
}

func TestCalculateMaxTotalShardsMoreThan1500(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	poc := NewPatchOpensearchConfig(cw.CliWriter, &MockMigratorV4UtilsImpl{}, &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return majorupgrade_utils.HAB_DIR },
	}, SPINNER_TEST_DURATION)
	var shardsUsed int32 = 1800
	var minShardVal int32 = MINIMUM_SHARD_VALUE
	var incrementShardValue int32 = INDICES_TOTAL_SHARD_INCREMENT_DEFAULT
	maxTotalShards := poc.calculateMaxTotalShards(shardsUsed, minShardVal, incrementShardValue)
	var expectedCount int32 = 2800
	assert.Equal(t, expectedCount, maxTotalShards)
}
