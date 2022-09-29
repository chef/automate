package upgradeinspectorv4

import (
	"errors"
	"io"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestInspectStoreESSettings(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mfs := &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: GetHabRootPath,
	}
	dm := NewStoreESSettingsInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		ExecRequestFunc: func(url, methodType string, requestBody io.Reader) ([]byte, error) {
			return []byte(`{"indices":{"shards":{"total":51}}}`), nil
		},
		GetESBasePathFunc: func(timeout int64) string { return "http://localhost:10144/" },
		WriteToFileFunc:   func(filepath string, data []byte) error { return nil },
	}, mfs, 10)
	err := dm.Inspect()
	assert.NoError(t, err)
}

func TestInspectStoreESSettingsError(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mfs := &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: GetHabRootPath,
	}
	dm := NewStoreESSettingsInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		ExecRequestFunc: func(url, methodType string, requestBody io.Reader) ([]byte, error) {
			return nil, errors.New("Unreachable")
		},
		GetESBasePathFunc: func(timeout int64) string { return "http://localhost:10144/" },
		WriteToFileFunc:   func(filepath string, data []byte) error { return nil },
	}, mfs, 10)
	err := dm.Inspect()
	assert.Error(t, err)
}

func TestInspectStoreESSettingsErrorWriting(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mfs := &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: GetHabRootPath,
	}
	dm := NewStoreESSettingsInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		ExecRequestFunc: func(url, methodType string, requestBody io.Reader) ([]byte, error) {
			return []byte(`{"indices":{"shards":{"total":51}}}`), nil
		},
		GetESBasePathFunc: func(timeout int64) string { return "http://localhost:10144/" },
		WriteToFileFunc:   func(filepath string, data []byte) error { return errors.New("Unexpected") },
	}, mfs, 10)
	err := dm.Inspect()
	assert.Error(t, err)
}
