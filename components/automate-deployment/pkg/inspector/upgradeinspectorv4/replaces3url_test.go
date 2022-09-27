package upgradeinspectorv4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestInspectReplaceS3URL(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	ds := NewReplaceS3UrlInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		GetBackupS3URLFunc: func(timeout int64) (string, error) {
			return "https://s3.us-east-1.amazonaws.com", nil
		},
		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
	}, 10)
	err := ds.Inspect()
	assert.NoError(t, err)
}

func TestInspectReplaceS3URLErrorInGetUrl(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	ds := NewReplaceS3UrlInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		GetBackupS3URLFunc: func(timeout int64) (string, error) {
			return "", errors.New("unexpected")
		},
		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
	}, 10)
	err := ds.Inspect()
	assert.Error(t, err)
}

func TestInspectReplaceS3URLErrorInPatchUrl(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	ds := NewReplaceS3UrlInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		GetBackupS3URLFunc: func(timeout int64) (string, error) {
			return "https://s3.us-east-1.amazonaws.com", nil
		},
		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
			return "", "", errors.New("unexpected")
		},
	}, 10)
	err := ds.Inspect()
	assert.Error(t, err)
}

func TestOutputInspectReplaceS3URLErrorInGetUrl(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	ds := NewReplaceS3UrlInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		GetBackupS3URLFunc: func(timeout int64) (string, error) {
			return "", errors.New("unexpected")
		},
		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
	}, 10)
	err := ds.Inspect()
	assert.Error(t, err)
	err = ds.ExitHandler()
	assert.NoError(t, err)
	expected := "[Error] unexpected\nUpgrade process terminated.\n"
	assert.Equal(t, expected, tw.Output())
}
