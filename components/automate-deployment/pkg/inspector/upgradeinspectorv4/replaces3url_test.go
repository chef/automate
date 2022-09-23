package upgradeinspectorv4

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestInspectReplaceS3URL(t *testing.T) {
	tw := NewTestWriter()
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
