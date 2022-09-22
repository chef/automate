package upgradeinspectorv4

import (
	"testing"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/stretchr/testify/assert"
)

func TestInspect(t *testing.T) {
	tw := NewTestWriter()
	ds := NewReplaceS3UrlInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		GetBackupS3URLFunc: func() (string, error) {
			return "https://s3.us-east-1.amazonaws.com", nil
		},
		PatchS3backupURLFunc: func(w *cli.Writer) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
	})
	err := ds.Inspect()
	assert.NoError(t, err)
}
