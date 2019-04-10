package deployment

import (
	"testing"

	dc "github.com/chef/automate/api/config/deployment"
	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/stretchr/testify/require"
)

func TestNewUserOverrideConfigFromBackupRestoreTask(t *testing.T) {
	t.Run("nil task", func(t *testing.T) {
		cfg := NewUserOverrideConfigFromBackupRestoreTask(nil)
		require.Equal(t, &dc.AutomateConfig{}, cfg)
	})
	t.Run("filesystem restore task", func(t *testing.T) {
		// In filesystem mode we don't yet care about information passed through
		// the restore task so the override config should be blank
		rt := NewBackupRestoreTask()
		cfg := NewUserOverrideConfigFromBackupRestoreTask(rt)
		require.Equal(t, &dc.AutomateConfig{}, cfg)
	})
	t.Run("s3 restore task", func(t *testing.T) {
		rt := NewBackupRestoreTask()
		rt.S3BackupLocation = &S3BackupLocation{
			BucketName:   "backup-bucket",
			BasePath:     "my-org/chef-automate",
			Endpoint:     "s3-us-east-2.amazonaws.com",
			AccessKey:    "AKIMYACCESSKEY",
			SecretKey:    "MYSUPERSECRETKEY",
			SessionToken: "MYSESSIONTOKEN",
		}
		expectedCfg := &dc.AutomateConfig{Global: config.NewGlobalConfig()}
		expectedCfg.Global.V1.Backups = &config.Backups{
			S3: &config.Backups_S3{
				Bucket: &config.Backups_S3_Bucket{
					Name:     w.String("backup-bucket"),
					BasePath: w.String("my-org/chef-automate"),
					Endpoint: w.String("s3-us-east-2.amazonaws.com"),
				},
				Credentials: &config.Backups_S3_AWSCredentials{
					AccessKey:    w.String("AKIMYACCESSKEY"),
					SecretKey:    w.String("MYSUPERSECRETKEY"),
					SessionToken: w.String("MYSESSIONTOKEN"),
				},
			},
		}
		cfg := NewUserOverrideConfigFromBackupRestoreTask(rt)
		require.Equal(t, expectedCfg, cfg)
	})
}
