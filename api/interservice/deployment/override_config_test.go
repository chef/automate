package deployment

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/config/compliance"
	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/event_feed"
	"github.com/chef/automate/api/config/ingest"
	"github.com/chef/automate/api/config/load_balancer"
	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/lib/pcmp/prequire"
)

// Return a valid user override config that a user must pass when configuring
// Automate.
func newValidOverrideConfig() *dc.AutomateConfig {
	cfg := &dc.AutomateConfig{
		Global: &config.GlobalConfig{
			V1: &config.V1{
				Fqdn: w.String("test-fqdn"),
			},
		},
		LoadBalancer: &load_balancer.ConfigRequest{
			V1: &load_balancer.ConfigRequest_V1{
				Sys: &load_balancer.ConfigRequest_V1_System{
					FrontendTls: []*config.FrontendTLSCredential{
						&config.FrontendTLSCredential{
							ServerName: "test",
							Cert:       "cert",
							Key:        "key",
						},
					},
				},
			},
		},
	}

	return cfg
}

func newDeprecatedConfig() *dc.AutomateConfig {
	cfg := newValidOverrideConfig()
	cfg.Compliance = compliance.NewConfigRequest()
	cfg.Compliance.V1.Sys.Retention = &compliance.ConfigRequest_V1_System_Retention{
		ComplianceReportDays: w.Int32(30),
	}
	cfg.EventFeedService = event_feed.NewConfigRequest()
	cfg.EventFeedService.V1.Sys.Service.PurgeEventFeedAfterDays = w.Int32(7)
	cfg.Ingest = ingest.NewConfigRequest()
	cfg.Ingest.V1.Sys.Service.PurgeActionsAfterDays = w.Int32(60)
	cfg.Ingest.V1.Sys.Service.PurgeConvergeHistoryAfterDays = w.Int32(60)

	return cfg
}

func TestMergeAndValidateNewUserOverrideConfig(t *testing.T) {
	t.Run("nil restore task", func(t *testing.T) {
		cfg := newValidOverrideConfig()
		err := MergeAndValidateNewUserOverrideConfig(cfg, nil)
		require.NoError(t, err)
		require.Equal(t, newValidOverrideConfig(), cfg)
	})

	t.Run("filesystem restore task", func(t *testing.T) {
		// In filesystem mode we don't yet care about information passed through
		// the restore task so the override config should be blank
		rt := NewBackupRestoreTask()
		cfg := newValidOverrideConfig()
		t.Log(cfg)
		err := MergeAndValidateNewUserOverrideConfig(cfg, rt)
		t.Log(cfg)
		require.NoError(t, err)
		prequire.Equal(t, newValidOverrideConfig(), cfg)
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
		expectedCfg := newValidOverrideConfig()
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
		cfg := newValidOverrideConfig()
		err := MergeAndValidateNewUserOverrideConfig(cfg, rt)
		require.NoError(t, err)
		prequire.Equal(t, expectedCfg, cfg)
	})

	t.Run("config patch 1", func(t *testing.T) {
		cfg := newValidOverrideConfig()
		rt := NewBackupRestoreTask()
		rt.S3BackupLocation = &S3BackupLocation{
			BucketName:   "backup-bucket",
			BasePath:     "my-org/chef-automate",
			Endpoint:     "s3-us-east-2.amazonaws.com",
			AccessKey:    "AKIMYACCESSKEY",
			SecretKey:    "MYSUPERSECRETKEY",
			SessionToken: "MYSESSIONTOKEN",
		}
		rt.PatchConfig = &dc.AutomateConfig{
			Global: config.NewGlobalConfig(),
		}
		rt.PatchConfig.Global.V1.Log = &config.Log{
			Level: w.String("debug"),
		}
		rt.PatchConfig.Global.V1.Backups = &config.Backups{
			S3: &config.Backups_S3{
				Bucket: &config.Backups_S3_Bucket{
					Name:     w.String("override-backup-bucket"),
					BasePath: w.String("my-org/chef-automate-override"),
					Endpoint: w.String("s3-us-east-1.amazonaws.com"),
				},
				Credentials: &config.Backups_S3_AWSCredentials{
					AccessKey:    w.String("AKIMYACCESSKEYOVERRIDE"),
					SecretKey:    w.String("MYSUPERSECRETKEYOVERRIDE"),
					SessionToken: w.String("MYSESSIONTOKENOVERRIDE"),
				},
			},
		}

		// Make sure the patch config overrides the default backup config
		// from the restore task
		expectedCfg := newValidOverrideConfig()
		expectedCfg.Global.V1.Backups = &config.Backups{
			S3: &config.Backups_S3{
				Bucket: &config.Backups_S3_Bucket{
					Name:     w.String("override-backup-bucket"),
					BasePath: w.String("my-org/chef-automate-override"),
					Endpoint: w.String("s3-us-east-1.amazonaws.com"),
				},
				Credentials: &config.Backups_S3_AWSCredentials{
					AccessKey:    w.String("AKIMYACCESSKEYOVERRIDE"),
					SecretKey:    w.String("MYSUPERSECRETKEYOVERRIDE"),
					SessionToken: w.String("MYSESSIONTOKENOVERRIDE"),
				},
			},
		}
		expectedCfg.Global.V1.Log = &config.Log{
			Level: w.String("debug"),
		}
		err := MergeAndValidateNewUserOverrideConfig(cfg, rt)
		require.NoError(t, err)
		prequire.Equal(t, expectedCfg, cfg)
	})

	t.Run("config patch 2", func(t *testing.T) {
		cfg := newValidOverrideConfig()
		rt := NewBackupRestoreTask()

		rt.PatchConfig = &dc.AutomateConfig{
			Global: config.NewGlobalConfig(),
		}
		rt.PatchConfig.Global.V1.Log = &config.Log{
			Level: w.String("debug"),
		}

		// Make sure the patch config overrides the default backup config
		// from the restore task
		expectedCfg := newValidOverrideConfig()

		expectedCfg.Global.V1.Log = &config.Log{
			Level: w.String("debug"),
		}
		err := MergeAndValidateNewUserOverrideConfig(cfg, rt)
		require.NoError(t, err)
		prequire.Equal(t, expectedCfg, cfg)
	})

	t.Run("config set", func(t *testing.T) {
		cfg := newValidOverrideConfig()
		rt := NewBackupRestoreTask()
		rt.S3BackupLocation = &S3BackupLocation{
			BucketName:   "backup-bucket",
			BasePath:     "my-org/chef-automate",
			Endpoint:     "s3-us-east-2.amazonaws.com",
			AccessKey:    "AKIMYACCESSKEY",
			SecretKey:    "MYSUPERSECRETKEY",
			SessionToken: "MYSESSIONTOKEN",
		}
		rt.SetConfig = newValidOverrideConfig()
		rt.SetConfig.Global.V1.Log = &config.Log{
			Level: w.String("debug"),
		}
		rt.SetConfig.Global.V1.Backups = &config.Backups{
			S3: &config.Backups_S3{
				Bucket: &config.Backups_S3_Bucket{
					Name:     w.String("override-backup-bucket"),
					BasePath: w.String("my-org/chef-automate-override"),
					Endpoint: w.String("s3-us-east-1.amazonaws.com"),
				},
				Credentials: &config.Backups_S3_AWSCredentials{
					AccessKey:    w.String("AKIMYACCESSKEYOVERRIDE"),
					SecretKey:    w.String("MYSUPERSECRETKEYOVERRIDE"),
					SessionToken: w.String("MYSESSIONTOKENOVERRIDE"),
				},
			},
		}

		// Make sure the set config is exactly what we've passed
		expectedCfg := newValidOverrideConfig()
		expectedCfg.Global.V1.Backups = &config.Backups{
			S3: &config.Backups_S3{
				Bucket: &config.Backups_S3_Bucket{
					Name:     w.String("override-backup-bucket"),
					BasePath: w.String("my-org/chef-automate-override"),
					Endpoint: w.String("s3-us-east-1.amazonaws.com"),
				},
				Credentials: &config.Backups_S3_AWSCredentials{
					AccessKey:    w.String("AKIMYACCESSKEYOVERRIDE"),
					SecretKey:    w.String("MYSUPERSECRETKEYOVERRIDE"),
					SessionToken: w.String("MYSESSIONTOKENOVERRIDE"),
				},
			},
		}
		expectedCfg.Global.V1.Log = &config.Log{
			Level: w.String("debug"),
		}
		err := MergeAndValidateNewUserOverrideConfig(cfg, rt)
		require.NoError(t, err)
		prequire.Equal(t, expectedCfg, cfg)
	})

	t.Run("existing deprecated config", func(t *testing.T) {
		rt := NewBackupRestoreTask()
		cfg := newValidOverrideConfig()
		err := cfg.OverrideConfigValues(newDeprecatedConfig())
		require.NoError(t, err)
		err = MergeAndValidateNewUserOverrideConfig(cfg, rt)
		require.NoError(t, err)
	})

	t.Run("config set with deprecated config", func(t *testing.T) {
		rt := NewBackupRestoreTask()
		cfg := newValidOverrideConfig()
		err := cfg.OverrideConfigValues(newDeprecatedConfig())
		require.NoError(t, err)

		rt.SetConfig = cfg
		err = MergeAndValidateNewUserOverrideConfig(cfg, rt)
		require.NoError(t, err)
	})

	t.Run("config patch with deprecated config", func(t *testing.T) {
		rt := NewBackupRestoreTask()
		cfg := newValidOverrideConfig()
		rt.PatchConfig = newDeprecatedConfig()
		err := MergeAndValidateNewUserOverrideConfig(cfg, rt)
		require.NoError(t, err)
	})

	t.Run("config patch valid patch with existing deprecated config", func(t *testing.T) {
		rt := NewBackupRestoreTask()
		cfg := newValidOverrideConfig()
		err := cfg.OverrideConfigValues(newDeprecatedConfig())
		require.NoError(t, err)

		rt.PatchConfig = &dc.AutomateConfig{
			Global: config.NewGlobalConfig(),
		}
		rt.PatchConfig.Global.V1.Log = &config.Log{
			Level: w.String("debug"),
		}

		expectedCfg := newValidOverrideConfig()
		err = expectedCfg.OverrideConfigValues(newDeprecatedConfig())
		require.NoError(t, err)
		expectedCfg.Global.V1.Log = &config.Log{
			Level: w.String("debug"),
		}

		err = MergeAndValidateNewUserOverrideConfig(cfg, rt)
		require.NoError(t, err)
	})

	t.Run("config patch deprecated patch with existing deprecated config", func(t *testing.T) {
		rt := NewBackupRestoreTask()
		cfg := newValidOverrideConfig()
		err := cfg.OverrideConfigValues(newDeprecatedConfig())
		require.NoError(t, err)

		rt.PatchConfig = newDeprecatedConfig()
		rt.PatchConfig.Compliance.V1.Sys.Retention.ComplianceReportDays = w.Int32(10)
		rt.PatchConfig.EventFeedService.V1.Sys.Service.PurgeEventFeedAfterDays = w.Int32(7)
		rt.PatchConfig.Ingest.V1.Sys.Service.PurgeActionsAfterDays = w.Int32(60)
		rt.PatchConfig.Ingest.V1.Sys.Service.PurgeConvergeHistoryAfterDays = w.Int32(60)

		err = MergeAndValidateNewUserOverrideConfig(cfg, rt)
		require.NoError(t, err)
	})
}
