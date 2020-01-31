package es_sidecar

import (
	"testing"

	wrappers "github.com/golang/protobuf/ptypes/wrappers"
	"github.com/stretchr/testify/assert"

	shared "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestValidateConfigRequestValid(t *testing.T) {
	c := DefaultConfigRequest()
	err := c.Validate()
	assert.Nil(t, err)
}

func TestSetGlobalConfig(t *testing.T) {
	t.Run("it configures the MLSA", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.V1.Sys.Mlsa.Accept = w.Bool(false)
		g := shared.DefaultGlobalConfig()
		g.V1.Mlsa.Accept = w.Bool(true)

		c.SetGlobalConfig(g)

		assert.Equal(t, true, c.V1.Sys.Mlsa.Accept.Value)
	})

	t.Run("with external s3 backup config", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.V1.Sys.Backups.S3.Bucket = w.String("internal-bucket")
		c.V1.Sys.Backups.S3.Client = w.String("internal-client")
		c.V1.Sys.Backups.S3.BasePath = w.String("internal-base-path")
		c.V1.Sys.Backups.S3.Es.Compress = w.Bool(false)

		g := shared.DefaultGlobalConfig()
		g.V1.External = &shared.External{
			Elasticsearch: &shared.External_Elasticsearch{
				Enable: w.Bool(true),
				Backup: &shared.External_Elasticsearch_Backup{
					Enable:   w.Bool(true),
					Location: w.String("s3"),
					S3: &shared.External_Elasticsearch_Backup_S3Settings{
						Bucket:   w.String("external-bucket"),
						Client:   w.String("external-client"),
						BasePath: w.String("external-base-path"),
						Settings: &shared.Backups_S3_Elasticsearch{
							Compress: w.Bool(true),
						},
					},
				},
			},
		}
		c.SetGlobalConfig(g)

		assert.Equal(t, "s3", c.V1.Sys.Backups.Backend.Value)
		assert.Equal(t, "external-bucket", c.V1.Sys.Backups.S3.Bucket.Value)
		assert.Equal(t, "external-client", c.V1.Sys.Backups.S3.Client.Value)
		assert.Equal(t, "external-base-path", c.V1.Sys.Backups.S3.BasePath.Value)
		assert.Equal(t, true, c.V1.Sys.Backups.S3.Es.Compress.Value)
	})

	t.Run("with external fs backup config", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.V1.Sys.Backups.Fs.RootLocation = w.String("internal-root")
		c.V1.Sys.Backups.Fs.MaxRestoreBytesPerSec = w.String("internal-restore-bytes")
		c.V1.Sys.Backups.Fs.MaxSnapshotBytesPerSec = w.String("internal-snapshot-bytes")

		g := shared.DefaultGlobalConfig()
		g.V1.External = &shared.External{
			Elasticsearch: &shared.External_Elasticsearch{
				Enable: w.Bool(true),
				Backup: &shared.External_Elasticsearch_Backup{
					Enable:   w.Bool(true),
					Location: w.String("fs"),
					Fs: &shared.External_Elasticsearch_Backup_FsSettings{
						Path: w.String("external-root"),
						Settings: &shared.External_Elasticsearch_Backup_FsSettings_OptionalSettings{
							MaxRestoreBytesPerSec:  w.String("external-restore-bytes"),
							MaxSnapshotBytesPerSec: w.String("external-snapshot-bytes"),
						},
					},
				},
			},
		}
		c.SetGlobalConfig(g)

		assert.Equal(t, "fs", c.V1.Sys.Backups.Backend.Value)
		assert.Equal(t, "external-root", c.V1.Sys.Backups.Fs.RootLocation.Value)
		assert.Equal(t, "external-restore-bytes", c.V1.Sys.Backups.Fs.MaxRestoreBytesPerSec.Value)
		assert.Equal(t, "external-snapshot-bytes", c.V1.Sys.Backups.Fs.MaxSnapshotBytesPerSec.Value)
	})

	t.Run("with external no backup config", func(t *testing.T) {
		c := DefaultConfigRequest()

		g := shared.DefaultGlobalConfig()
		g.V1.External = &shared.External{
			Elasticsearch: &shared.External_Elasticsearch{
				Enable: w.Bool(true),
				Backup: &shared.External_Elasticsearch_Backup{
					Enable: w.Bool(false),
				},
			},
		}
		c.SetGlobalConfig(g)

		assert.Equal(t, "disable", c.V1.Sys.Backups.Backend.Value)
	})

	t.Run("with internal global s3 backup config with user config", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.V1.Sys.Backups.S3.Bucket = w.String("user-bucket")
		c.V1.Sys.Backups.S3.Client = w.String("user-client")
		c.V1.Sys.Backups.S3.BasePath = w.String("user-base-path")
		c.V1.Sys.Backups.S3.Es.Compress = w.Bool(false)

		g := shared.DefaultGlobalConfig()
		g.V1.Backups = &shared.Backups{
			Location: w.String("s3"),
			S3: &shared.Backups_S3{
				Bucket: &shared.Backups_S3_Bucket{
					Name:     w.String("global-bucket"),
					BasePath: w.String("global-base-path"),
				},
				Es: &shared.Backups_S3_Elasticsearch{
					Compress: w.Bool(true),
				},
			},
		}

		c.SetGlobalConfig(g)
		assert.Equal(t, "s3", c.V1.Sys.Backups.Backend.Value)
		assert.Equal(t, "global-bucket", c.V1.Sys.Backups.S3.Bucket.Value)
		assert.Equal(t, "user-client", c.V1.Sys.Backups.S3.Client.Value)
		assert.Equal(t, "global-base-path", c.V1.Sys.Backups.S3.BasePath.Value)
		assert.Equal(t, true, c.V1.Sys.Backups.S3.Es.Compress.Value)
	})

	t.Run("with internal global fs backup config with user config", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.V1.Sys.Backups.Fs.RootLocation = w.String("user-root")
		c.V1.Sys.Backups.Fs.MaxSnapshotBytesPerSec = w.String("user-snapshot-bytes")
		c.V1.Sys.Backups.Fs.MaxRestoreBytesPerSec = w.String("user-restore-bytes")

		g := shared.DefaultGlobalConfig()
		g.V1.Backups = &shared.Backups{
			Location: w.String("fs"),
			Filesystem: &shared.Backups_Filesystem{
				Path:                     w.String("global-root"),
				EsMaxSnapshotBytesPerSec: w.String("global-snapshot-bytes"),
				EsMaxRestoreBytesPerSec:  w.String("global-restore-bytes"),
			},
		}

		c.SetGlobalConfig(g)

		// NOTE: This is bit tricky. Why we set the location to fs in the global
		// config, we actually don't want to do that anymore. Instead of want
		// to use the s3 backup-gateway.
		assert.Equal(t, "s3", c.V1.Sys.Backups.Backend.Value)
		assert.Equal(t, "global-root", c.V1.Sys.Backups.Fs.RootLocation.Value)
		assert.Equal(t, "global-snapshot-bytes", c.V1.Sys.Backups.Fs.MaxSnapshotBytesPerSec.Value)
		assert.Equal(t, "global-restore-bytes", c.V1.Sys.Backups.Fs.MaxRestoreBytesPerSec.Value)
	})

	t.Run("with internal global s3 backup config without user config", func(t *testing.T) {
		c := DefaultConfigRequest()

		g := shared.DefaultGlobalConfig()
		g.V1.Backups = &shared.Backups{
			Location: w.String("s3"),
			S3: &shared.Backups_S3{
				Bucket: &shared.Backups_S3_Bucket{
					Name:     w.String("global-bucket"),
					BasePath: w.String("global-base-path"),
				},
				Es: &shared.Backups_S3_Elasticsearch{
					Compress: w.Bool(true),
				},
			},
		}

		c.SetGlobalConfig(g)

		assert.Equal(t, "s3", c.V1.Sys.Backups.Backend.Value)
		assert.Equal(t, "global-bucket", c.V1.Sys.Backups.S3.Bucket.Value)
		assert.Equal(t, "global-base-path", c.V1.Sys.Backups.S3.BasePath.Value)
		assert.Equal(t, true, c.V1.Sys.Backups.S3.Es.Compress.Value)
	})

	t.Run("with internal global fs backup config without user config", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.V1.Sys.Backups.Fs = &ConfigRequest_V1_System_Backups_FsSettings{}

		g := shared.DefaultGlobalConfig()
		g.V1.Backups = &shared.Backups{
			Location: w.String("fs"),
			Filesystem: &shared.Backups_Filesystem{
				Path:                     w.String("global-root"),
				EsMaxSnapshotBytesPerSec: w.String("global-snapshot-bytes"),
				EsMaxRestoreBytesPerSec:  w.String("global-restore-bytes"),
			},
		}

		c.SetGlobalConfig(g)

		// NOTE: This is bit tricky. Why we set the location to fs in the global
		// config, we actually don't want to do that anymore. Instead of want
		// to use the s3 backup-gateway.
		assert.Equal(t, "s3", c.V1.Sys.Backups.Backend.Value)
		assert.Equal(t, "global-root", c.V1.Sys.Backups.Fs.RootLocation.Value)
		assert.Equal(t, "global-snapshot-bytes", c.V1.Sys.Backups.Fs.MaxSnapshotBytesPerSec.Value)
		assert.Equal(t, "global-restore-bytes", c.V1.Sys.Backups.Fs.MaxRestoreBytesPerSec.Value)
	})

	t.Run("with no backup config", func(t *testing.T) {
		c := DefaultConfigRequest()
		g := shared.DefaultGlobalConfig()

		c.SetGlobalConfig(g)

		// Default to the "backups" bucket on the backup-gateway
		assert.Equal(t, "s3", c.V1.Sys.Backups.Backend.Value)
		assert.Equal(t, "backups", c.V1.Sys.Backups.S3.Bucket.Value)
	})

	t.Run("automate issue 2520, external es with global s3", func(t *testing.T) {
		/*
			Given the following TOML config we should properly generate the correct
			es-sidecar config with the correct backend, bucket and base path.

			[global.v1.backups]
			  location = "s3"
			[global.v1.backups.s3.bucket]
			  name = "external-bucket"
			  base_path = "external-base-path"
			  endpoint = "my.aws.com"
			[global.v1.external.elasticsearch]
			  enable = true
			  nodes = ["http://127.0.0.1:59200"]
			[global.v1.external.elasticsearch.backup]
			  enable = true
			  location = "s3"
		*/

		c := DefaultConfigRequest()
		g := shared.DefaultGlobalConfig()
		g.V1.Backups.Location = w.String("s3")
		g.V1.Backups.S3 = &shared.Backups_S3{
			Bucket: &shared.Backups_S3_Bucket{
				Name:     w.String("external-bucket"),
				BasePath: w.String("external-base-path"),
				Endpoint: w.String("my.aws.com"),
			},
		}
		g.V1.External = &shared.External{
			Elasticsearch: &shared.External_Elasticsearch{
				Enable: w.Bool(true),
				Nodes:  []*wrappers.StringValue{w.String("http://127.0.0.1:59200")},
				Backup: &shared.External_Elasticsearch_Backup{
					Enable:   w.Bool(true),
					Location: w.String("s3"),
				},
			},
		}
		c.SetGlobalConfig(g)

		assert.Equal(t, "s3", c.V1.Sys.Backups.Backend.Value)
		assert.Equal(t, "external-bucket", c.V1.Sys.Backups.S3.Bucket.Value)
		assert.Equal(t, "external-base-path", c.V1.Sys.Backups.S3.BasePath.Value)
	})
}
