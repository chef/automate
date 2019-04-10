package es_sidecar

import (
	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:    &ac.Mlsa{},
				Service: &ConfigRequest_V1_System_Service{},
				Backups: &ConfigRequest_V1_System_Backups{
					Fs: &ConfigRequest_V1_System_Backups_FsSettings{},
					S3: &ConfigRequest_V1_System_Backups_S3Settings{
						Es: &ac.Backups_S3_Elasticsearch{},
					},
				},
				Log: &ConfigRequest_V1_System_Log{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("0.0.0.0")
	c.V1.Sys.Service.Port = w.Int32(10123)

	c.V1.Sys.Log.Level = w.String("info")
	c.V1.Sys.Log.Format = w.String("text")

	c.V1.Sys.Backups.Backend = w.String("fs")
	c.V1.Sys.Backups.S3.Es.Compress = w.Bool(true)
	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	return nil
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(creds *ac.TLSCredentials) (ac.PreparedSystemConfig, error) {
	sys := c.V1.Sys
	sys.Tls = creds
	return c.V1.Sys, nil
}

// SetGlobalConfig imports settings from the global configuration
func (c *ConfigRequest) SetGlobalConfig(g *ac.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa
	if externalCfg := g.GetV1().GetExternal().GetElasticsearch(); externalCfg.GetEnable().GetValue() {
		if externalCfg.GetBackup().GetEnable().GetValue() {
			switch externalCfg.GetBackup().GetLocation().GetValue() {
			case "s3":
				s3cfg := externalCfg.GetBackup().GetS3()
				c.V1.Sys.Backups = &ConfigRequest_V1_System_Backups{
					Backend: w.String("s3"),
					S3: &ConfigRequest_V1_System_Backups_S3Settings{
						Bucket:   s3cfg.GetBasePath(),
						BasePath: s3cfg.GetBasePath(),
						Client:   s3cfg.GetClient(),
						Es:       s3cfg.GetSettings(),
					},
				}
			case "fs":
				fsCfg := externalCfg.GetBackup().GetFs()
				c.V1.Sys.Backups = &ConfigRequest_V1_System_Backups{
					Backend: w.String("fs"),
					Fs: &ConfigRequest_V1_System_Backups_FsSettings{
						RootLocation:           fsCfg.GetPath(),
						MaxRestoreBytesPerSec:  fsCfg.GetSettings().GetMaxRestoreBytesPerSec(),
						MaxSnapshotBytesPerSec: fsCfg.GetSettings().GetMaxSnapshotBytesPerSec(),
					},
				}
			default:
				c.V1.Sys.Backups = &ConfigRequest_V1_System_Backups{
					Backend: w.String("disable"),
				}
			}
		} else {
			c.V1.Sys.Backups = &ConfigRequest_V1_System_Backups{
				Backend: w.String("disable"),
			}
		}
	} else {
		if b := g.GetV1().GetBackups(); b != nil {
			switch b.GetLocation().GetValue() {
			case "s3":
				globalS3 := b.GetS3()
				c.V1.Sys.Backups.Backend = w.String("s3")
				c.V1.Sys.Backups.S3 = &ConfigRequest_V1_System_Backups_S3Settings{
					Bucket:   globalS3.GetBucket().GetName(),
					BasePath: globalS3.GetBucket().GetBasePath(),
					Es:       globalS3.GetEs(),
				}
			default:
				// We continue to provide the FS config. This will be used if elasticsearch
				// is external
				c.V1.Sys.Backups.Backend = w.String("fs")

				if g.V1.Backups.Filesystem.Path != nil {
					c.V1.Sys.Backups.Fs.RootLocation = g.V1.Backups.Filesystem.Path
				}
				if g.V1.Backups.Filesystem.EsMaxSnapshotBytesPerSec != nil {
					c.V1.Sys.Backups.Fs.MaxSnapshotBytesPerSec = g.V1.Backups.Filesystem.EsMaxSnapshotBytesPerSec
				}
				if g.V1.Backups.Filesystem.EsMaxRestoreBytesPerSec != nil {
					c.V1.Sys.Backups.Fs.MaxRestoreBytesPerSec = g.V1.Backups.Filesystem.EsMaxRestoreBytesPerSec
				}

				// Here we're going to catch our default setting of "fs", which
				// now uses the backup-gateway in S3 mode. So we'll configure our
				// backend to the backup-gateway with the filesystem path base name
				// as our bucket.
				c.V1.Sys.Backups.Backend = w.String("s3")
				c.V1.Sys.Backups.S3 = &ConfigRequest_V1_System_Backups_S3Settings{
					Bucket: w.String("backups"),
					Es: &ac.Backups_S3_Elasticsearch{
						MaxSnapshotBytesPerSec: b.GetFilesystem().GetEsMaxSnapshotBytesPerSec(),
						MaxRestoreBytesPerSec:  b.GetFilesystem().GetEsMaxRestoreBytesPerSec(),
					},
				}
			}
		}
	}

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = logLevel
	}

	if logFormat := g.GetV1().GetLog().GetFormat().GetValue(); logFormat != "" {
		c.V1.Sys.Log.Format.Value = logFormat
	}
}
