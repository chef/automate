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
					Gcs: &ConfigRequest_V1_System_Backups_GCSSettings{
						Es: &ac.Backups_GCS_Elasticsearch{},
					},
				},
				Log: &ConfigRequest_V1_System_Log{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
// nolint: gomnd
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("0.0.0.0")
	c.V1.Sys.Service.Port = w.Int32(10123)

	c.V1.Sys.Log.Level = w.String("info")
	c.V1.Sys.Log.Format = w.String("text")

	c.V1.Sys.Backups.Backend = w.String("s3")
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

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = logLevel
	}

	if logFormat := g.GetV1().GetLog().GetFormat().GetValue(); logFormat != "" {
		c.V1.Sys.Log.Format.Value = logFormat
	}

	//
	// Users can configure the ElasticSearch backup configuration the at es-sidecar
	// level, the global backups level, and in some cases the global external
	// level. Precedence for configuration is as follows in order of priority
	//
	// * External configuration (if using external ElasticSearch)
	// * Global configuration
	// * User defined es-sidecar configuration
	// * es-sidecar default configuration
	//
	externalCfg := g.GetV1().GetExternal().GetElasticsearch()

	backupsCfg := c.GetV1().GetSys().GetBackups()
	if backupsCfg == nil {
		backupsCfg = &ConfigRequest_V1_System_Backups{}
	}

	c.applyGlobalBackupConfig(backupsCfg, g.GetV1().GetBackups())

	if externalCfg.GetEnable().GetValue() {
		c.applyExternalConfig(backupsCfg, externalCfg.GetBackup())
		// Early return as we don't need to migrate to the backup-gateway with
		// external es
		return
	}

	c.migrateToBackupGateway(backupsCfg)

	return
}

// Given a backup config and an external configuration, merge any external values
// into the backup config
func (c *ConfigRequest) applyExternalConfig(
	cfg *ConfigRequest_V1_System_Backups,
	override *ac.External_Elasticsearch_Backup) {

	// If backups are disabled we don't need to worry about applying any other
	// config.
	if !override.GetEnable().GetValue() {
		cfg.Backend = w.String("disable")
		return
	}

	// Repo verification is only useful on multi-node clusters, as such it is
	// disabled by default. If we're applying global config we're using an external
	// cluster that likely has multiple nodes so we'll enable it if the user
	// has not configured it.
	if cfg.GetVerifyRepo() == nil {
		cfg.VerifyRepo = w.Bool(true)
	}

	backend := override.GetLocation().GetValue()
	switch backend {
	case "s3":
		cfg.Backend = w.String(backend)
		if cfg.S3 == nil {
			cfg.S3 = &ConfigRequest_V1_System_Backups_S3Settings{}
		}

		extS3cfg := override.GetS3()
		if b := extS3cfg.GetBucket(); b != nil {
			cfg.S3.Bucket = b
		}

		if bp := extS3cfg.GetBasePath(); bp != nil {
			cfg.S3.BasePath = bp
		}

		if c := extS3cfg.GetClient(); c != nil {
			cfg.S3.Client = c
		}

		if s := extS3cfg.GetSettings(); s != nil {
			cfg.S3.Es = s
		}
	case "gcs":
		cfg.Backend = w.String(backend)
		if cfg.Gcs == nil {
			cfg.Gcs = &ConfigRequest_V1_System_Backups_GCSSettings{}
		}

		extGcscfg := override.GetGcs()
		if b := extGcscfg.GetBucket(); b != nil {
			cfg.Gcs.Bucket = b
		}

		if bp := extGcscfg.GetBasePath(); bp != nil {
			cfg.Gcs.BasePath = bp
		}

		if c := extGcscfg.GetClient(); c != nil {
			cfg.Gcs.Client = c
		}

		if s := extGcscfg.GetSettings(); s != nil {
			cfg.Gcs.Es = s
		}
	case "fs":
		cfg.Backend = w.String(backend)
		if cfg.Fs == nil {
			cfg.Fs = &ConfigRequest_V1_System_Backups_FsSettings{}
		}

		fsCfg := override.GetFs()
		if p := fsCfg.GetPath(); p != nil {
			cfg.Fs.RootLocation = p
		}

		if rb := fsCfg.GetSettings().GetMaxRestoreBytesPerSec(); rb != nil {
			cfg.Fs.MaxRestoreBytesPerSec = rb
		}

		if sb := fsCfg.GetSettings().GetMaxSnapshotBytesPerSec(); sb != nil {
			cfg.Fs.MaxSnapshotBytesPerSec = sb
		}
	default:
		// Disable external backups by default
		cfg.Backend = w.String("disabled")
	}

	return
}

// Given a backup config and the global backup config, merge any global values
// into the backup config
func (c *ConfigRequest) applyGlobalBackupConfig(
	cfg *ConfigRequest_V1_System_Backups,
	global *ac.Backups) {

	if global == nil {
		return
	}

	if backend := global.GetLocation().GetValue(); backend != "" {
		cfg.Backend = w.String(backend)
	}

	if globalS3 := global.GetS3(); globalS3 != nil {
		if cfg.GetS3() == nil {
			cfg.S3 = &ConfigRequest_V1_System_Backups_S3Settings{}
		}

		if name := globalS3.GetBucket().GetName(); name != nil {
			cfg.S3.Bucket = name
		}

		if bp := globalS3.GetBucket().GetBasePath(); bp != nil {
			cfg.S3.BasePath = bp
		}

		if es := globalS3.GetEs(); es != nil {
			newEs := &ac.Backups_S3_Elasticsearch{}
			_ = ac.Merge(cfg.S3.GetEs(), es, newEs)

			cfg.S3.Es = newEs
		}
	}

	if globalGcs := global.GetGcs(); globalGcs != nil {
		// ES is configured to talk to minio which presents ALL buckets as 'S3' buckets so we have to pass the same config as S3
		cfg.Backend = w.String("s3")

		if cfg.GetS3() == nil {
			cfg.S3 = &ConfigRequest_V1_System_Backups_S3Settings{}
		}

		if name := globalGcs.GetBucket().GetName(); name != nil {
			cfg.S3.Bucket = name
		}

		if bp := globalGcs.GetBucket().GetBasePath(); bp != nil {
			cfg.S3.BasePath = bp
		}

		if es := globalGcs.GetEs(); es != nil {
			newEs := &ac.Backups_S3_Elasticsearch{}
			_ = ac.Merge(cfg.S3.GetEs(), es, newEs)

			cfg.S3.Es = newEs
		}
	}

	if globalFs := global.GetFilesystem(); globalFs != nil {
		if p := globalFs.GetPath(); p != nil {
			cfg.Fs.RootLocation = p
		}

		if sb := globalFs.GetEsMaxSnapshotBytesPerSec(); sb != nil {
			cfg.Fs.MaxSnapshotBytesPerSec = sb
		}

		if rb := globalFs.GetEsMaxRestoreBytesPerSec(); rb != nil {
			cfg.Fs.MaxRestoreBytesPerSec = rb
		}
	}

	return
}

// Given a backup config that is not using external elasticsearch, migrate
// old fs backend to the s3 compatible backup-gateway
func (c *ConfigRequest) migrateToBackupGateway(cfg *ConfigRequest_V1_System_Backups) {
	// If the backend is configured to use S3 then we need not worry about fallback
	if cfg.GetBackend().GetValue() == "s3" || cfg.GetBackend().GetValue() == "gcs" {
		return
	}

	// Here we're going to catch our default setting of "fs", which
	// now uses the backup-gateway in S3 mode. So we'll configure our
	// backend to the backup-gateway with "backups" as the default bucket
	cfg.Backend = w.String("s3")
	cfg.S3 = &ConfigRequest_V1_System_Backups_S3Settings{
		Bucket: w.String("backups"),
		Es: &ac.Backups_S3_Elasticsearch{
			MaxSnapshotBytesPerSec: cfg.GetFs().GetMaxSnapshotBytesPerSec(),
			MaxRestoreBytesPerSec:  cfg.GetFs().GetMaxRestoreBytesPerSec(),
		},
	}

	return
}
