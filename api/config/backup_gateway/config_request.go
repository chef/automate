package backupgw

import (
	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Service: &ConfigRequest_V1_System_Service{},
				Gateway: &ConfigRequest_V1_System_Gateway{
					Backup: &ConfigRequest_V1_System_Gateway_Backup{
						Filesystem: &ConfigRequest_V1_System_Gateway_Backup_Filesystem{},
						S3: &ac.Backups_S3{
							Bucket:      &ac.Backups_S3_Bucket{},
							Credentials: &ac.Backups_S3_AWSCredentials{},
							Es:          &ac.Backups_S3_Elasticsearch{},
						},
					},
				},
			},
		},
	}
}

func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("127.0.0.1")
	c.V1.Sys.Service.Port = w.Int32(10143)
	return c
}

func (c *ConfigRequest) Validate() error {
	return nil
}

func (c *ConfigRequest) PrepareSystemConfig(creds *ac.TLSCredentials) (ac.PreparedSystemConfig, error) {
	sys := c.V1.Sys
	sys.Tls = creds
	return c.V1.Sys, nil
}

// SetGlobalConfig imports settings from the global configuration
func (c *ConfigRequest) SetGlobalConfig(g *ac.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa

	if b := g.GetV1().GetBackups(); b != nil {
		switch b.GetLocation().GetValue() {
		case "s3":
			if globalS3 := b.GetS3(); globalS3 != nil {
				// It's possible that a user may have configured s3 backups at
				// both the global and backup-gateway levels. If so, merge the global
				// configuration into the existing bgw backup config.
				// Otherwise, copy the global configuration.
				merged := &ac.Backups_S3{}
				bgws3 := c.GetV1().GetSys().GetGateway().GetBackup().GetS3()
				if bgws3 != nil {
					ac.Merge(bgws3, globalS3, merged) // nolint errcheck
					c.V1.Sys.Gateway.Backup.S3 = merged
				} else {
					c.V1.Sys.Gateway.Backup.S3 = globalS3
				}
			}
		case "gcs":
			if globalGCS := b.GetGcs(); globalGCS != nil {
				// It's possible that a user may have configured GCS backups at
				// both the global and backup-gateway levels. If so, merge the global
				// configuration into the existing bgw backup config.
				// Otherwise, copy the global configuration.
				merged := &ac.Backups_GCS{}
				bgwgcs := c.GetV1().GetSys().GetGateway().GetBackup().GetGcs()
				if bgwgcs != nil {
					ac.Merge(bgwgcs, globalGCS, merged) // nolint errcheck
					c.V1.Sys.Gateway.Backup.Gcs = merged
				} else {
					c.V1.Sys.Gateway.Backup.Gcs = globalGCS
				}
				// Escape /n in cred string
				credstring := c.GetV1().GetSys().GetGateway().GetBackup().GetGcs().GetCredentials().GetJson().Value
				c.V1.Sys.Gateway.Backup.Gcs.Credentials.Json = w.String(ac.PrepareGCSCredentials(credstring))
			}
		default:
			if path := g.GetV1().GetBackups().GetFilesystem().GetPath().GetValue(); path != "" {
				// Set the base path to the directory containing the backups so
				// we can use the dirname as the bucket name.
				c.V1.Sys.Gateway.Backup.Filesystem.Path = w.String(path)
			}
		}
	}
}
