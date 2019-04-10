package bookshelf

import (
	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Log:       &ConfigRequest_V1_System_Log{},
				Network:   &ConfigRequest_V1_System_Network{},
				Sql:       &ConfigRequest_V1_System_Sql{},
				Bookshelf: &ConfigRequest_V1_System_Bookshelf{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()

	c.V1.Sys.Network.Port = w.Int32(10201)
	c.V1.Sys.Network.ListenIp = w.String("0.0.0.0")

	c.V1.Sys.Log.Level = w.String("info")
	c.V1.Sys.Log.RotationMaxBytes = w.Int64(104857600)
	c.V1.Sys.Log.RotationMaxFiles = w.Int32(10)
	c.V1.Sys.Log.MaxErrorLogsPerSecond = w.Int32(1000)

	c.V1.Sys.Sql.Timeout = w.Int32(5000)
	c.V1.Sys.Sql.PoolInitSize = w.Int32(20)
	c.V1.Sys.Sql.PoolMaxSize = w.Int32(20)
	c.V1.Sys.Sql.PoolQueueMax = w.Int32(50)
	c.V1.Sys.Sql.PoolQueueTimeout = w.Int32(2000)

	c.V1.Sys.Bookshelf.SqlRetryCount = w.Int32(0)
	c.V1.Sys.Bookshelf.SqlRetryDelay = w.Int32(10)
	c.V1.Sys.Bookshelf.AbandonedUploadCleanupInterval = w.Int32(1140000)
	c.V1.Sys.Bookshelf.DeletedDataCleanupInterval = w.Int32(420000)
	c.V1.Sys.Bookshelf.StreamDownload = w.Bool(true)

	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	return nil
}

func (c *ConfigRequest) SetGlobalConfig(g *ac.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = logLevel
	}
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(creds *ac.TLSCredentials) (ac.PreparedSystemConfig, error) {
	c.V1.Sys.Tls = creds
	return c.V1.Sys, nil
}
