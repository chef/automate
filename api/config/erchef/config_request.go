package erchef

import (
	"fmt"
	"strings"

	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Log:           &ConfigRequest_V1_System_Log{},
				Keygen:        &ConfigRequest_V1_System_Keygen{},
				Api:           &ConfigRequest_V1_System_ChefApi{},
				Index:         &ConfigRequest_V1_System_Indexing{},
				Network:       &ConfigRequest_V1_System_Network{},
				Sql:           &ConfigRequest_V1_System_Sql{},
				Authz:         &ConfigRequest_V1_System_Authz{},
				DataCollector: &ConfigRequest_V1_System_DataCollector{},
				Depsolver:     &ConfigRequest_V1_System_Depsolver{},
				Memory:        &ConfigRequest_V1_System_Memory{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()

	c.V1.Sys.Network.Port = w.Int32(10203)
	c.V1.Sys.Network.ListenIp = w.String("0.0.0.0")

	c.V1.Sys.Log.Level = w.String("info")
	c.V1.Sys.Log.RotationMaxBytes = w.Int64(104857600)
	c.V1.Sys.Log.RotationMaxFiles = w.Int32(10)
	c.V1.Sys.Log.MaxErrorLogsPerSecond = w.Int32(1000)

	c.V1.Sys.Api.AuthSkew = w.Int32(900)
	c.V1.Sys.Api.BulkFetchBatchSize = w.Int32(5)
	c.V1.Sys.Api.MaxRequestSize = w.Int32(1000000)
	c.V1.Sys.Api.BaseResourceUrl = w.String("host_header")
	c.V1.Sys.Api.StrictSearchResultAcls = w.Bool(false)

	c.V1.Sys.Keygen.WorkerCount = w.Int32(2)
	c.V1.Sys.Keygen.CacheSize = w.Int32(10)
	c.V1.Sys.Keygen.StartSize = w.Int32(2)
	c.V1.Sys.Keygen.Timeout = w.Int32(20000)

	c.V1.Sys.Index.Timeout = w.Int32(30000)
	c.V1.Sys.Index.PoolInitSize = w.Int32(25)
	c.V1.Sys.Index.PoolMaxSize = w.Int32(100)
	c.V1.Sys.Index.PoolQueueMax = w.Int32(50)
	c.V1.Sys.Index.BatchSize = w.Int32(5000000)
	c.V1.Sys.Index.BatchMaxWait = w.Int32(10)
	c.V1.Sys.Index.ReindexBatchSize = w.Int32(10)
	c.V1.Sys.Index.ReindexSleepMinMs = w.Int32(50)
	c.V1.Sys.Index.ReindexSleepMaxMs = w.Int32(2000)
	c.V1.Sys.Index.ReindexItemRetries = w.Int32(3)

	c.V1.Sys.Authz.Fanout = w.Int32(20)
	c.V1.Sys.Authz.Timeout = w.Int32(2000)
	c.V1.Sys.Authz.PoolInitSize = w.Int32(25)
	c.V1.Sys.Authz.PoolMaxSize = w.Int32(100)
	c.V1.Sys.Authz.PoolQueueMax = w.Int32(50)
	c.V1.Sys.Authz.PoolQueueTimeout = w.Int32(2000)

	c.V1.Sys.Sql.Timeout = w.Int32(5000)
	c.V1.Sys.Sql.PoolInitSize = w.Int32(20)
	c.V1.Sys.Sql.PoolMaxSize = w.Int32(50)
	c.V1.Sys.Sql.PoolQueueMax = w.Int32(20)
	c.V1.Sys.Sql.PoolQueueTimeout = w.Int32(2000)

	c.V1.Sys.DataCollector.Timeout = w.Int32(5000)
	c.V1.Sys.DataCollector.PoolInitSize = w.Int32(25)
	c.V1.Sys.DataCollector.PoolMaxSize = w.Int32(100)
	c.V1.Sys.DataCollector.PoolQueueMax = w.Int32(50)
	c.V1.Sys.DataCollector.Enabled = w.Bool(true)

	// TODO(ssd) 2018-07-24: We should auto-calculate this based
	// on CPU on the target.
	c.V1.Sys.Depsolver.Timeout = w.Int32(20000)
	c.V1.Sys.Depsolver.PoolInitSize = w.Int32(5)
	c.V1.Sys.Depsolver.PoolMaxSize = w.Int32(5)
	c.V1.Sys.Depsolver.PoolQueueMax = w.Int32(50)
	c.V1.Sys.Depsolver.PoolQueueTimeout = w.Int32(0)

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

	c.V1.Sys.ExternalDataCollector = g.GetV1().GetExternal().GetDataCollector()
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(creds *ac.TLSCredentials) (ac.PreparedSystemConfig, error) {
	if maxMegaBytes := c.GetV1().GetSys().GetLog().GetRotationMaxMegabytes().GetValue(); maxMegaBytes == 0 {
		rotationBytes := c.V1.Sys.Log.GetRotationMaxBytes().GetValue()
		rotationBytesInMB := (rotationBytes / 1024) / 1024
		c.V1.Sys.Log.RotationMaxMegabytes = w.Int32(int32(rotationBytesInMB))
	}

	// If the base resource url is "host_header" we need to render it as an atom
	// in the configuration file. If it's something else we'll render it as a
	// string.
	baseResourceUrl := c.GetV1().GetSys().GetApi().GetBaseResourceUrl().GetValue()
	if baseResourceUrl != "host_header" {
		if !strings.HasPrefix(baseResourceUrl, "\"") {
			baseResourceUrl = fmt.Sprintf("\"%s", baseResourceUrl)
		}
		if !strings.HasSuffix(baseResourceUrl, "\"") {
			baseResourceUrl = fmt.Sprintf("%s\"", baseResourceUrl)
		}

		c.V1.Sys.Api.BaseResourceUrl = w.String(baseResourceUrl)
	}

	c.V1.Sys.Tls = creds

	return c.V1.Sys, nil
}
