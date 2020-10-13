package data_feed

import (
	"github.com/golang/protobuf/ptypes/wrappers"

	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:    &config.Mlsa{},
				Tls:     &config.TLSCredentials{},
				Service: &ConfigRequest_V1_System_Service{},
				Log:     &ConfigRequest_V1_System_Log{},
			},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("127.0.0.1")
	c.V1.Sys.Service.Port = w.Int32(10105)
	c.V1.Sys.Service.FeedInterval = w.String("4h")
	c.V1.Sys.Service.AssetPageSize = w.Int32(100)
	c.V1.Sys.Service.ReportsPageSize = w.Int32(1000)
	c.V1.Sys.Service.NodeBatchSize = w.Int32(50)
	c.V1.Sys.Service.UpdatedNodesOnly = w.Bool(true)
	c.V1.Sys.Service.DisableCidrFilter = w.Bool(true)
	c.V1.Sys.Service.CidrFilter = w.String("0.0.0.0/0")
	c.V1.Sys.Service.AcceptedStatusCodes = []*wrappers.Int32Value{w.Int32(200), w.Int32(201), w.Int32(202), w.Int32(203), w.Int32(204)}
	c.V1.Sys.Service.ContentType = w.String("application/json")
	c.V1.Sys.Log.Level = w.String("info")
	c.V1.Sys.Log.Format = w.String("text")
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
func (c *ConfigRequest) PrepareSystemConfig(creds *config.TLSCredentials) (config.PreparedSystemConfig, error) {
	sys := c.V1.Sys
	sys.Tls = creds
	return c.V1.Sys, nil
}

// SetGlobalConfig imports settings from the global configuration
func (c *ConfigRequest) SetGlobalConfig(g *config.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa

	if g.GetV1().Fqdn != nil {
		c.V1.Sys.Service.ExternalFqdn = g.GetV1().Fqdn
	}

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = logLevel
	}

	if logFormat := g.GetV1().GetLog().GetFormat().GetValue(); logFormat != "" {
		c.V1.Sys.Log.Format.Value = logFormat
	}
}
