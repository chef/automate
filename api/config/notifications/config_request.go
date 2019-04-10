package notifications

import (
	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:      &config.Mlsa{},
				Tls:       &config.TLSCredentials{},
				Service:   &ConfigRequest_V1_System_Service{},
				Storage:   &ConfigRequest_V1_System_Storage{},
				Cache:     &ConfigRequest_V1_System_Cache{},
				Proxy:     &config.Proxy{},
				Migration: &ConfigRequest_V1_System_Migration{},
				Log:       &ConfigRequest_V1_System_Log{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("0.0.0.0")
	c.V1.Sys.Service.Port = w.Int32(10125)
	c.V1.Sys.Log.Level = w.String("info")
	return c
}

// Validate validates that the config is sufficient to start the Service and returns true.
func (c *ConfigRequest) Validate() error {
	// Right now the service should start without requiring user config.
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
	c.V1.Sys.Proxy = g.V1.Proxy
	c.V1.Sys.Service.ExternalFqdn = g.V1.Fqdn

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = logLevel
	}
}
