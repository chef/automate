package compliance

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
				Service:   &ConfigRequest_V1_System_Service{},
				Logger:    &ConfigRequest_V1_System_Logger{},
				Retention: &ConfigRequest_V1_System_Retention{},
				Agent:     &ConfigRequest_V1_System_Agent{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("0.0.0.0")
	c.V1.Sys.Service.Port = w.Int32(10121)
	c.V1.Sys.Logger.Level = w.String("info")
	c.V1.Sys.Logger.Format = w.String("text")
	c.V1.Sys.Retention.ComplianceReportDays = w.Int32(60)
	c.V1.Sys.Agent.BufferSize = w.Int32(1000)
	c.V1.Sys.Agent.Workers = w.Int32(10)
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
	c.V1.Sys.Service.ExternalFqdn = g.V1.GetFqdn()

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Logger.Level.Value = logLevel
	}

	if logFormat := g.GetV1().GetLog().GetFormat().GetValue(); logFormat != "" {
		c.V1.Sys.Logger.Format.Value = logFormat
	}
}
