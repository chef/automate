package license_control

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
				Telemetry: &ConfigRequest_V1_System_Telemetry{},
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
	c.V1.Sys.Service.Port = w.Int32(10124)
	// do not default the OptOut field because it is need to be blank for migration
	// c.V1.Sys.Telemetry.OptOut = w.Bool(true)
	c.V1.Sys.Telemetry.Url = w.String("https://telemetry.chef.io")
	c.V1.Sys.Log.Level = w.String("info")
	c.V1.Sys.Log.Format = w.String("text")
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
	c.V1.Sys.Tls = creds
	return c.V1.Sys, nil
}

// SetGlobalConfig imports settings from the global configuration
func (c *ConfigRequest) SetGlobalConfig(g *config.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = logLevel
	}

	if logFormat := g.GetV1().GetLog().GetFormat().GetValue(); logFormat != "" {
		c.V1.Sys.Log.Format.Value = logFormat
	}
}
