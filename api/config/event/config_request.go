package event

import (
	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:              &config.Mlsa{},
				Tls:               &config.TLSCredentials{},
				Service:           &ConfigRequest_V1_System_Service{},
				Log:               &ConfigRequest_V1_System_Log{},
				Handlers:          &ConfigRequest_V1_System_Handlers{},
				InternalMessaging: &ConfigRequest_V1_System_InternalMessaging{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("0.0.0.0")
	c.V1.Sys.Service.Port = w.Int32(10132)
	c.V1.Sys.Service.EventLimit = w.Int32(100000)
	c.V1.Sys.Service.ListenerLimit = w.Int32(100000)
	c.V1.Sys.InternalMessaging.Port = w.Int32(10140)
	c.V1.Sys.InternalMessaging.GatewayPort = w.Int32(10147)
	c.V1.Sys.Log.Level = w.String("info")
	c.V1.Sys.Log.Format = w.String("text")
	c.V1.Sys.Handlers.Feed = w.String("0.0.0.0:10121")
	c.V1.Sys.Handlers.Cfgingest = w.String("0.0.0.0:10122")
	c.V1.Sys.Handlers.Authz = w.String("0.0.0.0:10130")
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

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = logLevel
	}

	if logFormat := g.GetV1().GetLog().GetFormat().GetValue(); logFormat != "" {
		c.V1.Sys.Log.Format.Value = logFormat
	}
}
