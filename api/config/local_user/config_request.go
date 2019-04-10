package local_user

import (
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
				Logger:  &ConfigRequest_V1_System_Logger{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("0.0.0.0")
	c.V1.Sys.Service.Port = w.Int32(10127)
	c.V1.Sys.Logger.Format = w.String("text")
	c.V1.Sys.Logger.Level = w.String("info")
	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	cfgErr := config.NewInvalidConfigError()
	logLevel := c.GetV1().GetSys().GetLogger().GetLevel().GetValue()
	if logLevel != "" {
		err := config.ValidateZapLogLevel(logLevel)
		if err != nil {
			cfgErr.AddInvalidValue("local_user.v1.sys.log.level", err.Error())
		}
	}

	if cfgErr.IsEmpty() {
		return nil
	}
	return cfgErr
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(creds *config.TLSCredentials) (config.PreparedSystemConfig, error) {
	c.V1.Sys.Tls = creds
	return c.V1.Sys, nil
}

// SetGlobalConfig imports settings from the global configuration
func (c *ConfigRequest) SetGlobalConfig(g *config.GlobalConfig) {
	sys := c.V1.Sys
	sys.Mlsa = g.V1.Mlsa

	if g.V1.Fqdn != nil {
		sys.Service.ExternalFqdn = g.V1.Fqdn
	}

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		sys.Logger.Level.Value = config.GlobalLogLevelToZapLevel(logLevel)
	}

	if logFormat := g.GetV1().GetLog().GetFormat().GetValue(); logFormat != "" {
		sys.Logger.Format.Value = logFormat
	}
}
