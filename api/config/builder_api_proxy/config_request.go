package bldrapiproxy

import (
	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new ConfigRequest instance with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:    &config.Mlsa{},
				Service: &ConfigRequest_V1_System_Service{},
				Log:     &ConfigRequest_V1_System_Logger{},
				Http:    &ConfigRequest_V1_System_HTTP{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new ConfigRequest instance with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("127.0.0.1")
	c.V1.Sys.Service.Port = w.Int32(10104)
	c.V1.Sys.Log.Level = w.String("error")

	c.V1.Sys.Http.SslProtocols = w.String("TLSv1.2 TLSv1.3")
	c.V1.Sys.Http.SslCiphers = w.String(config.InternalCipherSuite)
	c.V1.Sys.Http.SslVerifyDepth = w.Int32(2)
	return c
}

// ValidateConfigRequest validates that the config is sufficient to start the
// Service and returns true.
func (c *ConfigRequest) Validate() error {
	cfgErr := config.NewInvalidConfigError()

	logLevel := c.GetV1().GetSys().GetLog().GetLevel().GetValue()
	if logLevel != "" {
		err := config.ValidateNginxLogLevel(logLevel)
		if err != nil {
			cfgErr.AddInvalidValue("builder_api_proxy.v1.sys.log.level", err.Error())
		}
	}

	if cfgErr.IsEmpty() {
		return nil
	}
	return cfgErr
}

// SetGlobalConfig takes a pointer to a config.GlobalConfig and applies any
// global configuration that the service requires.
func (c *ConfigRequest) SetGlobalConfig(g *config.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = config.GlobalLogLevelToNginxLevel(logLevel)
	}
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(certificate *config.TLSCredentials) (config.PreparedSystemConfig, error) {
	c.V1.Sys.Tls = certificate
	return c.V1.Sys, nil
}
