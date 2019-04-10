package ui

import (
	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:    &ac.Mlsa{},
				Service: &ConfigRequest_V1_System_Service{},
				Log:     &ConfigRequest_V1_System_Log{},
				Ngx: &ConfigRequest_V1_System_Nginx{
					Http: &ConfigRequest_V1_System_Nginx_Http{},
				},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("0.0.0.0")
	c.V1.Sys.Service.Port = w.Int32(10161)

	c.V1.Sys.Log.Level = w.String("error")

	c.V1.Sys.Ngx.Http.ClientBodyBufferSize = w.String("16k")

	c.V1.Sys.Ngx.Http.SslCiphers = w.String(ac.InternalCipherSuite)
	c.V1.Sys.Ngx.Http.SslProtocols = w.String("TLSv1.2")
	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	cfgErr := ac.NewInvalidConfigError()
	logLevel := c.GetV1().GetSys().GetLog().GetLevel().GetValue()
	if logLevel != "" {
		err := ac.ValidateNginxLogLevel(logLevel)
		if err != nil {
			cfgErr.AddInvalidValue("u_i.v1.sys.log.level", err.Error())
		}
	}

	if cfgErr.IsEmpty() {
		return nil
	}
	return cfgErr
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(creds *ac.TLSCredentials) (ac.PreparedSystemConfig, error) {
	sys := c.V1.Sys
	sys.Tls = creds
	return c.V1.Sys, nil
}

// SetGlobalConfig imports settings from the global configuration
func (c *ConfigRequest) SetGlobalConfig(g *ac.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = ac.GlobalLogLevelToNginxLevel(logLevel)
	}
}
