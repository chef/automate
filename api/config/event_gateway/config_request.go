package event_gateway

import (
	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:        &config.Mlsa{},
				Tls:         &config.TLSCredentials{},
				Service:     &ConfigRequest_V1_System_Service{},
				Log:         &ConfigRequest_V1_System_Log{},
				FrontendTls: []*config.FrontendTLSCredential{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("0.0.0.0")
	c.V1.Sys.Service.Port = w.Int32(4222)
	c.V1.Sys.Service.GatewayPort = w.Int32(10148)
	c.V1.Sys.Service.EnableNatsFeature = w.Bool(false)
	c.V1.Sys.Log.Level = w.String("info")
	c.V1.Sys.Log.Format = w.String("text")
	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	cfgErr := config.NewInvalidConfigError()

	if len(c.GetV1().GetSys().GetFrontendTls()) < 1 {
		// TODO: this needs to become required when EAS stuff goes GA
		//  cfgErr.AddMissingKey("event_gateway.v1.sys.frontend_tls")
	} else {
		for _, tls := range c.GetV1().GetSys().GetFrontendTls() {
			if tls.Cert == "" {
				cfgErr.AddMissingKey("event_gateway.v1.sys.frontend_tls.cert")
			}
			if tls.Key == "" {
				cfgErr.AddMissingKey("event_gateway.v1.sys.frontend_tls.key")
			}
			if tls.ServerName == "" {
				cfgErr.AddInvalidValue("event_gateway.v1.sys.frontend_tls.server_name", "server_name must be a valid FQDN")
			}
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
	sys := c.V1.Sys
	sys.Tls = creds
	return c.V1.Sys, nil
}

// SetGlobalConfig imports settings from the global configuration
func (c *ConfigRequest) SetGlobalConfig(g *config.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa

	// We expect certs to be set on the global config and not our per-service
	// config, so overwrite service with global
	if g.GetV1().GetFrontendTls() != nil {
		if len(g.GetV1().GetFrontendTls()) > 0 {
			c.V1.Sys.FrontendTls = g.GetV1().GetFrontendTls()
		}
	}

	// Fixup any certs that don't have the FQDN. This has to come after we copy
	// the certs over from global.
	if g.GetV1().GetFqdn() != nil {
		for _, cred := range c.GetV1().GetSys().GetFrontendTls() {
			// Override any missing or default server_name's with our global FQDN.
			if cred.GetServerName() == "localhost" || cred.GetServerName() == "" {
				cred.ServerName = g.GetV1().GetFqdn().Value
			}
		}
	}

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = logLevel
	}

	if logFormat := g.GetV1().GetLog().GetFormat().GetValue(); logFormat != "" {
		c.V1.Sys.Log.Format.Value = logFormat
	}
}
