package session

import (
	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

var _ config.PlatformServiceConfigurable = (*ConfigRequest)(nil)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:    &config.Mlsa{},
				Service: &ConfigRequest_V1_System_Service{},
				Oidc:    &ConfigRequest_V1_System_Oidc{},
				Storage: &ConfigRequest_V1_System_Storage{},
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
	c.V1.Sys.Service.Port = w.Int32(10115)
	c.V1.Sys.Logger.Level = w.String("info")
	c.V1.Sys.Logger.Format = w.String("text")

	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	cfgErr := config.NewInvalidConfigError()

	if c.V1.Sys.Service.ExternalFqdn == nil {
		cfgErr.AddMissingKey("session.v1.sys.service.external_fqdn")
	}

	bldrSignInURL := c.V1.Sys.Service.BldrSigninUrl.GetValue()
	bldrClientID := c.V1.Sys.Service.BldrClientId.GetValue()
	bldrClientSecret := c.V1.Sys.Service.BldrClientSecret.GetValue()
	if IfEmptyButOthersAreNot(bldrSignInURL, []string{bldrClientID, bldrClientSecret}) {
		cfgErr.AddMissingKey("session.v1.sys.service.bldr_signin_url")
	}

	if IfEmptyButOthersAreNot(bldrClientID, []string{bldrSignInURL, bldrClientSecret}) {
		cfgErr.AddMissingKey("session.v1.sys.service.bldr_client_id")
	}

	if IfEmptyButOthersAreNot(bldrClientSecret, []string{bldrSignInURL, bldrClientID}) {
		cfgErr.AddMissingKey("session.v1.sys.service.bldr_client_secret")
	}

	if cfgErr.IsEmpty() {
		return nil
	}

	return cfgErr
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(certificate *config.TLSCredentials) (config.PreparedSystemConfig, error) {
	sys := c.V1.Sys
	sys.Tls = certificate
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
		sys.Logger.Level.Value = logLevel
	}

	if logFormat := g.GetV1().GetLog().GetFormat().GetValue(); logFormat != "" {
		sys.Logger.Format.Value = logFormat
	}
}

// IfEmptyButOthersAreNot returns false if the stringToCheck is not empty
// but true if it is not empty but any of the others are not.
func IfEmptyButOthersAreNot(stringToCheck string, others []string) bool {
	if stringToCheck != "" {
		return false
	}
	for _, other := range others {
		if other != "" {
			return true
		}
	}
	return false
}
