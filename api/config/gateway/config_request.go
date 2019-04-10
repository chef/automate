package gateway

import (
	"fmt"

	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/lib/stringutils"
)

// ValidTrialLicenseURLs lists the usable trial license URLs
var ValidTrialLicenseURLs = [...]string{
	"https://licensing.chef.io/create-trial",
	"https://licensing-acceptance.chef.io/create-trial",
	"https://licensing-dev.chef.io/create-trial",
}

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa: &ac.Mlsa{},
				Tls:  &ac.TLSCredentials{},
				Service: &ConfigRequest_V1_System_Service{
					Log: &ConfigRequest_V1_System_Log{}, // backwards compat with old log level config
				},
				Log: &ConfigRequest_V1_System_Log{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()

	c.V1.Sys.Service.Host = w.String("0.0.0.0")
	c.V1.Sys.Service.Port = w.Int32(2000)
	c.V1.Sys.Service.GrpcPort = w.Int32(2001)
	c.V1.Sys.Service.TrialLicenseUrl = w.String(ValidTrialLicenseURLs[0])
	c.V1.Sys.Service.Log.Level = w.String("info")      // backwards compat with old log level config
	c.V1.Sys.Service.EnableAppsFeature = w.Bool(false) // explicitly disable experimental feature
	//c.V1.Sys.Log.Level = w.String("info")
	//c.V1.Sys.Log.Format = w.String("text")

	return c
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(certificate *ac.TLSCredentials) (ac.PreparedSystemConfig, error) {
	c.V1.Sys.Tls = certificate

	return c.V1.Sys, nil
}

// SetGlobalConfig imports settings from the global configuration
func (c *ConfigRequest) SetGlobalConfig(g *ac.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa

	if g.V1.Fqdn != nil {
		c.V1.Sys.Service.ExternalFqdn = g.V1.Fqdn
	}

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level = w.String(logLevel)
	}

	if logFormat := g.GetV1().GetLog().GetFormat().GetValue(); logFormat != "" {
		c.V1.Sys.Log.Format = w.String(logFormat)
	}
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	cfgErr := ac.NewInvalidConfigError()

	if c.V1.Sys.Service.TrialLicenseUrl != nil {
		url := c.V1.Sys.Service.TrialLicenseUrl.Value
		valid := stringutils.SliceContains(ValidTrialLicenseURLs[:], url)
		if !valid {
			cfgErr.AddInvalidValue("gateway.v1.sys.service.trial_license_url", fmt.Sprintf("url must be one of %v", ValidTrialLicenseURLs))
		}
	}

	// Give preference to the new config
	lvl := c.V1.GetSys().GetLog().GetLevel()
	if lvl == nil {
		// backwards compat with old log level config
		lvl = c.V1.GetSys().GetService().GetLog().GetLevel()
	}

	if lvl != nil {
		err := ac.ValidateLogrusLogLevel(lvl.Value)
		if err != nil {
			cfgErr.AddInvalidValue("gateway.v1.sys.log.level", err.Error())
		}
	}

	if cfgErr.IsEmpty() {
		return nil
	}

	return cfgErr
}
