package authz

import (
	fmt "fmt"

	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	constants "github.com/chef/automate/components/authz-service/constants"
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
				Storage: &ConfigRequest_V1_System_Storage{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()

	c.V1.Sys.Service.Host = w.String("127.0.0.1")
	c.V1.Sys.Service.Port = w.Int32(10130)
	c.V1.Sys.Service.ProjectLimit = w.Int32(constants.DefaultProjectLimit)
	c.V1.Sys.Logger.Level = w.String("info")
	c.V1.Sys.Logger.Format = w.String("text")
	c.V1.Sys.Storage.Database = w.String("chef_authz_service")
	c.V1.Sys.Storage.User = w.String("authz")
	c.V1.Sys.Storage.MaxConnections = w.Int32(10)
	c.V1.Sys.Storage.MaxIdleConnections = w.Int32(10)

	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	cfgErr := config.NewInvalidConfigError()
	projectLimit := c.GetV1().GetSys().GetService().GetProjectLimit()

	if projectLimit != nil {
		if limit := projectLimit.GetValue(); limit < constants.MinConfigurableProjects {
			// Previously, users could not configure their project_limit to be below the
			// default.
			// MinConfigurableProjects supports customers who previously
			// increased their limit to a number that is lower than the new default.
			// It should be removed when we no longer limit projects.
			failureStr := fmt.Sprintf("project limit must be at least %d", constants.MinConfigurableProjects)
			cfgErr.AddInvalidValue("auth_z.v1.sys.service.project_limit", failureStr)
		}
	}

	if cfgErr.IsEmpty() {
		return nil
	}
	return cfgErr
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(certificate *config.TLSCredentials) (config.PreparedSystemConfig, error) {
	c.V1.Sys.Tls = certificate
	return c.V1.Sys, nil
}

// SetGlobalConfig imports settings from the global configuration
func (c *ConfigRequest) SetGlobalConfig(g *config.GlobalConfig) {
	c.V1.Sys.Mlsa = g.GetV1().Mlsa

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Logger.Level.Value = logLevel
	}

	if logFormat := g.GetV1().GetLog().GetFormat().GetValue(); logFormat != "" {
		c.V1.Sys.Logger.Format.Value = logFormat
	}
}
