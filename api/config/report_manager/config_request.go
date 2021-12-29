package report_manager

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
				Log:     &config.Log{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("127.0.0.1")
	c.V1.Sys.Service.Port = w.Int32(10152)

	c.V1.Sys.Log.Level = w.String("info")
	c.V1.Sys.Log.Format = w.String("text")
	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	err := config.NewInvalidConfigError()

	if c.GetV1().GetSys().GetService().GetEnableLargeReporting().GetValue() {
		if c.GetV1().GetSys().GetMinio().GetEndpoint().GetValue() == "" {
			err.AddInvalidValue("global.v1.external.minio.endpoint", "value should not be empty")
		}
		if c.GetV1().GetSys().GetMinio().GetRootUser().GetValue() == "" {
			err.AddInvalidValue("global.v1.external.minio.root_user", "value should not be empty")
		}
		if c.GetV1().GetSys().GetMinio().GetRootPassword().GetValue() == "" {
			err.AddInvalidValue("global.v1.external.minio.root_password", "value should not be empty")
		}
	}

	if err.IsEmpty() {
		return nil
	}

	return err
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

	if largeReporting := g.GetV1().GetLargeReporting().GetEnableLargeReporting(); largeReporting != nil {
		c.V1.Sys.Service.EnableLargeReporting = largeReporting
	}

	if minio := g.GetV1().GetExternal().GetMinio(); minio != nil {
		c.V1.Sys.Minio = &ConfigRequest_V1_System_Minio{
			Endpoint:     minio.GetEndpoint(),
			RootUser:     minio.GetRootUser(),
			RootPassword: minio.GetRootPassword(),
		}
	}
}
