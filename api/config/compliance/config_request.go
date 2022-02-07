package compliance

import (
	"runtime"

	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:     &config.Mlsa{},
				Service:  &ConfigRequest_V1_System_Service{},
				Logger:   &ConfigRequest_V1_System_Logger{},
				Agent:    &ConfigRequest_V1_System_Agent{},
				Profiles: &ConfigRequest_V1_System_Profiles{},
				Proxy:    &ConfigRequest_V1_System_Proxy{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("127.0.0.1")
	c.V1.Sys.Service.Port = w.Int32(10121)
	c.V1.Sys.Logger.Level = w.String("info")
	c.V1.Sys.Logger.Format = w.String("text")
	c.V1.Sys.Agent.BufferSize = w.Int32(1000)
	c.V1.Sys.Agent.Workers = w.Int32(defaultWorkerCount())
	// Relying on default.toml to provide the value for RemoteInspecVersion
	// c.V1.Sys.Agent.RemoteInspecVersion = w.String("3.9.0")
	return c
}

func defaultWorkerCount() int32 {
	n := int32(runtime.NumCPU())
	if n < 2 {
		return 2
	}
	if n > 8 {
		return 10
	}

	return n + 2
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	err := config.NewInvalidConfigError()

	ret := c.GetV1().GetSys().GetRetention()
	if ret != nil {
		err.AddDeprecation(
			"compliance.v1.sys.retention",
			"Configure the retention data lifecycle with the chef.automate.domain.data_lifecycle.api.Purge gRPC interface",
		)
	}

	if err.IsEmpty() {
		return nil
	}

	return err
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
	c.V1.Sys.Proxy.ConnectionString = g.ProxyString()
	c.V1.Sys.Proxy.NoProxyString = g.NoProxyString()
	c.V1.Sys.Service.ExternalFqdn = g.V1.GetFqdn()

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Logger.Level.Value = logLevel
	}

	if logFormat := g.GetV1().GetLog().GetFormat().GetValue(); logFormat != "" {
		c.V1.Sys.Logger.Format.Value = logFormat
	}
}

func (c *ConfigRequest) ConfigureProduct(productConfig *config.ProductConfig) {
	c.V1.Sys.Profiles.InstallMarketProfiles = w.Bool(true)
	if len(productConfig.Products) > 0 {
		for _, product := range productConfig.Products {
			if product == "desktop" {
				c.V1.Sys.Profiles.InstallMarketProfiles = w.Bool(false)
				return
			}
		}
	}
}
