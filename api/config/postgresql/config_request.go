package pg

import (
	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Service:   &ConfigRequest_V1_System_Service{},
				Logger:    &ConfigRequest_V1_System_Logger{},
				Pg:        NewPGConfig(),
				Superuser: &ConfigRequest_V1_System_Superuser{},
				Tls:       &config.TLSCredentials{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Pg = DefaultPGConfig()
	c.V1.Sys.Logger.Level = w.String("ERROR")
	c.V1.Sys.Service.Port = w.Int32(5432)
	c.V1.Sys.Service.Host = w.String("0.0.0.0")
	c.V1.Sys.Superuser.Name = w.String("automate")

	return c
}

// NewPGConfig returns a new instance of ConfigRequest_PGConfig with zero values.
func NewPGConfig() *ConfigRequest_V1_System_PGConfig {
	return &ConfigRequest_V1_System_PGConfig{}
}

// DefaultPGConfig returns a new instance of ConfigRequest_PGConfig with default
// values.
func DefaultPGConfig() *ConfigRequest_V1_System_PGConfig {
	c := NewPGConfig()
	c.MaxWalSize = w.String("1GB")
	c.MinWalSize = w.String("80MB")
	c.WalKeepSegments = w.Int32(32)
	c.CheckpointTimeout = w.String("5min")
	c.CheckpointCompletionTarget = w.Float(0.5)
	c.MaxConnections = w.Int32(100)
	c.MaxLocksPerTransaction = w.Int32(64)
	// We use a very open default since we require MD5 auth and
	// the plan is to also require SSL eventually. Our experience
	// with other products is that any other default won't work
	// once we move to multi-machine setups since our users often
	// don't know or control their network topology.
	c.Md5AuthCidrAddresses = []string{"0.0.0.0/0", "::0/0"}
	// We use the ExternalCipherSuite to be a bit more permissive
	// to external monitoring tools.
	c.SslCiphers = w.String(config.ExternalCipherSuite)
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
	return sys, nil

}

// SetGlobalConfig imports settings from the global configuration
func (c *ConfigRequest) SetGlobalConfig(g *config.GlobalConfig) {
	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Logger.Level.Value = logLevel
	}
	if g.GetV1().GetExternal().GetPostgresql().GetEnable().GetValue() {
		c.V1.Sys.Disable = w.Bool(true)
	}
}
