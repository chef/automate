package builder_api

import (
	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Service: &ConfigRequest_V1_System_Service{},
				Log:     &ConfigRequest_V1_System_Log{},
			},
		},
	}
}

func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("127.0.0.1")
	c.V1.Sys.Service.Port = w.Int32(10103)
	return c
}

func (c *ConfigRequest) Validate() error {
	return nil
}

func (c *ConfigRequest) PrepareSystemConfig(creds *ac.TLSCredentials) (ac.PreparedSystemConfig, error) {
	sys := c.V1.Sys
	sys.Tls = creds
	return c.V1.Sys, nil
}

// SetGlobalConfig imports settings from the global configuration
func (c *ConfigRequest) SetGlobalConfig(g *ac.GlobalConfig) {
	var level string
	if c.GetV1().GetSys().GetLog().GetLevel().GetValue() != "" {
		level = c.GetV1().GetSys().GetLog().GetLevel().GetValue()
	} else {
		level = g.GetV1().GetLog().GetLevel().GetValue()
	}
	switch level {
	case "debug":
		level = "debug"
	case "", "info", "warn", "warning", "panic", "fatal":
		level = "error"
	default:
		// We'll allow it (this will cover trace)
	}
	if level == "" {
		c.GetV1().GetSys().GetLog().Level = nil
	} else {
		c.GetV1().GetSys().GetLog().Level = w.String(level)
	}
	if len(c.GetV1().GetSys().GetLog().GetScopedLevels()) == 0 {
		c.GetV1().GetSys().GetLog().ScopedLevels = nil
	}
}
