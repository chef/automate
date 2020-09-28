package ingest

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
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.Host = w.String("127.0.0.1")
	c.V1.Sys.Service.Port = w.Int32(10122)
	c.V1.Sys.Service.MaxNumberOfBundledRunMsgs = w.Int32(2500)
	c.V1.Sys.Service.MaxNumberOfBundledActionMsgs = w.Int32(10000)
	c.V1.Sys.Service.NumberOfRunMsgsTransformers = w.Int32(9)
	c.V1.Sys.Service.NumberOfRunMsgPublishers = w.Int32(2)

	c.V1.Sys.Log.Level = w.String("info")
	c.V1.Sys.Log.Format = w.String("text")
	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	err := ac.NewInvalidConfigError()

	if c.GetV1().GetSys().GetService().GetPurgeConvergeHistoryAfterDays() != nil {
		err.AddDeprecation(
			"ingest.v1.sys.service.purge_converge_history_after_days",
			"Configure the converge history data lifecycle with the chef.automate.domain.data_lifecycle.api.Purge gRPC interface",
		)
	}

	if c.GetV1().GetSys().GetService().GetPurgeActionsAfterDays() != nil {
		err.AddDeprecation(
			"ingest.v1.sys.service.purge_actions_after_days",
			"Configure the converge history data lifecycle with the chef.automate.domain.data_lifecycle.api.Purge gRPC interface",
		)
	}

	if err.IsEmpty() {
		return nil
	}

	return err
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
		c.V1.Sys.Log.Level.Value = logLevel
	}

	if logFormat := g.GetV1().GetLog().GetFormat().GetValue(); logFormat != "" {
		c.V1.Sys.Log.Format.Value = logFormat
	}
}

func (c *ConfigRequest) ConfigureProduct(productConfig *ac.ProductConfig) {
	c.V1.Sys.Service.NodesMissingRunningDefault = w.Bool(true)
	c.V1.Sys.Service.MissingNodesForDeletionRunningDefault = w.Bool(true)
	if len(productConfig.Products) > 0 {
		for _, product := range productConfig.Products {
			if product == "desktop" {
				c.V1.Sys.Service.NodesMissingRunningDefault = w.Bool(false)
				c.V1.Sys.Service.MissingNodesForDeletionRunningDefault = w.Bool(false)
				return
			}
		}
	}
}
