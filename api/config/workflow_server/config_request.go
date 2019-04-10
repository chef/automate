package workflow_server

import (
	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:       &ac.Mlsa{},
				Log:        &ConfigRequest_V1_System_Log{},
				SshGit:     &ConfigRequest_V1_SshGit{},
				Telemetry:  &ConfigRequest_V1_Telemetry{},
				ChefServer: &ConfigRequest_V1_ChefServer{},
				Postgresql: &ConfigRequest_V1_Postgresql{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Log.Level = w.String("info")

	// Delivery
	c.V1.Sys.Fqdn = w.String("127.0.0.1")
	c.V1.Sys.ApiPort = w.Int32(9611)
	c.V1.Sys.ApiProto = w.String("https")
	c.V1.Sys.TrustedCertificatesFile = w.String("root_ca.crt")

	// Delivery SSH Git Server
	c.V1.Sys.SshGit.HostAddress = w.String("0.0.0.0")
	c.V1.Sys.SshGit.Port = w.Int32(8989)
	c.V1.Sys.SshGit.GitRepoTemplate = w.String("git_repo_template")

	// A2 Workflow Database
	c.V1.Sys.Postgresql.DbName = w.String("chef_workflow")

	// chef-server
	c.V1.Sys.ChefServer.Url = w.String("https://localhost/organizations/changeme")
	c.V1.Sys.ChefServer.WebUiUrl = w.String("https://localhost")
	c.V1.Sys.ChefServer.Vip = w.String("127.0.0.1")
	c.V1.Sys.ChefServer.ChefUser = w.String("delivery")

	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	return nil
}

func (c *ConfigRequest) SetGlobalConfig(g *ac.GlobalConfig) {
	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level = w.String(logLevel)
	}
	c.V1.Sys.Fqdn = g.V1.Fqdn
	c.V1.Sys.Mlsa = g.V1.Mlsa
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(creds *ac.TLSCredentials) (ac.PreparedSystemConfig, error) {
	c.V1.Sys.Tls = creds

	return c.V1.Sys, nil
}
