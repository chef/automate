package ocid

import (
	"fmt"
	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"regexp"
)

const regexValidURI = `^(http(s)?:\/\/)[\w.-]+(?:\.[\w\.-]+)+[\w\-\._~:/?#[\]@!\$&'\(\)\*\+,;=.]+$`

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Log:     &ConfigRequest_V1_System_Log{},
				Network: &ConfigRequest_V1_System_Network{},
				Sql:     &ConfigRequest_V1_System_Sql{},
				Ocid:    &ConfigRequest_V1_System_Ocid{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()

	c.V1.Sys.Network.Port = w.Int32(10114)
	c.V1.Sys.Network.ListenIp = w.String("127.0.0.1")

	c.V1.Sys.Log.Level = w.String("info")
	c.V1.Sys.Log.RotationMaxBytes = w.Int64(104857600)
	c.V1.Sys.Log.RotationMaxFiles = w.Int32(10)
	c.V1.Sys.Log.MaxErrorLogsPerSecond = w.Int32(1000)

	c.V1.Sys.Sql.Timeout = w.Int32(30000)
	c.V1.Sys.Sql.PoolInitSize = w.Int32(25)
	c.V1.Sys.Sql.PoolMaxSize = w.Int32(100)
	c.V1.Sys.Sql.PoolQueueMax = w.Int32(50)
	c.V1.Sys.Sql.PoolQueueTimeout = w.Int32(2000)

	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	cfgErr := ac.NewInvalidConfigError()
	oauthApplications := c.GetV1().GetSys().GetOcid().GetOauthApplicationConfig().GetOauthApplications()
	oauthApplicationsCount := len(oauthApplications)

	if oauthApplicationsCount < 1 {
		return nil
	}

	for i := 0; i < oauthApplicationsCount; i++ {
		appName := oauthApplications[i].GetName().GetValue()
		redirectURI := oauthApplications[i].GetRedirectUri().GetValue()

		// We need this condition to skip empty objects
		if appName == "" && redirectURI == "" {
			continue
		}

		// Validating App Name
		if appName == "" {
			cfgErr.AddMissingKey("Oauth Application Name")
		}

		// Validating App Redirect URI
		if redirectURI == "" {
			cfgErr.AddMissingKey("Oauth Application Redirect URI")
		} else {
			// Validating Redirect URI format
			validUrl, _ := regexp.MatchString(regexValidURI, redirectURI)
			if !validUrl {
				keyName := fmt.Sprintf("Redirect URI for App: %v", appName)
				cfgErr.AddInvalidValue(keyName, redirectURI)
			}
		}
	}

	if cfgErr.IsEmpty() {
		return nil
	}
	return cfgErr
}

func (c *ConfigRequest) SetGlobalConfig(g *ac.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = ac.GlobalLogLevelToZapLevel(logLevel)
	}
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(creds *ac.TLSCredentials) (ac.PreparedSystemConfig, error) {
	c.V1.Sys.Tls = creds
	return c.V1.Sys, nil
}
