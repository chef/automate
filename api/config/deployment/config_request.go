package deployment

import (
	"fmt"
	"strings"
	"time"

	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/lib/stringutils"
)

// NewConfigRequest returns a new ConfigRequest instance with zero values
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:       &config.Mlsa{},
				Service:    &ConfigRequest_V1_System_Service{},
				Log:        &ConfigRequest_V1_System_Log{},
				GatherLogs: &ConfigRequest_V1_System_GatherLogs{},
				Proxy:      &ConfigRequest_V1_System_Proxy{},
				Backup: &ConfigRequest_V1_System_Backup{
					Filesystem: &ConfigRequest_V1_System_Backup_Filesystem{},
				},
			},
			Svc: &ConfigRequest_V1_Service{
				AdminUser: &ConfigRequest_V1_AdminUser{},
			},
		},
	}
}

// DefaultConfigRequest returns a new ConfigRequest instance with default values
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Mlsa.Accept = w.Bool(true)
	c.V1.Sys.Service.ListenAddress = w.String("0.0.0.0")
	c.V1.Sys.Service.Port = w.Int32(10160)
	c.V1.Sys.Log.Level = w.String("info")

	c.V1.Svc.DeploymentType = w.String("local")
	c.V1.Svc.Channel = w.String("current")
	c.V1.Svc.UpgradeStrategy = w.String("at-once")
	c.V1.Svc.AdminUser.Username = w.String("admin")
	c.V1.Svc.AdminUser.Name = w.String("Local Administrator")
	c.V1.Svc.ManifestCacheExpiry = w.String("60m")
	c.V1.Svc.PackageCleanupMode = w.String("conservative")

	return c
}

// Validate validates that the config is sufficient to start the service and returns true.
func (c *ConfigRequest) Validate() error {
	err := config.NewInvalidConfigError()

	deploymentType := c.V1.Svc.DeploymentType
	if deploymentType == nil {
		err.AddMissingKey("deployment.v1.svc.deployment_type")
	} else if deploymentType.Value != "local" {
		err.AddInvalidValue("deployment.v1.svc.deployment_type", "The only supported deployment type is 'local'")
	}

	channel := c.V1.Svc.Channel
	if channel == nil {
		err.AddMissingKey("deployment.v1.svc.channel")
	} else {
		if valid, msg := validateChannel(channel.Value); !valid {
			err.AddInvalidValue("deployment.v1.svc.channel", msg)
		}
	}

	upgradeStrategy := c.V1.Svc.UpgradeStrategy
	if upgradeStrategy == nil {
		err.AddMissingKey("deployment.v1.svc.upgrade_strategy")
	} else {
		if valid, msg := validateUpgradeStrategy(upgradeStrategy.Value); !valid {
			err.AddInvalidValue("deployment.v1.svc.upgrade_strategy", msg)
		}
	}

	if v := c.V1.Svc.GetManifestCacheExpiry().GetValue(); v != "" {
		_, parseErr := time.ParseDuration(v)
		if parseErr != nil {
			err.AddInvalidValue("deployment.v1.svc.manifest_cache_expiry", parseErr.Error())
		}
	}

	packageCleanupMode := c.V1.Svc.PackageCleanupMode
	if packageCleanupMode != nil {
		if valid, msg := validatePackageCleanupMode(packageCleanupMode.Value); !valid {
			err.AddInvalidValue("deployment.v1.svc.package_cleanup_mode", msg)
		}
	}

	if err.IsEmpty() {
		return nil
	}

	return err
}

// ValidateCredentials checks that AdminUser credentials are present in the
// config. It's used when we initially configure a deployment to ensure we have
// the admin user name/username/password set. In other contexts, we do not need
// this configuration.
// TODO XXX UGH: This is here because we want to remove AdminUser from
// deployment's config, but we have thus far only implemented a workaround
// where we hide it from the user but we still need to have it in the config
// for the initial deployment. Therefore we need to have one code path for
// deployment and a different path for `chef-automate config set`. This is the
// code path for deployment.
func (c *ConfigRequest) ValidateCredentials() error {
	err := config.NewInvalidConfigError()

	adminUserName := c.V1.Svc.AdminUser.Name
	if adminUserName == nil {
		err.AddMissingKey("deployment.v1.svc.admin_user.name")
	}

	adminUserPassword := c.V1.Svc.AdminUser.Password
	if adminUserPassword == nil {
		err.AddMissingKey("deployment.v1.svc.admin_user.password")
	}

	adminUserUsername := c.V1.Svc.AdminUser.Username
	if adminUserUsername == nil {
		err.AddMissingKey("deployment.v1.svc.admin_user.username")
	}

	if err.IsEmpty() {
		return nil
	}

	return err
}

func validateChannel(channel string) (bool, string) {
	return validateOneOf("Channel", channel, []string{"current", "acceptance", "dev"})
}

func validateUpgradeStrategy(strategy string) (bool, string) {
	return validateOneOf("Upgrade strategy", strategy, []string{"none", "at-once"})
}

func validatePackageCleanupMode(mode string) (bool, string) {
	return validateOneOf("Package cleanup mode", mode, []string{"conservative", "aggressive", "disabled"})
}

func validateOneOf(msgPrefix string, input string, allowedValues []string) (bool, string) {
	valid := stringutils.SliceContains(allowedValues, input)
	if !valid {
		return valid, msgPrefix + " must be one of: " + strings.Join(allowedValues, ", ")
	}
	return valid, ""
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(*config.TLSCredentials) (config.PreparedSystemConfig, error) {
	return c.V1.Sys, nil
}

// SetGlobalConfig imports settings from the global configuration
func (c *ConfigRequest) SetGlobalConfig(g *config.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa
	c.V1.Sys.Proxy.ConnectionString = g.ProxyString()
	c.V1.Sys.Proxy.NoProxyString = g.NoProxyString()
	c.V1.Sys.Backup.Filesystem.Path = g.GetV1().GetBackups().GetFilesystem().GetPath()

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = logLevel
	}
}

// SystemdProxyConfig returns the proxy configuration in a format that
// can be directly injected into a systemd unit file.
func (c *ConfigRequest) SystemdProxyConfig() string {
	extraConfig := strings.Builder{}
	proxyConfig := c.GetV1().GetSys().GetProxy()

	proxyConnectionString := proxyConfig.GetConnectionString().GetValue()
	if proxyConnectionString != "" {
		extraConfig.WriteString(fmt.Sprintf("Environment = \"https_proxy=%s\"\n", proxyConnectionString))
	}

	noProxyString := proxyConfig.GetNoProxyString().GetValue()
	if noProxyString != "" {
		extraConfig.WriteString(fmt.Sprintf("Environment = \"no_proxy=%s\"\n", noProxyString))
	}
	return extraConfig.String()
}
