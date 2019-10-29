package deployment

import (
	"reflect"
	"strings"
	"testing"

	"github.com/chef/automate/api/config/compliance"
	es "github.com/chef/automate/api/config/elasticsearch"
	config "github.com/chef/automate/api/config/shared"
	"github.com/chef/automate/components/automate-grpc/protoc-gen-a2-config/api/a2conf"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/components/automate-deployment/pkg/toml"
)

func TestValidateInvalid(t *testing.T) {
	invalidCfg := NewAutomateConfig()
	expected := config.NewInvalidConfigError()
	for _, k := range []string{
		"global.v1.fqdn",
		"deployment.v1.svc.deployment_type",
		"deployment.v1.svc.channel",
		"deployment.v1.svc.upgrade_strategy",
		"load_balancer.v1.sys.service.external_fqdn",
		"load_balancer.v1.sys.frontend_tls",
		"session.v1.sys.service.external_fqdn",
	} {
		expected.AddMissingKey(k)
	}

	assert.EqualError(t, invalidCfg.Validate(), expected.Error(), "")
}

func TestValidateWithCredsInvalid(t *testing.T) {
	invalidCfg := NewAutomateConfig()

	invalidCfg.Global.V1.Fqdn = w.String("fqdn")
	invalidCfg.Global.V1.FrontendTls = validTLSCredentialSliceForTest()
	invalidCfg.Deployment.V1.Svc.DeploymentType = w.String("local")
	invalidCfg.Deployment.V1.Svc.Channel = w.String("current")
	invalidCfg.Deployment.V1.Svc.UpgradeStrategy = w.String("at-once")
	invalidCfg.SetGlobalConfig()

	expected := config.NewInvalidConfigError()
	for _, k := range []string{
		"deployment.v1.svc.admin_user.password",
	} {
		expected.AddMissingKey(k)
	}

	assert.EqualError(t, invalidCfg.ValidateWithGlobalAndDefaultsAndCredentials(), expected.Error(), "")
}

func TestValidateValid(t *testing.T) {
	t.Run("With AdminUser email and username", func(t *testing.T) {
		c := DefaultAutomateConfig()

		c.Global.V1.Fqdn = w.String("fqdn")
		c.Global.V1.FrontendTls = validTLSCredentialSliceForTest()
		c.Deployment.V1.Svc.DeploymentType = w.String("local")
		c.Deployment.V1.Svc.Channel = w.String("current")
		c.Deployment.V1.Svc.UpgradeStrategy = w.String("at-once")
		c.Deployment.V1.Svc.AdminUser.Email = w.String("cowboy@chef.io")
		c.Deployment.V1.Svc.AdminUser.Username = w.String("cowboy")
		c.Deployment.V1.Svc.AdminUser.Password = w.String("ponies")
		c.SetGlobalConfig()

		assert.Nil(t, c.Validate())
	})

	t.Run("With AdminUser name and username", func(t *testing.T) {
		c := DefaultAutomateConfig()

		c.Global.V1.Fqdn = w.String("fqdn")
		c.Global.V1.FrontendTls = validTLSCredentialSliceForTest()
		c.Deployment.V1.Svc.DeploymentType = w.String("local")
		c.Deployment.V1.Svc.Channel = w.String("current")
		c.Deployment.V1.Svc.UpgradeStrategy = w.String("at-once")
		c.Deployment.V1.Svc.AdminUser.Name = w.String("Cowboy")
		c.Deployment.V1.Svc.AdminUser.Username = w.String("cowboy")
		c.Deployment.V1.Svc.AdminUser.Password = w.String("ponies")
		c.SetGlobalConfig()

		assert.Nil(t, c.Validate())
	})
}

func TestInitConfigRenderValid(t *testing.T) {
	c := NewInitConfig()
	c.Channel = "current"
	c.DeploymentType = "local"
	c.UpgradeStrategy = "at-once"
	c.Fqdn = "localhost"
	c.FrontendCert = "-----BEGIN CERTIFICATE-----\nEND CERTIFICATE"
	c.FrontendKey = "-----BEGIN RSA PRIVATE KEY-----"
	c.ProxyHost = "proxy.example.com"
	c.ProxyPort = 3128
	c.ProxyUser = "chef-automate"
	c.ProxyPassword = "helloworld"

	r, err := c.Render()
	assert.Nil(t, err)

	ac := NewAutomateConfig()
	err = toml.Unmarshal([]byte(r), ac)
	assert.Nil(t, err)

	ac.AddCredentials("Local Administrator", "admin", "chefautomate")

	ac.SetGlobalConfig()
	err = ac.Validate()
	assert.Nil(t, err)
}

func TestInitConfigRenderInvalid(t *testing.T) {
	c := NewInitConfig()
	assert := assert.New(t)

	r, err := c.Render()
	assert.Nil(err)

	ac := NewAutomateConfig()
	err = toml.Unmarshal([]byte(r), ac)
	assert.Nil(err)

	expected := config.NewInvalidConfigError()
	for _, k := range []string{
		"load_balancer.v1.sys.service.external_fqdn",
		"load_balancer.v1.sys.frontend_tls",
	} {
		expected.AddMissingKey(k)
	}

	expected.AddInvalidValue("deployment.v1.svc.upgrade_strategy", "Upgrade strategy must be one of: none, at-once")
	expected.AddInvalidValue("deployment.v1.svc.deployment_type", "The only supported deployment type is 'local'")
	expected.AddInvalidValue("deployment.v1.svc.channel", "Channel must be one of: current, acceptance, unstable")

	actual := ac.Validate()
	msg := actual.Error()
	for _, m := range []string{
		"load_balancer.v1.sys.service.external_fqdn",
		"load_balancer.v1.sys.frontend_tls",
		"deployment.v1.svc.upgrade_strategy",
		"deployment.v1.svc.deployment_type",
		"deployment.v1.svc.channel",
	} {
		assert.Contains(msg, m)
	}
}

func TestNewUserOverrideConfigFromTOML(t *testing.T) {
	t.Run("test valid TOML",
		func(t *testing.T) {
			toml := `
[deployment.v1]
  [deployment.v1.svc]
    hartifacts_path = "/tmp/override"
    override_origin = "bananas"`
			assert := assert.New(t)
			cfg, err := NewUserOverrideConfigFromTOML([]byte(toml))
			assert.Nil(err)
			assert.Equal("/tmp/override", cfg.Deployment.V1.Svc.HartifactsPath.Value)
			assert.Equal("bananas", cfg.Deployment.V1.Svc.OverrideOrigin.Value)

		})
	t.Run("test invalid TOML",
		func(t *testing.T) {
			toml := `
---
  this_is_yaml:
    - not
	- toml`
			cfg, err := NewUserOverrideConfigFromTOML([]byte(toml))
			assert.Nil(t, cfg)
			assert.Error(t, err)
		})

	t.Run("test valid TOML with unknown key",
		func(t *testing.T) {
			toml := `
[deployment.v1]
  [deployment.v1.svc]
    hartifacts_path = "/tmp/override"
    unknown_key = "bananas"`
			_, err := NewUserOverrideConfigFromTOML([]byte(toml))
			assert.Error(t, err)

		})
}

func TestMergeWithDefaults(t *testing.T) {
	defaultConfig := DefaultAutomateConfig()
	overrideConfig := &AutomateConfig{
		Deployment: &ConfigRequest{
			V1: &ConfigRequest_V1{
				Svc: &ConfigRequest_V1_Service{
					Channel: w.String("foo"),
				},
			},
		},
	}
	mergedConfig, err := MergeWithDefaults(overrideConfig)
	assert := assert.New(t)
	assert.NoError(err)

	// Make sure our defaults are correct
	assert.Equal("current", defaultConfig.Deployment.V1.Svc.Channel.Value)
	assert.Equal("local", defaultConfig.Deployment.V1.Svc.DeploymentType.Value)

	// Make sure our merged config copy is correct
	assert.Equal("foo", mergedConfig.Deployment.V1.Svc.Channel.Value)
	assert.Equal("local", mergedConfig.Deployment.V1.Svc.DeploymentType.Value)

	// Make sure our passed override config wasn't changed
	assert.Equal("foo", overrideConfig.Deployment.V1.Svc.Channel.Value)
	assert.Nil(overrideConfig.Deployment.V1.Svc.DeploymentType)
}

func TestOverrideConfigValues(t *testing.T) {
	userConfig := &AutomateConfig{
		Deployment: &ConfigRequest{
			V1: &ConfigRequest_V1{
				Svc: &ConfigRequest_V1_Service{
					Channel:        w.String("user"),
					HartifactsPath: w.String("/tmp/user/hartifacts"),
				},
			},
		},
	}
	overrideConfig := &AutomateConfig{
		Deployment: &ConfigRequest{
			V1: &ConfigRequest_V1{
				Svc: &ConfigRequest_V1_Service{
					Channel:        w.String("override"),
					OverrideOrigin: w.String("override"),
				},
			},
		},
	}

	assert := assert.New(t)
	err := userConfig.OverrideConfigValues(overrideConfig)

	assert.NoError(err)
	assert.Equal("/tmp/user/hartifacts", userConfig.Deployment.V1.Svc.HartifactsPath.Value)
	assert.Equal("override", userConfig.Deployment.V1.Svc.Channel.Value)
	assert.Equal("override", userConfig.Deployment.V1.Svc.OverrideOrigin.Value)
}

func TestDeepCopy(t *testing.T) {
	cfg := DefaultAutomateConfig()
	copy, err := cfg.NewDeepCopy()
	require.NoError(t, err)

	// make sure they're the same
	assert.True(t, reflect.DeepEqual(cfg, copy))
	cfg.Deployment = nil
	assert.Nil(t, cfg.Deployment)
	assert.Equal(t, copy.Deployment, DefaultConfigRequest())
}

func TestOverrideBooleans(t *testing.T) {
	defaultConfig := &AutomateConfig{
		Elasticsearch: &es.ConfigRequest{
			V1: &es.ConfigRequest_V1{
				Sys: &es.ConfigRequest_V1_System{
					Node: &es.ConfigRequest_V1_Node{
						Data:   w.Bool(true),
						Master: w.Bool(false),
					},
				},
			},
		},
	}
	overrideConfig := &AutomateConfig{
		Elasticsearch: &es.ConfigRequest{
			V1: &es.ConfigRequest_V1{
				Sys: &es.ConfigRequest_V1_System{
					Node: &es.ConfigRequest_V1_Node{
						Data:   w.Bool(false),
						Master: w.Bool(true),
					},
				},
			},
		},
	}

	assert := assert.New(t)
	err := defaultConfig.OverrideConfigValues(overrideConfig)

	assert.NoError(err)
	assert.Equal(w.Bool(false), defaultConfig.Elasticsearch.V1.Sys.Node.Data)
	assert.Equal(w.Bool(true), defaultConfig.Elasticsearch.V1.Sys.Node.Master)
}

func TestCorrectlyAnnotated(t *testing.T) {
	// All service configs require an annotation like this:
	// ```
	// import "components/automate-grpc/protoc-gen-a2-config/api/a2conf/annotations.proto";

	// message ConfigRequest {
	// 		option (chef.automate.api.service_config) = {name: "es-sidecar-service"};
	// ```
	//
	// Make sure to use something like the following when building your protobufs:
	// ```
	// protoc -I .\
	// -I "$GOPATH/src" \
	// --go_out=plugins=grpc,paths=source_relative:/src" \
	// --a2-config_out,paths=source_relative=/src \
	// "${proto_files}"
	//```

	cfg := AutomateConfig{}
	a2ServiceConfigType := reflect.TypeOf((*a2conf.A2ServiceConfig)(nil)).Elem()
	root := reflect.TypeOf(cfg)
	for i := 0; i < root.NumField(); i++ {
		f := root.Field(i)
		if f.Name == "Global" || strings.HasPrefix(f.Name, "XXX_") {
			continue
		}

		if !f.Type.Implements(a2ServiceConfigType) {
			assert.Fail(t, "service is not correctly annotated", "%s is not correctly annotated", f.Name)
		}
	}
}

func TestDeprecatedRedaction(t *testing.T) {
	cfg := NewAutomateConfig()
	cfg.Compliance.V1.Sys.Retention = &compliance.ConfigRequest_V1_System_Retention{
		ComplianceReportDays: w.Int32(1),
	}
	cfg.EventFeedService.V1.Sys.Service.PurgeEventFeedAfterDays = w.Int32(2)
	cfg.Ingest.V1.Sys.Service.PurgeActionsAfterDays = w.Int32(3)
	cfg.Ingest.V1.Sys.Service.PurgeConvergeHistoryAfterDays = w.Int32(4)

	cfg.Redact()

	require.Nil(t, cfg.Compliance.V1.Sys.Retention, "compliance retention not redacted")
	require.Nil(t, cfg.EventFeedService.V1.Sys.Service.PurgeEventFeedAfterDays, "event feed purge after days not redacted")
	require.Nil(t, cfg.Ingest.V1.Sys.Service.PurgeActionsAfterDays, "ingest purge action after days not redacted")
	require.Nil(t, cfg.Ingest.V1.Sys.Service.PurgeConvergeHistoryAfterDays, "purge converge history after days not redacted")
}

func validTLSCredentialForTest() *shared.FrontendTLSCredential {
	return &shared.FrontendTLSCredential{
		ServerName: "default_value_in_test.example",
		Cert:       "Certificate content goes here in real life",
		Key:        "Private key content goes here in real life",
	}
}

func validTLSCredentialSliceForTest() []*shared.FrontendTLSCredential {
	return []*shared.FrontendTLSCredential{validTLSCredentialForTest()}
}
