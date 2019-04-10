package load_balancer

import (
	"testing"

	"github.com/stretchr/testify/assert"

	shared "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestValidateConfigRequestMissingLB(t *testing.T) {
	c := NewConfigRequest()
	err := c.Validate()

	expected := shared.InvalidConfigError{}
	expected.AddMissingKey("load_balancer.v1.sys.service.external_fqdn")
	expected.AddMissingKey("load_balancer.v1.sys.frontend_tls")

	assert.EqualError(t, err, expected.Error(), "")
}

func TestValidateConfigRequestMissingFrontendTLS(t *testing.T) {
	c := NewConfigRequest()
	c.V1.Sys.Service.ExternalFqdn = w.String("fqdn")
	c.V1.Sys.FrontendTls = []*shared.FrontendTLSCredential{
		&shared.FrontendTLSCredential{
			ServerName: "foo",
			Key:        "foo",
		},
	}
	err := c.Validate()

	expected := shared.InvalidConfigError{}
	expected.AddMissingKey("load_balancer.v1.sys.frontend_tls.cert")

	assert.EqualError(t, err, expected.Error(), "")
}

func TestValidateConfigRequestValid(t *testing.T) {
	c := newValidConfigRequest()

	assert.Nil(t, c.Validate())
}

func TestValidateGlobalConfig(t *testing.T) {
	t.Run("it configures the fqdn and default frontend",
		func(t *testing.T) {
			c := newValidConfigRequest()
			// confirm that global FQDN gets set on FrontendTls when no ServerName is
			// explicitly configured there.
			c.V1.Sys.FrontendTls[0].ServerName = ""
			g := shared.NewGlobalConfig()
			g.V1.Fqdn = w.String("test")

			c.SetGlobalConfig(g)

			assert.Equal(t, "test", c.V1.Sys.Service.ExternalFqdn.Value)
			assert.Equal(t, "test", c.V1.Sys.FrontendTls[0].ServerName)
		},
	)
	t.Run("it sets the default frontend server_name",
		func(t *testing.T) {
			c := newValidConfigRequest()
			c.V1.Sys.FrontendTls[0].ServerName = ""

			cred2 := validTLSCredentialForTest()
			cred2.ServerName = "internal_fqdn"
			c.V1.Sys.FrontendTls = append(c.V1.Sys.FrontendTls, cred2)

			g := shared.NewGlobalConfig()
			g.V1.Fqdn = w.String("external_fqdn")

			c.SetGlobalConfig(g)

			assert.Equal(t, "external_fqdn", c.V1.Sys.Service.ExternalFqdn.Value)
			assert.Equal(t, "external_fqdn", c.V1.Sys.FrontendTls[0].ServerName)
			assert.Equal(t, "internal_fqdn", c.V1.Sys.FrontendTls[1].ServerName)
		},
	)
	t.Run("it sets missing server_name's to the fqdn",
		func(t *testing.T) {
			c := newValidConfigRequest()
			g := shared.NewGlobalConfig()
			c.V1.Sys.FrontendTls[0].ServerName = ""
			g.V1.Fqdn = w.String("modified_value_from_global_cfg.example")

			c.SetGlobalConfig(g)

			assert.Equal(t, "modified_value_from_global_cfg.example", c.V1.Sys.FrontendTls[0].ServerName)
		},
	)
	t.Run("it does not modify fqdn or server_name if global fqdn is missing",
		func(t *testing.T) {
			c := newValidConfigRequest()
			g := shared.NewGlobalConfig()

			c.SetGlobalConfig(g)

			assert.Equal(t, "default_value_in_test.example", c.V1.Sys.Service.ExternalFqdn.Value)
			assert.Equal(t, "default_value_in_test.example", c.V1.Sys.FrontendTls[0].ServerName)
		},
	)
	t.Run("it configures mlsa",
		func(t *testing.T) {
			c := newValidConfigRequest()
			g := shared.NewGlobalConfig()
			g.V1.Mlsa = &shared.Mlsa{
				Accept: w.Bool(true),
			}
			c.SetGlobalConfig(g)

			assert.Equal(t, true, c.V1.Sys.Mlsa.Accept.Value)
		},
	)
}

func newValidConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.ExternalFqdn = w.String("default_value_in_test.example")
	c.V1.Sys.FrontendTls = validTLSCredentialSliceForTest()

	return c
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
