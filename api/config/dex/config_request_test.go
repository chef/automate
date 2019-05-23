package dex_test

import (
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/config/dex"
	"github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/lib/tls/test/helpers"
)

func completeMSADWithoutOverrides() *dex.ConfigRequest_V1_Msad_Ldap {
	return &dex.ConfigRequest_V1_Msad_Ldap{
		Host:              w.String("ad.au"),
		BindDn:            w.String("cn=admin,dc=org"),
		BindPassword:      w.String("ChefAutomate1"),
		BaseUserSearchDn:  w.String("ou=people,dc=org"),
		BaseGroupSearchDn: w.String("ou=groups,dc=org"),
	}
}

func TestValidate(t *testing.T) {
	t.Run("Validates when there are no connectors", func(t *testing.T) {
		cfg := dex.DefaultConfigRequest()
		assert.Nil(t, cfg.Validate())
	})

	devCert, err := ioutil.ReadFile(helpers.DevRootCACert())
	require.NoError(t, err)

	completeLDAP := func() *dex.ConfigRequest_V1_Ldap {
		return &dex.ConfigRequest_V1_Ldap{
			Host:              w.String("ad.au"),
			BindDn:            w.String("cn=admin,dc=org"),
			BindPassword:      w.String("ChefAutomate1"),
			BaseUserSearchDn:  w.String("ou=people,dc=org"),
			UsernameAttr:      w.String("sn"),
			UserIdAttr:        w.String("cn"),
			BaseGroupSearchDn: w.String("ou=groups,dc=org"),
		}
	}

	completeSAML := func() *dex.ConfigRequest_V1_Saml {
		return &dex.ConfigRequest_V1_Saml{
			CaContents:   w.String(string(devCert)),
			SsoUrl:       w.String("https://saml.com/idp"),
			EmailAttr:    w.String("email"),
			UsernameAttr: w.String("username"),
			GroupsAttr:   w.String("groups"),
		}
	}

	t.Run("Validates when there is one connector (ldap, valid config)", func(t *testing.T) {
		t.Run("full config", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{
				Ldap: completeLDAP(),
			}
			assert.Nil(t, cfg.Validate())
		})

		t.Run("anon bind (no bind_dn or bind_password)", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			ldap := completeLDAP()
			ldap.BindPassword = nil
			ldap.BindDn = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			assert.Nil(t, cfg.Validate())
		})

		t.Run("unauthenticated bind (no bind_password))", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			ldap := completeLDAP()
			ldap.BindPassword = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			assert.Nil(t, cfg.Validate())
		})

		t.Run("no base_group_search_dn", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			ldap := completeLDAP()
			ldap.BaseGroupSearchDn = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			assert.Nil(t, cfg.Validate())
		})
	})

	t.Run("Validates when there is one connector (msad, valid config)", func(t *testing.T) {
		t.Run("full config", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{
				MsadLdap: completeMSADWithoutOverrides(),
			}
			assert.Nil(t, cfg.Validate())
		})

		t.Run("anon bind (no bind_dn or bind_password)", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			msad := completeMSADWithoutOverrides()
			msad.BindPassword = nil
			msad.BindDn = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{MsadLdap: msad}
			assert.Nil(t, cfg.Validate())
		})

		t.Run("unauthenticated bind (no bind_password))", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			msad := completeMSADWithoutOverrides()
			msad.BindPassword = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{MsadLdap: msad}
			assert.Nil(t, cfg.Validate())
		})

		t.Run("no base_group_search_dn", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			msad := completeMSADWithoutOverrides()
			msad.BaseGroupSearchDn = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{MsadLdap: msad}
			assert.Nil(t, cfg.Validate())
		})
	})

	t.Run("Validates when there is one connector (saml, valid config)", func(t *testing.T) {
		cfg := dex.DefaultConfigRequest()
		cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{
			Saml: completeSAML(),
		}
		assert.Nil(t, cfg.Validate())
	})

	t.Run("missing values (ldap)", func(t *testing.T) {
		t.Run("host", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			ldap := completeLDAP()
			ldap.Host = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("bind_password without bind_dn", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			ldap := completeLDAP()
			ldap.BindDn = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("base_user_search_dn", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			ldap := completeLDAP()
			ldap.BaseUserSearchDn = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("username_attr", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			ldap := completeLDAP()
			ldap.UsernameAttr = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("user_id_attr", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			ldap := completeLDAP()
			ldap.UserIdAttr = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			assert.NotNil(t, cfg.Validate())
		})
	})

	t.Run("empty-string values (ldap)", func(t *testing.T) {
		t.Run("host", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			ldap := completeLDAP()
			ldap.Host = w.String("")
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("bind_password without bind_dn", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			ldap := completeLDAP()
			ldap.BindDn = w.String("")
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("base_user_search_dn", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			ldap := completeLDAP()
			ldap.BaseUserSearchDn = w.String("")
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("username_attr", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			ldap := completeLDAP()
			ldap.UsernameAttr = w.String("")
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("user_id_attr", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			ldap := completeLDAP()
			ldap.UserIdAttr = w.String("")
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			assert.NotNil(t, cfg.Validate())
		})
	})

	t.Run("missing values (msad)", func(t *testing.T) {
		t.Run("host", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			msad := completeMSADWithoutOverrides()
			msad.Host = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{MsadLdap: msad}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("bind_password without bind_dn", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			msad := completeMSADWithoutOverrides()
			msad.BindDn = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{MsadLdap: msad}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("base_user_search_dn", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			msad := completeMSADWithoutOverrides()
			msad.BaseUserSearchDn = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{MsadLdap: msad}
			assert.NotNil(t, cfg.Validate())
		})
	})

	t.Run("empty-string values (msad)", func(t *testing.T) {
		t.Run("host", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			msad := completeMSADWithoutOverrides()
			msad.Host = w.String("")
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{MsadLdap: msad}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("bind_password without bind_dn", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			msad := completeMSADWithoutOverrides()
			msad.BindDn = w.String("")
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{MsadLdap: msad}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("base_user_search_dn", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			msad := completeMSADWithoutOverrides()
			msad.BaseUserSearchDn = w.String("")
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{MsadLdap: msad}
			assert.NotNil(t, cfg.Validate())
		})
	})

	t.Run("missing values (saml)", func(t *testing.T) {
		t.Run("ca_contents", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			saml := completeSAML()
			saml.CaContents = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Saml: saml}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("sso_url", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			saml := completeSAML()
			saml.SsoUrl = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Saml: saml}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("username_attr", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			saml := completeSAML()
			saml.UsernameAttr = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Saml: saml}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("email_attr", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			saml := completeSAML()
			saml.EmailAttr = nil
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Saml: saml}
			assert.NotNil(t, cfg.Validate())
		})
	})

	t.Run("empty-string values (saml)", func(t *testing.T) {
		t.Run("ca_contents", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			saml := completeSAML()
			saml.CaContents = w.String("")
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Saml: saml}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("sso_url", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			saml := completeSAML()
			saml.SsoUrl = w.String("")
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Saml: saml}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("username_attr", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			saml := completeSAML()
			saml.UsernameAttr = w.String("")
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Saml: saml}
			assert.NotNil(t, cfg.Validate())
		})

		t.Run("email_attr", func(t *testing.T) {
			cfg := dex.DefaultConfigRequest()
			saml := completeSAML()
			saml.EmailAttr = w.String("")
			cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{Saml: saml}
			assert.NotNil(t, cfg.Validate())
		})
	})

	t.Run("ca_contents", func(t *testing.T) {
		tests := map[string]func(certs string) *dex.ConfigRequest_V1_Connectors{
			"saml": func(certs string) *dex.ConfigRequest_V1_Connectors {
				saml := completeSAML()
				saml.CaContents = w.String(certs)
				return &dex.ConfigRequest_V1_Connectors{Saml: saml}
			},
			"ldap": func(certs string) *dex.ConfigRequest_V1_Connectors {
				ldap := completeLDAP()
				ldap.CaContents = w.String(certs)
				return &dex.ConfigRequest_V1_Connectors{Ldap: ldap}
			},
			"msad": func(certs string) *dex.ConfigRequest_V1_Connectors {
				msad := completeMSADWithoutOverrides()
				msad.CaContents = w.String(certs)
				return &dex.ConfigRequest_V1_Connectors{MsadLdap: msad}
			},
		}
		for name, connector := range tests {
			t.Run(name, func(t *testing.T) {
				t.Run("multiple certs", func(t *testing.T) {
					var certs string
					for _, svc := range []string{"automate-dex", "authn-service", "authz-service"} {
						caData, err := ioutil.ReadFile(helpers.DevCertPath(svc))
						require.NoError(t, err)
						certs += string(caData)
					}
					cfg := dex.DefaultConfigRequest()
					cfg.V1.Sys.Connectors = connector(certs)
					assert.Nil(t, cfg.Validate())
				})

				t.Run("multiple certs with more newlines", func(t *testing.T) {
					var certs string
					for _, svc := range []string{"automate-dex", "authn-service", "authz-service"} {
						caData, err := ioutil.ReadFile(helpers.DevCertPath(svc))
						require.NoError(t, err)
						certs += "\n\n" + string(caData)
					}
					cfg := dex.DefaultConfigRequest()
					cfg.V1.Sys.Connectors = connector(certs)
					assert.Nil(t, cfg.Validate())
				})

				failWithCert := func(cert string) func(*testing.T) {
					return func(t *testing.T) {
						cfg := dex.DefaultConfigRequest()
						cfg.V1.Sys.Connectors = connector(cert)
						assert.NotNil(t, cfg.Validate())
					}
				}

				t.Run("trailing data", failWithCert(string(devCert)+"\nyadda"))
				t.Run("non-certificate PEM", func(t *testing.T) {
					caData, err := ioutil.ReadFile(helpers.DevRootCAKey())
					require.NoError(t, err)
					failWithCert(string(caData))(t)
				})
			})
		}
	})

	t.Run("Validate fails when there is more than one connector", func(t *testing.T) {
		cfg := dex.DefaultConfigRequest()
		combinations := []dex.ConfigRequest_V1_Connectors{
			{
				Ldap: &dex.ConfigRequest_V1_Ldap{},
				Saml: &dex.ConfigRequest_V1_Saml{},
			},
			{
				Ldap:     &dex.ConfigRequest_V1_Ldap{},
				MsadLdap: &dex.ConfigRequest_V1_Msad_Ldap{},
			},
			{
				MsadLdap: &dex.ConfigRequest_V1_Msad_Ldap{},
				Saml:     &dex.ConfigRequest_V1_Saml{},
			},
			{
				MsadLdap: &dex.ConfigRequest_V1_Msad_Ldap{},
				Saml:     &dex.ConfigRequest_V1_Saml{},
				Ldap:     &dex.ConfigRequest_V1_Ldap{},
			},
		}

		for _, connector := range combinations {
			cfg.V1.Sys.Connectors = &connector

			err := cfg.Validate()
			require.Error(t, err)
			cfgErr, ok := err.(shared.Error)
			require.True(t, ok)
			assert.Equal(t, 1, len(cfgErr.InvalidValues()))
		}
	})
}

func TestMSADConfig(t *testing.T) {
	t.Run("returns empty results for optional fields", func(t *testing.T) {
		cfg := dex.DefaultConfigRequest()
		msadConfig := completeMSADWithoutOverrides()
		cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{
			MsadLdap: msadConfig,
		}
		sys, err := cfg.PrepareSystemConfig(&shared.TLSCredentials{})
		require.NoError(t, err)
		require.NotNil(t, sys)
		c, ok := sys.(*dex.ConfigRequest_V1_System)
		require.True(t, ok)

		// User defined
		assert.Equal(t, "ad.au", c.GetConnectors().GetMsadLdap().GetHost().GetValue())
		assert.Equal(t, "cn=admin,dc=org", c.GetConnectors().GetMsadLdap().GetBindDn().GetValue())
		assert.Equal(t, "ChefAutomate1", c.GetConnectors().GetMsadLdap().GetBindPassword().GetValue())
		assert.Equal(t, "ChefAutomate1", c.GetConnectors().GetMsadLdap().GetBindPassword().GetValue())
		assert.Equal(t, "ou=people,dc=org", c.GetConnectors().GetMsadLdap().GetBaseUserSearchDn().GetValue())
		assert.Equal(t, "ou=groups,dc=org", c.GetConnectors().GetMsadLdap().GetBaseGroupSearchDn().GetValue())

		// Defaults are unset so that they receive overrides in config
		assert.Equal(t, false, c.GetConnectors().GetMsadLdap().GetInsecureNoSsl().GetValue())
		assert.Equal(t, "", c.GetConnectors().GetMsadLdap().GetUserQueryFilter().GetValue())
		assert.Equal(t, "", c.GetConnectors().GetMsadLdap().GetUsernameAttr().GetValue())
		assert.Equal(t, "", c.GetConnectors().GetMsadLdap().GetUserIdAttr().GetValue())
		assert.Equal(t, "", c.GetConnectors().GetMsadLdap().GetEmailAttr().GetValue())
		assert.Equal(t, "", c.GetConnectors().GetMsadLdap().GetUserDisplayNameAttr().GetValue())
		assert.Equal(t, "", c.GetConnectors().GetMsadLdap().GetGroupQueryFilter().GetValue())
		assert.Equal(t, "", c.GetConnectors().GetMsadLdap().GetFilterGroupsByUserValue().GetValue())
		assert.Equal(t, "", c.GetConnectors().GetMsadLdap().GetFilterGroupsByUserAttr().GetValue())
		assert.Equal(t, "", c.GetConnectors().GetMsadLdap().GetGroupDisplayNameAttr().GetValue())
	})

	t.Run("returns populated fields if specified by user", func(t *testing.T) {
		cfg := dex.DefaultConfigRequest()
		msadConfig := completeMSADWithoutOverrides()
		msadConfig.InsecureNoSsl = w.Bool(true)
		msadConfig.UserQueryFilter = w.String("(objectClass=NotPerson)")
		msadConfig.UsernameAttr = w.String("NotsAMAccountName")
		msadConfig.UserIdAttr = w.String("DefinitelyNotsAMAccountName")
		msadConfig.EmailAttr = w.String("NotMail")
		msadConfig.EmailAttr = w.String("NotMail")
		msadConfig.UserDisplayNameAttr = w.String("NotDisplayName")
		msadConfig.GroupQueryFilter = w.String("(objectClass=NotGroup)")
		msadConfig.FilterGroupsByUserValue = w.String("NotDN")
		msadConfig.FilterGroupsByUserAttr = w.String("NotMember")
		msadConfig.GroupDisplayNameAttr = w.String("DefinitelyNotGroupDisplayName")
		cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{
			MsadLdap: msadConfig,
		}
		sys, err := cfg.PrepareSystemConfig(&shared.TLSCredentials{})
		require.NoError(t, err)
		require.NotNil(t, sys)
		c, ok := sys.(*dex.ConfigRequest_V1_System)
		require.True(t, ok)

		// User defined
		assert.Equal(t, "ad.au", c.GetConnectors().GetMsadLdap().GetHost().GetValue())
		assert.Equal(t, "cn=admin,dc=org", c.GetConnectors().GetMsadLdap().GetBindDn().GetValue())
		assert.Equal(t, "ChefAutomate1", c.GetConnectors().GetMsadLdap().GetBindPassword().GetValue())
		assert.Equal(t, "ChefAutomate1", c.GetConnectors().GetMsadLdap().GetBindPassword().GetValue())
		assert.Equal(t, "ou=people,dc=org", c.GetConnectors().GetMsadLdap().GetBaseUserSearchDn().GetValue())
		assert.Equal(t, "ou=groups,dc=org", c.GetConnectors().GetMsadLdap().GetBaseGroupSearchDn().GetValue())

		// Defaults are unset so that they receive overrides in config
		assert.Equal(t, true, c.GetConnectors().GetMsadLdap().GetInsecureNoSsl().GetValue())
		assert.Equal(t, "(objectClass=NotPerson)", c.GetConnectors().GetMsadLdap().GetUserQueryFilter().GetValue())
		assert.Equal(t, "NotsAMAccountName", c.GetConnectors().GetMsadLdap().GetUsernameAttr().GetValue())
		assert.Equal(t, "DefinitelyNotsAMAccountName", c.GetConnectors().GetMsadLdap().GetUserIdAttr().GetValue())
		assert.Equal(t, "NotMail", c.GetConnectors().GetMsadLdap().GetEmailAttr().GetValue())
		assert.Equal(t, "NotDisplayName", c.GetConnectors().GetMsadLdap().GetUserDisplayNameAttr().GetValue())
		assert.Equal(t, "(objectClass=NotGroup)", c.GetConnectors().GetMsadLdap().GetGroupQueryFilter().GetValue())
		assert.Equal(t, "NotDN", c.GetConnectors().GetMsadLdap().GetFilterGroupsByUserValue().GetValue())
		assert.Equal(t, "NotMember", c.GetConnectors().GetMsadLdap().GetFilterGroupsByUserAttr().GetValue())
		assert.Equal(t, "DefinitelyNotGroupDisplayName", c.GetConnectors().GetMsadLdap().GetGroupDisplayNameAttr().GetValue())
	})
}

func TestPrepareSystemConfig(t *testing.T) {
	t.Run("returns PreparedSystemConfig", func(t *testing.T) {
		cfg := dex.DefaultConfigRequest()
		sys, err := cfg.PrepareSystemConfig(&shared.TLSCredentials{})
		assert.NoError(t, err)
		assert.NotNil(t, sys)
	})

	t.Run("fixes well-known issues with sAMAccountName case", func(t *testing.T) {
		cfg := dex.DefaultConfigRequest()
		cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{
			Ldap: &dex.ConfigRequest_V1_Ldap{
				UserIdAttr:              w.String("samAccountName"),
				UsernameAttr:            w.String("samAccountName"),
				UserDisplayNameAttr:     w.String("samAccountName"),
				GroupDisplayNameAttr:    w.String("samAccountName"),
				FilterGroupsByUserAttr:  w.String("samAccountName"),
				FilterGroupsByUserValue: w.String("samAccountName"),
				EmailAttr:               w.String("samAccountName"),
			},
		}
		sys, err := cfg.PrepareSystemConfig(&shared.TLSCredentials{})
		require.NoError(t, err)
		require.NotNil(t, sys)
		c, ok := sys.(*dex.ConfigRequest_V1_System)
		require.True(t, ok)
		isFixed := func(s string) { assert.Equal(t, "sAMAccountName", s) }
		isFixed(c.GetConnectors().GetLdap().GetUserIdAttr().GetValue())
		isFixed(c.GetConnectors().GetLdap().GetUsernameAttr().GetValue())
		isFixed(c.GetConnectors().GetLdap().GetUserDisplayNameAttr().GetValue())
		isFixed(c.GetConnectors().GetLdap().GetGroupDisplayNameAttr().GetValue())
		isFixed(c.GetConnectors().GetLdap().GetFilterGroupsByUserAttr().GetValue())
		isFixed(c.GetConnectors().GetLdap().GetFilterGroupsByUserValue().GetValue())
		isFixed(c.GetConnectors().GetLdap().GetEmailAttr().GetValue())
	})

	t.Run("keeps unset values intact when fixing samAccountName", func(t *testing.T) {
		cfg := dex.DefaultConfigRequest()
		cfg.V1.Sys.Connectors = &dex.ConfigRequest_V1_Connectors{
			Ldap: &dex.ConfigRequest_V1_Ldap{},
		}
		sys, err := cfg.PrepareSystemConfig(&shared.TLSCredentials{})
		require.NoError(t, err)
		require.NotNil(t, sys)
		c, ok := sys.(*dex.ConfigRequest_V1_System)
		require.True(t, ok)
		isUntouched := func(s interface{}) { assert.Nil(t, s) }
		isUntouched(c.GetConnectors().GetLdap().GetUserIdAttr())
		isUntouched(c.GetConnectors().GetLdap().GetUsernameAttr())
		isUntouched(c.GetConnectors().GetLdap().GetUserDisplayNameAttr())
		isUntouched(c.GetConnectors().GetLdap().GetGroupDisplayNameAttr())
		isUntouched(c.GetConnectors().GetLdap().GetFilterGroupsByUserAttr())
		isUntouched(c.GetConnectors().GetLdap().GetFilterGroupsByUserValue())
		isUntouched(c.GetConnectors().GetLdap().GetEmailAttr())
	})
}
