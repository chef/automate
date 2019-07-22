package dex

import (
	"encoding/pem"
	"fmt"
	"strings"

	google_protobuf "github.com/golang/protobuf/ptypes/wrappers"

	"github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new ConfigRequests instance with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:       &shared.Mlsa{},
				Log:        &ConfigRequest_V1_Log{},
				Service:    &ConfigRequest_V1_System_Service{},
				Grpc:       &ConfigRequest_V1_Grpc{},
				Storage:    &ConfigRequest_V1_Storage{},
				Expiry:     &ConfigRequest_V1_Expiry{},
				Bootstrap:  &ConfigRequest_V1_Bootstrap{},
				Connectors: &ConfigRequest_V1_Connectors{},
				Tls:        &shared.TLSCredentials{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new ConfigRequest instance with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()

	c.V1.Sys.Grpc.Host = w.String("0.0.0.0")
	c.V1.Sys.Grpc.Port = w.Int32(10116)

	c.V1.Sys.Service.Host = w.String("0.0.0.0")
	c.V1.Sys.Service.Port = w.Int32(10117)

	c.V1.Sys.Expiry.IdTokens = w.String("3m")

	c.V1.Sys.Bootstrap.InsecureAdmin = w.Bool(false)

	c.V1.Sys.Log.Level = w.String("info")

	return c
}

// Validate validates that the config is sufficient to start the service and returns true.
func (c *ConfigRequest) Validate() error {
	cfgErr := shared.NewInvalidConfigError()

	if conn := c.V1.Sys.Connectors; conn != nil {
		// we can only have one connector: ldap, saml, or msad_ldap
		if (conn.Saml != nil && conn.Ldap != nil) ||
			(conn.MsadLdap != nil && conn.Ldap != nil) ||
			(conn.MsadLdap != nil && conn.Saml != nil) {
			cfgErr.AddInvalidValue("dex.v1.sys.connectors",
				"auth config can only have one of the following connectors: [ldap, saml, msad_ldap]")
		}

		if ldap := conn.Ldap; ldap != nil {
			for key, val := range map[string]*google_protobuf.StringValue{
				"host":                ldap.Host,
				"base_user_search_dn": ldap.BaseUserSearchDn,
				"username_attr":       ldap.UsernameAttr,
				"user_id_attr":        ldap.UserIdAttr,
			} {
				if val.GetValue() == "" {
					cfgErr.AddMissingKey("dex.v1.sys.connectors.ldap." + key)
				}
			}

			if ldap.BindPassword.GetValue() != "" && ldap.BindDn.GetValue() == "" {
				cfgErr.AddInvalidValue("dex.v1.sys.connectors.ldap", "bind_password with unset bind_dn is invalid")
			}

			// optionally verify ca_contents
			if caContents := ldap.CaContents.GetValue(); caContents != "" {
				checkCertsPEM(cfgErr, "dex.v1.sys.connectors.ldap.ca_contents", caContents)
			}
		}

		if msad := conn.MsadLdap; msad != nil {
			for key, val := range map[string]*google_protobuf.StringValue{
				"host":                msad.Host,
				"base_user_search_dn": msad.BaseUserSearchDn,
			} {
				if val.GetValue() == "" {
					cfgErr.AddMissingKey("dex.v1.sys.connectors.msad_ldap." + key)
				}
			}

			if msad.BindPassword.GetValue() != "" && msad.BindDn.GetValue() == "" {
				cfgErr.AddInvalidValue("dex.v1.sys.connectors.msad", "bind_password with unset bind_dn is invalid")
			}

			// optionally verify ca_contents
			if caContents := msad.CaContents.GetValue(); caContents != "" {
				checkCertsPEM(cfgErr, "dex.v1.sys.connectors.msad.ca_contents", caContents)
			}
		}

		if saml := conn.Saml; saml != nil {
			for key, val := range map[string]*google_protobuf.StringValue{
				"ca_contents":   saml.CaContents,
				"sso_url":       saml.SsoUrl,
				"username_attr": saml.UsernameAttr,
				"email_attr":    saml.EmailAttr,
			} {
				if val.GetValue() == "" {
					cfgErr.AddMissingKey("dex.v1.sys.connectors.saml." + key)
				}
			}
			checkCertsPEM(cfgErr, "dex.v1.sys.connector.saml.ca_contents", saml.CaContents.GetValue())
			valid := map[string]bool{
				"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress":               true,
				"urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified":                true,
				"urn:oasis:names:tc:SAML:1.1:nameid-format:X509SubjectName":            true,
				"urn:oasis:names:tc:SAML:1.1:nameid-format:WindowsDomainQualifiedName": true,
				"urn:oasis:names:tc:SAML:2.0:nameid-format:encrypted":                  true,
				"urn:oasis:names:tc:SAML:2.0:nameid-format:entity":                     true,
				"urn:oasis:names:tc:SAML:2.0:nameid-format:kerberos":                   true,
				"urn:oasis:names:tc:SAML:2.0:nameid-format:persistent":                 true,
				"urn:oasis:names:tc:SAML:2.0:nameid-format:transient":                  true,
			}
			if val := saml.NameIdPolicyFormat.GetValue(); val != "" {
				if !valid[val] {
					cfgErr.AddInvalidValue("dex.v1.sys.connector.saml.name_id_policy_format",
						fmt.Sprintf("invalid name_id_policy_format: %q", val))
				}
			}
		}
	}

	if cfgErr.IsEmpty() {
		return nil
	}
	return cfgErr
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(creds *shared.TLSCredentials) (shared.PreparedSystemConfig, error) {
	sys := c.V1.Sys
	sys.Tls = creds

	// Note: it's common to use a case variant for sAMAccountName that will cause
	// issues only at login-time.
	// For querying LDAP, attribute case doesn't matter. However, when
	// constructing the user entry from the response, dex currently does a case-
	// sensitive string match. To work around that issue, we ensure that commonly
	// "odd-cased" attribute names are changed to reflect what's in their
	// corresponding OID registration entry, and this should coincide with what
	// any actual LDAP service should return.
	//
	// There's an upstream PR to fix this here: https://github.com/dexidp/dex/pull/1251
	// However, this workaround will make the papercut go away faster.
	c.V1.Sys.GetConnectors().GetLdap().fixCommonCaseIssues()

	// default name_id_policy_format (SAML)
	c.V1.Sys.GetConnectors().GetSaml().setNameIDPolicyDefault()

	return c.V1.Sys, nil
}

// SetGlobalConfig imports settings from the global configuration
func (c *ConfigRequest) SetGlobalConfig(g *shared.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa
	c.V1.Sys.Service.ExternalFqdn = g.V1.Fqdn

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = GlobalLogLevelToDexLevel(logLevel)
	}
}

// Convert the accepted GlobalLogLevels to a log level accepted by
// Dex.
func GlobalLogLevelToDexLevel(level string) string {
	switch level {
	case "info":
		return "info"
	case "debug":
		return "debug"
	case "warning", "error", "fatal", "panic":
		return "error"
	default:
		return "info"
	}
}

func (ldapCfg *ConfigRequest_V1_Ldap) fixCommonCaseIssues() {
	if ldapCfg == nil {
		return
	}
	// Note: we fix every user-controlled ldap attribute, let's not assume too
	// much and only fix half the issues
	ldapCfg.UserIdAttr = fixCase(ldapCfg.UserIdAttr)
	ldapCfg.UsernameAttr = fixCase(ldapCfg.UsernameAttr)
	ldapCfg.UserDisplayNameAttr = fixCase(ldapCfg.UserDisplayNameAttr)
	ldapCfg.GroupDisplayNameAttr = fixCase(ldapCfg.GroupDisplayNameAttr)
	ldapCfg.FilterGroupsByUserAttr = fixCase(ldapCfg.FilterGroupsByUserAttr)
	// This is also an attribute, albeit an oddly-named one
	ldapCfg.FilterGroupsByUserValue = fixCase(ldapCfg.FilterGroupsByUserValue)
	ldapCfg.EmailAttr = fixCase(ldapCfg.EmailAttr)
}

func fixCase(wrap *google_protobuf.StringValue) *google_protobuf.StringValue {
	if wrap == nil {
		return nil
	}
	return w.String(fix(wrap.Value))
}

func fix(in string) string {
	// initialize commonAttrs
	// key => val is  samaccountname (all lowercase) => sAMAccountName (defined case)
	commonAttrs := map[string]string{}
	for _, attr := range []string{
		"DN", // Note: DN is handled in a special way and thus needs to be uppercase
		"sAMAccountName",
		"cn",
		"mail",
		"uid",
		"gidNumber",
		"memberOf",
	} {
		commonAttrs[strings.ToLower(attr)] = attr
	}

	if out, ok := commonAttrs[strings.ToLower(in)]; ok {
		return out
	}
	return in
}

func checkCertsPEM(cfgErr *shared.InvalidConfigError, key, data string) {
	caData := []byte(data)
	var block *pem.Block
	for {
		block, caData = pem.Decode(caData)
		if block == nil {
			if len(caData) > 0 { // nothing decoded but there's data left -- bad input
				cfgErr.AddInvalidValue(key,
					fmt.Sprintf("invalid certificate data: %q", string(caData)))
			}
			break
		} else if block.Type != "CERTIFICATE" {
			cfgErr.AddInvalidValue(key,
				fmt.Sprintf("invalid PEM type: %q, expected \"CERTIFICATE\"", block.Type))
		}
	}
}

func (samlCfg *ConfigRequest_V1_Saml) setNameIDPolicyDefault() {
	if samlCfg == nil {
		return
	}
	if samlCfg.NameIdPolicyFormat == nil {
		samlCfg.NameIdPolicyFormat = w.String("urn:oasis:names:tc:SAML:2.0:nameid-format:persistent")
	}
}
