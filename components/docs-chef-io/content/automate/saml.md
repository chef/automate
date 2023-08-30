+++
title = "SAML"

date = 2018-05-11T09:27:09+00:00
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "SAML"
    parent = "automate/users/authentication"
    identifier = "automate/users/saml.md SAML"
    weight = 50
+++

Chef Automate can integrate with existing Security Assertion Markup Language (SAML) services to authenticate users in Chef Automate, and use their existing group memberships to determine their Chef Automate permissions.

## Authentication via Existing Identity Management Systems

Chef Automate supports externally managed users from an external identity provider (IdP) as well as local users. You can configure _one_ LDAP service (or MSAD for simplified configuration of Active Directory setups) and _one_ SAML IdP on the same instance.

If you are only using local users and teams that you create in Chef Automate, then you do not need to configure an external IdP. See the [Users]({{< relref "users.md" >}}) documentation for information on creating and managing local users.

{{< warning >}}
A Chef Automate instance supports using two different IdPs at the same time:

- One IdP using SAML and
- One IdP using LDAP

A Chef Automate does not support using two of the _same_ IdPs at one time:

- Two SAML IdPs on one Chef Automate instance will not work.
- Two LDAP IdPs on one Chef Automate instance will not work/
{{< /warning >}}

### Configure Identity Providers

To configure authentication for your Chef Automate installation:

1. Create a TOML file with your partial SAML configuration.
1. Run `chef-automate config patch </path/to/your-file.toml>` on the command line to apply your change.

### Change Identity Providers

To change your configured IdP, replace your existing configuration with these steps:

1. Run `chef-automate config show config.toml`.
2. Edit `config.toml` to replace the `dex.v1.sys.connectors` section with the config values for your new identity provider.
3. Run `chef-automate config set config.toml` to set your updated config.

{{< note >}}
Users who sign in via SAML will have a session time of 24 hours before needing to sign in again.
{{< /note >}}

## Supported Identity Management Systems

- [Azure AD]({{< relref "#azure-ad" >}})
- Office365
- OKTA
- OneLogin
- Ping
- Tivoli Federated Identity Manager

Chef Automate uses the [Dex](https://github.com/dexidp/dex) library to support SAML integrations. Dex does not support IdP-initiated SAML logins with for these IdPs.
This means that Chef Automate also cannot support IdP-initiated SAML logins with these IdPs.

Attempting to sign in with an unsupported IdP-supported SAML login causes the `unsupported auth mode` error. Fall back to the typical SP-initiated login mode and proceed with your Chef Automate SAML configuration.

### Azure Active Directory

Using Azure AD as an SAML IdP requires specific configuration for both Azure AD and Chef Automate.

{{< note >}}
The signing certificate used for Chef Automate's SAML integration with Azure AD requires manual management.
Signing key rotation is not done automatically.
{{< /note >}}

In Azure AD, add Chef Automate as a `non-gallery application`, and then configure its SAML sign-in method.
[The Azure AD documentation](https://docs.microsoft.com/en-us/azure/active-directory/manage-apps/configure-saml-single-sign-on) provides a detailed guide.
Enter `https://{{< example_fqdn "automate" >}}/dex/callback` as the value for both _Identifier (Entity ID)_ and _Reply URL (Assertion Consumer Service URL)_.

You may use the default claims provided by Azure AD.
Remember to edit the Chef Automate configuration in `config.toml` to reflect this claims information.

Download the _Certificate (Base64)_ in Azure AD and take note of the _Login URL_ of use in the Chef Automate configuration.

After configuring Azure AD, edit your Chef Automate `config.toml` configuration file to reflect the values entered in the Azure AD interface.

The minimal configuration snippet in `config.toml` will looks like:

```toml
[dex.v1.sys.connectors.saml]
ca_contents="""
<<Certificate (Base64)>>
"""
sso_url = "<<Login URL>>"
email_attr = "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress"
username_attr = "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress"
entity_issuer = "https://{{< example_fqdn "automate" >}}/dex/callback"
```

where:

- `ca_contents` contains the value of the _Certificate (Base64)_, and includes the `-----BEGIN CERTIFICATE-----` and `-----END CERTIFICATE-----` markers
- `sso_url` contains the value of _Login URL_
- `entity_issuer` contains the value of _Identifier (Entity ID)_

See the [SAML Configuration Settings]({{< relref "saml.md#saml-configuration-settings" >}}) for further configuration options.

{{< warning >}}
Azure AD lets you choose the _NameID_ field and also apply _transformations_ to it.
The SAML configuration setting of the selected value in "Choose name identifier format" in Azure AD must match the `name_id_policy_format` configuration in Chef Automate.
{{< /warning >}}

## SAML Configuration Settings

The SAML configuration settings are:

```toml
[dex.v1.sys.connectors.saml]
ca_contents = "<your ca contents>"          # required
sso_url = "<your SSO URL>"                  # required
email_attr = "<your email attribute>"       # required
username_attr = "<your username attribute>" # required
groups_attr = "<your groups attribute>"     # optional
allowed_groups = ["group1", "group 2"]      # optional
entity_issuer = "<your entity issuer>"      # optional
name_id_policy_format = "<see below>"       # optional
```

`ca_contents` must contain a copy of the certificate used to sign the SAML assertions.
The certificate should be a PEM-encoded string.
For example,

```toml
ca_contents = """-----BEGIN CERTIFICATE-----
MIIE+DCCAuCgAwIBAgIBATANBgkqhkiG9w0BAQsFADAcMRowGAYDVQQDExFDaGVm
[...]
s1V9oZ7+NcK8vtcdXhjB5N65LbPlaT3nbvXGIvsQmoGc+FQ5WI4agoNlofOCogdW
k2WFcoiiKyeIznNScx/K6AeykKR/lPrJedanSA==
-----END CERTIFICATE-----
"""
```

{{< warning >}}
The `groups_attr` setting is optional, but if not provided, users authenticating via SAML will not be members of any teams.
{{< /warning >}}

Setting `allowed_groups` provides SAML sign-in for members of the listed groups and discards all user groups that are _not_ in the list. Groups must be on the `allowed_groups` list to access Chef Automate.

For example, in the configuration example above, the users that belong to "group1" or "group2" may sign in to Chef Automate, and those groups would appear as `team:saml:group1` and `team:saml:group2`, respectively. Members of the unlisted "group 3" would not have access to Chef Automate.

Chef Automate supports using SAML to authenticate users and apply permissions to SAML groups. See [IAM Overview]({{< relref "iam_v2_overview.md" >}}).

Member expressions are required for externally managed users and teams, as well as API tokens. See [IAM_v2_Guide]({{< relref "iam_v2_guide#member-expressions" >}}).

```toml
[dex.v1.sys.connectors.saml]
  ca_contents = "<your ca contents>"
  sso_url = "<your SAML SSO URL>"

  ###
  # SAML Attributes to map to a user's Chef Automate session
  ###
  # Example: "email"
  email_attr = "<your email attribute>"
  # Example: "name"
  username_attr = "<your username attribute>"
  # Example: "groups"
  groups_attr = "<your groups attribute>"

  # Optional: Manually specify Chef Automate's Issuer value.
  #
  # When provided Chef Automate will include this as the Issuer value in the SAML
  # AuthnRequest. It will also override the redirectURI as the required audience
  # when evaluating AudienceRestriction elements in the response.
  # Example: "https://{{< example_fqdn "automate" >}}/dex/callback"
  entity_issuer = "<your entity issuer>"

  # Optional: Specify the NameIDPolicy to use
  #
  # When provided, Chef Automate will request a name ID of the configured format
  # in the SAML AuthnRequest.
  # Defaults to "urn:oasis:names:tc:SAML:2.0:nameid-format:persistent".
  #
  # Note: Even when configured otherwise, the username gathered from the SAML
  # response is _treated_ as persistent. So, if this is set to
  #    "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress"
  # and a user has changed their email address, they will be a _new_ user to Chef
  # Automate.
  name_id_policy_format = "urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified"
```

In your SAML Identity Provider (IdP), your Chef Automate instance needs to be referenced as a Service Provider (SP).
To do so, use `https://{{< example_fqdn "automate" >}}/dex/callback`.
The concrete configuration items differ between IdP products, but it is often something like "Assertion Consumption URI" or "Single sign on URL".
For "Audience URI" or "SP Entity ID", use the same address.

These values are accepted for `name_id_policy_format`:

- `urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress`
- `urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified`
- `urn:oasis:names:tc:SAML:1.1:nameid-format:X509SubjectName`
- `urn:oasis:names:tc:SAML:1.1:nameid-format:WindowsDomainQualifiedName`
- `urn:oasis:names:tc:SAML:2.0:nameid-format:encrypted`
- `urn:oasis:names:tc:SAML:2.0:nameid-format:entity`
- `urn:oasis:names:tc:SAML:2.0:nameid-format:kerberos`
- `urn:oasis:names:tc:SAML:2.0:nameid-format:persistent`
- `urn:oasis:names:tc:SAML:2.0:nameid-format:transient`

## Troubleshooting

### Error: Unsupported auth mode

Attempting to sign in with an unsupported IdP-supported SAML login causes the `unsupported auth mode` error.

To remedy this error, fall back to the standard SP-initiated login mode.

### Error: No attribute with name

This error occurs in case of Azure AD where groups attribute is included use below values in config patch

`
email_attr = "emailaddress"
username_attr = "username"
groups_attr = "http://schemas.microsoft.com/ws/2008/06/identity/claims/groups"
`

Chef Automate uses the [Dex](https://github.com/dexidp/dex) library to support SAML integrations. Dex does not support IdP-initiated SAML logins with of these IdPs.
This means that Chef Automate also cannot support IdP-initiated SAML logins with the IdPs:

- [Azure AD]({{< relref "#azure-ad" >}})
- Office365
- OKTA
- OneLogin
- Ping
- Tivoli Federated Identity Manager
