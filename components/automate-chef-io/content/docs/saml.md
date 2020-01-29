+++
title = "SAML"
description = "SAML Configuration"
date = 2018-05-11T09:27:09+00:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "configuring_automate"
    weight = 50
+++

## Authentication via Existing Identity Management Systems

Chef Automate can integrate with existing SAML services to authenticate users in Chef Automate, and thus use their existing group memberships to determine their Chef Automate permissions.

Chef Automate supports using both local users and externally managed users from an external identity provider (IdP).
Both _one_ LDAP service (or MSAD for simplified configuration of Active Directory setups) and _one_ SAML IdP can be used.
You do not need to configure an external IdP if you simply want to create users and teams local to Chef Automate.
See the [Users]({{< relref "users.md" >}}) documentation for additional information.

Chef Automate uses [Dex](https://github.com/dexidp/dex) to support SAML integrations.
To configure authentication for your Chef Automate installation, create a TOML file that contains the partial SAML configuration.
Then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

{{% warning %}}
You may only integrate one IdP using SAML and one IdP using LDAP at a time.
Chef Automate does not support using _two_ SAML IdPs or _two_ LDAP services simultaneously.
{{% /warning %}}

If you need to change your configured IdP, you will need to replace
your existing configuration by following these steps:

1. Run `chef-automate config show config.toml`.
2. Edit `config.toml` to replace the `dex.sys.connectors` section with the config values for your new identity provider.
3. Run `chef-automate config set config.toml` to set your updated config.

{{< info >}}
Users who sign in via SAML will have a session time of 24 hours before needing to sign in again.
Local, MSAD, and LDAP users will have their Chef Automate sessions refreshed while they maintain an active
browsing session of the Chef Automate UI or until they sign out directly.
{{< /info >}}

## Supported Identity Management Systems

- Office365
- OKTA
- OneLogin
- Ping
- Tivoli Federated Identity Manager

## SAML Configuration Settings

The SAML configuration settings are:

```toml
[dex.v1.sys.connectors.saml]
ca_contents = "<your ca contents>"          # required
sso_url = "<your SSO URL>"                  # required
email_attr = "<your email attribute>"       # required
username_attr = "<your username attribute>" # required
groups_attr = "<your groups attribute>"     # optional
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

{{% warning %}}
The `groups_attr` setting is optional. However, if it is not provided,
users authenticating via SAML will not be members of any teams.
{{% /warning %}}

Chef Automate supports using SAML to authenticate users and apply permissions to SAML groups. See [IAM v2 Overview]({{< relref "iam-v2-overview.md" >}}).

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
