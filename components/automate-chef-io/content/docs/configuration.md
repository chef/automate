+++
title = "Configuration"
description = "Configuring Chef Automate"
date = 2018-05-08T18:54:09+00:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "configuring_automate"
    weight = 10
+++

# Overview

The `chef-automate` CLI provides commands to help you work with your existing Chef Automate configuration:

* `chef-automate config show` shows you your current configuration, with the exception of default settings
* `chef-automate config patch </path/to/partial-config.toml>` updates an existing Chef Automate configuration by merging the contents of`</path/to/partial-config.toml>` with your current Chef Automate configuration, and applying any changes. This command is sufficient in most situations
* `chef-automate config set </path/to/full-config.toml>` replaces the current Chef Automate configuration with the provided configuration, and applies any changes. Use this command to replace your Chef Automate configuration

Update your Chef Automate configuration by generating a section of a configuration, and applying it with `chef-automate config patch`. The rest of this document describes how to make common configuration changes.

## Use Cases

### Minimal Configuration

The `chef-automate init-config` command generates an annotated Chef Automate configuration file with the minimum settings needed to deploy Chef Automate. This section describes those settings and how to change them on an existing Chef Automate installation.

#### Chef Automate FQDN

To change the fully qualified domain name (FQDN) of your Chef Automate installation, create a TOML file that contains the partial configuration:

```TOML
[global.v1]
  fqdn = "{{< example_fqdn "automate" >}}"
```

Then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

#### Install Channel

Chef Automate is made up of [Habitat](https://www.habitat.sh/) packages installed from a release channel. The default channel is `current`, but we will introduce additional channels in the future.

#### Upgrade Strategy

The upgrade strategy determines when a Chef Automate installation is upgraded. The upgrade strategy can be set to:

* `at-once` (default) upgrades the installation when new packages are detected in your install channel
* `none` freezes the installation with its current set of packages

Changing the upgrade strategy from `none` to `at-once` will install the latest packages from your install channel.

To change the upgrade strategy of your Chef Automate installation, create a TOML file that contains the partial configuration:

```toml
[deployment.v1.svc]
upgrade_strategy = "at-once"
```

Then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

To upgrade a Chef Automate installation when the `upgrade_strategy` is set to `none`, run:

```bash
chef-automate upgrade
```

This will upgrade Chef Automate to the latest version from your install channel.

#### Deployment Type

Do not change `deployment_type` at this time. Currently, the only supported `deployment_type` is `local`.

#### Settings

Currently, you cannot change the admin username, name, and password set during initial deployment.

To change the admin password after deployment, use the Automate UI.
Log in as the admin user, navigate to the _User_ page under the **Settings**  tab. Selecting "Local Administrator" opens a form for updating the password. Enter and confirm your new password in the interface, and then select the **Update Password** button to save your changes.

To change the admin password from the command-line, first
[fetch the admin user record]({{< relref "users.md#fetching-users" >}})
and copy the User ID, then use:

```bash
export TOKEN=`chef-automate admin-token`

curl -X PUT -H "api-token: $TOKEN" -H "Content-Type: application/json" -d '{"name":"Local Administrator", "id": "<admin user ID>", "password":"<password>"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/users/admin?pretty
```

#### Load Balancer Certificate and Private Key

To change the load balancer certificate and private key of your Chef Automate installation, create a TOML file that contains the partial configuration:

```toml
[[global.v1.frontend_tls]]
# The TLS certificate for the load balancer frontend.
cert = """-----BEGIN CERTIFICATE-----
<your certificate>
-----END CERTIFICATE-----
"""

# The TLS RSA key for the load balancer frontend.
key = """-----BEGIN RSA PRIVATE KEY-----
<your private key>
-----END RSA PRIVATE KEY-----
"""
```

Then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

#### License Key

You can apply your Chef Automate license with the `chef-automate license apply` command in one of two ways:

* `chef-automate license apply </path/to/license-file`
* `chef-automate license apply <content-of-license>`

Currently you cannot apply a license after your initial deployment by patching the configuration file.

#### Proxy Settings

You can configure Chef Automate to use a proxy either by setting environment variables, or by setting configuration options.

Chef Automate respects the proxy environment variables:

* `HTTPS_PROXY`/`https_proxy`
* `HTTP_PROXY`/`http_proxy`
* `NO_PROXY`/`no_proxy` (See [Required Sites and Domains]({{< relref "#required-sites-and-domains" >}}).)

Setting these environment variables prior to initial deployment of Chef Automate
adds them to the configuration.

To set a proxy by editing a configuration file, create a TOML file that contains the partial configuration:

```toml
[global.v1.proxy]
host = "<your proxy host>"
port = <your proxy port>
no_proxy = ["0.0.0.0", "127.0.0.1"]
# user = "<your proxy user>"
# password = "<your proxy password>"
```

Then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

##### Required Sites and Domains

Chef Automate must be able to access the following:

* `packages.chef.io`
* `licensing.chef.io`
* `raw.githubusercontent.com`
* `api.bintray.com`
* `bldr.habitat.sh`
* `akamai.bintray.com`
* `dl.bintray.com`
* `bintray.com`
* `localhost`
* `127.0.0.1`
* `0.0.0.0`

#### Global Log Level

Configure the log level for all Chef Automate services by creating a TOML file. By default each service will initialize at the `info` level, but the following settings are available: `debug`, `info`, `warning`, `panic`, or `fatal`.

```toml
[global.v1.log]
level = "debug"
```

Then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

#### Sample Configuration

```toml
# This is a default Chef Automate configuration file. You can run
# 'chef-automate deploy' with this config file and it should
# successfully create a new Chef Automate instance with default settings.

[global.v1]
# The external fully qualified domain name.
 # When the application is deployed you should be able to access 'https://<fqdn>/'
  # to login.
  fqdn = "chef-automate.test"

  # The following TLS certificate and RSA public key were
  # automatically generated. The certificate is a self-signed
  # certificate and will likely produce security warnings when you
  # visit Chef Automate in your web browser. We recommend using a
  # certificate signed by a certificate authority you trust.
  [[global.v1.frontend_tls]]
    # The TLS certificate for the load balancer frontend.
    cert = """-----BEGIN CERTIFICATE-----
<the load balancer's certificate>
-----END CERTIFICATE-----
"""

    # The TLS RSA key for the load balancer frontend.
    key = """-----BEGIN RSA PRIVATE KEY-----
<the load balancer's TLS RSA key>
-----END RSA PRIVATE KEY-----
"""

# Deployment service configuration.
[deployment.v1]
  [deployment.v1.svc]
    # Habitat channel to install hartifact from.
    # Can be 'dev', 'current', or 'acceptance'
    channel = "current"
    upgrade_strategy = "at-once"
    deployment_type = "local"

```

### Additional Configuration

#### Authentication via Existing Identity Management Systems

Chef Automate can integrate with LDAP or SAML to authenticate your users against Chef Automate, and thus use their existing group memberships to determine their Chef Automate permissions.

Chef Automate supports using both local users and externally managed users from an external identity provider (IdP).
Both _one_ LDAP service (or MSAD for simplified configuration of Active Directory setups) and _one_ SAML IdP can be used.
You do not need to configure an external IdP if you simply want to create users and teams local to Chef Automate.
See the [Users]({{< relref "users.md" >}}) documentation for additional information.

Chef Automate uses [Dex](https://github.com/dexidp/dex) to support LDAP and SAML integrations. To configure authentication for your Chef Automate
installation, create a TOML file that contains the partial configuration for either LDAP or SAML. Then run
`chef-automate config patch </path/to/your-file.toml>` to deploy your change.

{{% warning %}}
You may only integrate one IdP using SAML and one IdP using LDAP at a time.
Chef Automate does not support using _two_ SAML IdPs or _two_ LDAP services simultaneously.
{{% /warning %}}

If you need to change your configured IdP, you will need to replace
your existing configuration by following these steps:

1. Run `chef-automate config show config.toml`.
2. Edit `config.toml` to replace the `dex.sys.connectors` section with the config values for your new identity provider.
3. Run `chef-automate config set config.toml` to set your updated config.

Switching between a Microsoft AD configuration and generic LDAP configuration will
not affect your [policies]({{< relref "authorization-overview.md" >}}), as they are both LDAP configurations.
However, switching between either of those configurations and a SAML configuration will
require you to remake your policies, as the [subjects]({{< relref "authorization-overview.md#structure" >}}) of those
policies will be incorrectly labelled as `ldap`.

{{< info >}}
Users who log in via SAML will have a session time of 24 hours before needing to log in again.
Local, MSAD, and LDAP users will have their Chef Automate sessions refreshed while they maintain an active
browsing session of the Chef Automate UI or until they log out directly.
{{< /info >}}

##### Microsoft Active Directory

Chef Automate comes with a default LDAP configuration for Microsoft Active Directory (MSAD).
The MSAD configuration is intended for getting started with minimal configuration for standard MSAD systems.
Default values in this MSAD configuration can be overridden.

{{< warning >}}
Chef Automate's default configuration for Microsoft AD is specific to LDAP.
To configure Microsoft AD using SAML, see the [SAML configuration section]({{< relref "configuration.md#saml" >}}).
{{< /warning >}}

In this config, the required fields are `host`, `bind_dn`,
`bind_password`, `base_user_search_dn`, and `base_group_search_dn`.
See [LDAP Integration]({{< relref "ldap.md" >}}) for more information on config fields, including optional fields.

```toml
[dex.v1.sys.connectors.msad_ldap]
host = "<your host>"
bind_dn = "<your bind_dn>"
bind_password = "<your bind_password>"
base_user_search_dn = "<your base user search DN>"
base_group_search_dn = "<your base group search DN>"
ca_contents = "<your ca contents>" # optional, but recommended
```

The MSAD configuration is an LDAP configuration with more provided default
values that are commonly a good fit for Active Directory. Override any single default value by uncommenting it in the config and setting its value:

```toml
[dex.v1.sys.connectors.msad_ldap]
host = "<your host>"
bind_dn = "<your bind_dn>"
bind_password = "<your bind_password>"
base_user_search_dn = "<your base user search DN>"
base_group_search_dn = "<your base group search DN>"
ca_contents = "<your ca contents>" # optional

# MSAD default values (uncomment to override a specific one)
# insecure_no_ssl = false
# user_query_filter = "(objectClass=person)"
# user_id_attr = "sAMAccountName"
# username_attr = "sAMAccountName"
# email_attr = "mail"
# user_display_name_attr = "displayName"
# group_query_filter = "(objectClass=group)"
# filter_groups_by_user_value = "DN"
# filter_groups_by_user_attr = "member"
# group_display_name_attr = "displayName"
```

{{< warning >}}
Connecting to an LDAP service without TLS is not recommended.
{{< /warning >}}

However, if you wish to integrate with an LDAP server with TLS disabled:

```toml
insecure_no_ssl = true
```

##### LDAP

For those who do not use Microsoft AD or require greater control over their configuration,
Chef Automate has the following customizable LDAP configuration settings:

```toml
[dex.v1.sys.connectors.ldap]
ca_contents = "<your ca contents>"
host = "<your host>"
bind_dn = "<your bind_dn>"
bind_password = "<your bind_password>"
insecure_no_ssl = true or false
base_user_search_dn = "<your base user search DN>"
user_query_filter = "<your user query filter>"
username_attr = "<your username attribute>"
user_id_attr = "<your userid attribute>"
email_attr = "<your email attribute>"
user_display_name_attr = "<your user display name attribute>"
base_group_search_dn = "<your base group search DN>"
group_query_filter = "<your group query filter>"
filter_groups_by_user_attr = "<groups to filter by user attribute>"
filter_groups_by_user_value = "<groups to filter by user value>"
group_display_name_attr = "<group display name attribute>"
```

See the [LDAP]({{< relref "ldap.md" >}}) for more information on configuration fields.
You have the full extent of TOML is at your disposal for declaring configuration fields.

{{< warning >}}
Connecting to an LDAP service without TLS is not recommended.
{{< /warning >}}

However, if you wish to integrate with an LDAP server with TLS disabled:

```toml
insecure_no_ssl = true
```

See below for the full config and additional details about all LDAP configuration options.

```toml
[dex.v1.sys.connectors.ldap]
  ###
   # Configuration for querying your LDAP server
   ###
   ca_contents = "<your ca contents>"
   host = "<your host>"

   # The DN and password you wish to bind to your LDAP server to search for
   # users to authenticate for Chef Automate (and also to search for their group membership).
   # Example: "uid=seviceaccount,cn=users,dc=example,dc=com"
   bind_dn = "<your bind_dn>"
   bind_password = "<your bind_password>"

   ###
   # User Query (search for LDAP users to authenticate for Chef Automate)
   ###
   # The base DN to start the user query.
   # Chef Automate will use this as the base DN on which to search for users to authenticate against your LDAP server.
   # Example: "cn=users,dc=example,dc=com"
   base_user_search_dn = "<your base user search DN>"

   # The LDAP field used to filter the query for users to authenticate for Chef Automate.
   # Example: Setting this to "uid" would result in a filter of "(uid=<username_for_user_trying_to_authenticate>)".
   username_attr = "<your username attribute>"

   # Optional: LDAP query filter to apply when searching for users to authenticate.
   # This will be combined with username_attr filter above.
   # Example: Setting this to "(objectClass=person)" will filter on human actors only.
   user_query_filter = "<your user query filter>"

   ###
   # Populating the Chef Automate User via LDAP
   ###
   # Determines which LDAP field populates the username in a user's Chef Automate session on successful authentication.
   user_id_attr = "<your userid attribute>"

   # Optional: determines which LDAP field populates the email in a user's Chef Automate session on successful authentication.
   # Defaults to "user_id_attr" if not specified.
   email_attr = "<your email attribute>"

   # Optional: determines which LDAP field populates the display name in a user's Chef Automate session on successful authentication.
   # Defaults to "name" if not specified.
   user_display_name_attr = "<your user display name attribute>"

   ###
   # Group Query (search for LDAP group membership for an authenticated user)
   ###
   # The base DN to start the group membership query.
   # Chef Automate will use this as the base DN on which to search for LDAP group membership for a specific LDAP user.
   # Example: "cn=groups,dc=freeipa,dc=example,dc=com"
   base_group_search_dn = "<your base group search DN>"

   # The following two fields are used to match a user to a group.
   # If the defaults are used, then you end up with a group membership
   # filter of "(&(objectClass=group)(member=<user's DN>))".
   # Optional: The LDAP field by which you wish to filter group membership.
   # Defaults to "member".
   filter_groups_by_user_attr = "<groups to filter by user attribute>"
   # Optional: The LDAP field from the authenticated user you wish to use as input to the above filter.
   # Defaults to "DN".
   filter_groups_by_user_value = "<groups to filter by user value>"

   # Optional: Additional LDAP filter you can define to further filter group membership results.
   group_query_filter = "<your group query filter>"

   # The LDAP field on the group you wish to use as the Chef Automate Team name for the group.
   # Defaults to "name".
   group_display_name_attr = "<group display name attribute>"
```

Additional notes regarding the configuration of Chef Automate's LDAP integration can be found [here]({{< relref "ldap.md" >}}).

##### SAML

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

{{< warning >}}
The `groups_attr` setting is optional. However, if it's not provided,
users authenticating via SAML will not be members of any teams.
{{< /warning >}}

Chef Automate supports using SAML to authenticate users and [applying permissions to SAML groups]({{< relref "authorization-overview.md" >}}). SAML is a simpler protocol, so the configuration options for it are also simpler:

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
  # and a user has changed their email address, they'll be a _new_ user to Chef
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

#### Alpha: Setting up Automate as an OAuth Provider for Habitat Builder

{{% warning %}}
Authentication via Automate for Habitat Builder is still in Alpha.
{{% /warning %}}

To configure Chef Automate as an OAuth Provider for Habitat Builder, create a TOML file
that contains the partial configuration below.
Run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

`bldr_client_id` and `bldr_client_secret` simply need to match what you configured for the corresponding
values in Habitat Builder (see below). However, we strongly recommend those values follow
[best practices](https://www.oauth.com/oauth2-servers/client-registration/client-id-secret/)
for `client_id` and `client_secret` in the Oauth2 standard.

```toml
[session.v1.sys.service]
bldr_signin_url = "<your Builder fqdn>" # for example, "http://builder.test/"

# This needs to match what you configured OAUTH_CLIENT_ID as when you configured Habitat Builder.
bldr_client_id = "<your Habitat Builder Oauth2 Client ID>"

# This needs to match what you configured OAUTH_CLIENT_SECRET as when you configured Habitat Builder.
bldr_client_secret = "<your Habitat Builder Oauth2 Client Secret>"
```

You'll need to add Automate's TLS certificate to Builder's list of accepted certificates
in addition to these configuration changes.
Locate Automate's default self-signed certificate by running `cat /hab/svc/automate-load-balancer/data/{{< example_fqdn "automate" >}}.cert`
You can copy this default certificate then add it to your Builder instance's list of accepted certs.

```text
-----BEGIN CERTIFICATE-----
MIIDfDCCAmSgAcasldkaf...
-----END CERTIFICATE-----
```

If you are using a certificate signed by a trusted certificate authority instead of the default certificate,
you can provide Builder with the root certificate authority for the signed certificate.

Further explanation on how to configure Builder to authenticate via Chef Automate can be found [here](https://github.com/habitat-sh/on-prem-builder).

#### General Elasticsearch Configuration

To configure Elasticsearch for your Chef Automate installation, create a TOML file that contains the partial configuration below. Uncomment and change settings as needed, then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

```toml
[elasticsearch.v1.sys.proxy]
# NOTE: The elasticsearch proxy settings are derived from the global proxy settings.
# host = "<proxy host>"
# port = <proxy port>
# user = "<proxy username>"
# password = "<proxy password>"
# no_proxy = <["0.0.0.0", "127.0.0.1"]>
[elasticsearch.v1.sys.cluster]
# name = "chef-insights"
[elasticsearch.v1.sys.cluster.routing.allocation]
# node_concurrent_recoveries = 2
# node_initial_primaries_recoveries = 4
# same_shard_host = false
[elasticsearch.v1.sys.node]
# max_local_storage_nodes = 1
# master = true
# data = true
[elasticsearch.v1.sys.path]
# logs = "logs"
[elasticsearch.v1.sys.indices.recovery]
# max_bytes_per_sec = "20mb"
[elasticsearch.v1.sys.indices.breaker]
# total_limit = "70%"
# fielddata_limit = "60%"
# fielddata_overhead = "1.03"
# request_limit = "40%"
# request_overhead = "1"
[elasticsearch.v1.sys.bootstrap]
# memory_lock = false
[elasticsearch.v1.sys.network]
# host = "0.0.0.0"
# port = 10141
[elasticsearch.v1.sys.transport]
# port = "10142"
[elasticsearch.v1.sys.discovery]
# ping_unicast_hosts = "[]"
# minimum_master_nodes = 1
# zen_fd_ping_timeout = "30s"
[elasticsearch.v1.sys.gateway]
# expected_nodes = 0
# expected_master_nodes = 0
# expected_data_nodes = 0
[elasticsearch.v1.sys.action]
# destructive_requires_name = true
[elasticsearch.v1.sys.logger]
# level = "info"
[elasticsearch.v1.sys.runtime]
# max_locked_memory = "unlimited"
# es_java_opts = ""
# NOTE: heapsize should be set to 50% of available RAM up to 32g
# See: https://www.elastic.co/guide/en/elasticsearch/guide/current/heap-sizing.html
# heapsize = "1g"
```

#### Setting Elasticsearch Heap

Per the Elasticsearch documentation, the [Elasticsearch heap size](https://www.elastic.co/guide/en/elasticsearch/guide/current/heap-sizing.html) should be up to 50% of the available RAM, up to 32GB. To set the Elasticsearch heap size, create a TOML file that contains the partial configuration below.
Uncomment and change settings as needed, then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

```toml
[elasticsearch.v1.sys.runtime]
# heapsize = "<your Elasticsearch heapsize>"
```

#### PostgreSQL

To configure PostgreSQL for your Chef Automate installation, create a TOML file that contains the partial configuration below. Uncomment and change settings as needed, with the following caveats:

* Chef Automate currently does not support use of an external Postgres database
* Chef Automate uses TLS mutual authentication to communicate with its Postgres database

Then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

```toml
[postgresql.v1.sys.service]
# host = "0.0.0.0"
# port = 5432
[postgresql.v1.sys.pg]
# md5_auth_cidr_addresses = ["0.0.0.0/0", "::0/0"]
# max_wal_size = "1GB"
# min_wal_size = "80MB"
# wal_keep_segments = 32
# checkpoint_timeout = "5min"
# checkpoint_completion_target = 0.5
# max_connections = 100
# max_locks_per_transaction = 64
[postgresql.v1.sys.logger]
# level = "ERROR"
[postgresql.v1.sys.superuser]
# name = "automate"
```

#### Load Balancer

To configure your Chef Automate installation's load balancer, create a TOML file that contains the partial configuration below. Uncomment and change settings as needed, then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

```toml
[load_balancer.v1.sys.service]
# https_port = 443
# http_port = 80
# NOTE: the external_fqdn setting is derived from the global settings and
# should be configured via the `[global.v1]` setting.
# external_fqdn = "<your Chef Automate fqdn>"
[load_balancer.v1.sys.log]
# level = "info"
[load_balancer.v1.sys.ngx.main]
# worker_processes = 4
# error_log = "/dev/stderr"
[load_balancer.v1.sys.ngx.events]
# worker_connections = 1024
# worker_processor_method = "epoll"
# multi_accept = "on"
[load_balancer.v1.sys.ngx.http]
# access_log = "/dev/stdout"
# client_max_body_size = "250m"
# default_type = "application/octet-stream"
# keepalive_timeout = 60
# keepalive_requests = 10000
# gzip = "on"
# gzip_comp_level = "2"
# gzip_disable = "MSIE [1-6]\\."
# gzip_http_version = "1.0"
# gzip_min_length = 10240
# gzip_proxied = "expired no-cache no-store private auth"
# gzip_types = "text/plain text/css text/xml text/javascript application/x-javascript application/xml"
# gzip_vary = "on"
# large_client_header_buffers_size = "8k"
# large_client_header_buffers_number = 4
# sendfile = "on"
# ssl_ciphers = "ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256:AES256-GCM-SHA384:!aNULL:!eNULL:!EXPORT"
# ssl_protocols = "TLSv1.2"
# tcp_nodelay = "on"
# tcp_nopush = "on"
[load_balancer.v1.sys.proxy]
# NOTE: The load_balancer proxy settings are derived from the global proxy settings.
# host = "<proxy host>"
# port = <proxy port>
# user = "<proxy username>"
# password = "<proxy password>"
# no_proxy = <["0.0.0.0", "127.0.0.1"]>
[[load_balancer.v1.sys.frontend_tls]]
# NOTE: the load_balancer TLS certificate settings are derived from global
# settings and should be configured via `[[global.v1.frontend_tls]]` settings
# server_name = "<your Chef Automate server name>"
# cert = "-----BEGIN CERTIFICATE-----\n<your load balancer cert>\n-----END CERTIFICATE-----\n"
# key = "-----BEGIN RSA PRIVATE KEY-----\n<your load balancer private key>\n-----END RSA PRIVATE KEY-----\n"
```

#### Data Retention

The bulk of Chef Automate's data is stored by the ingest, event-feed, and compliance services.
The retention policies for each service can be modified using the service's
gRPC `Purge.Configure` interface.

To configure each service's retention policies run the following commands with the
configuration request tailored to your specific needs. The `recurrence` field can be set
to any valid recurrence rule [as defined in section 4.3.10 of RFC 2445](https://www.ietf.org/rfc/rfc2445.txt).
Any omitted fields will not be updated or overwritten.

```bash
chef-automate dev grpcurl compliance-service -- chef.automate.infra.data_lifecycle.api.Purge.Configure -d '{
  "enabled":true,
  "recurrence":"FREQ=DAILY;DTSTART=20190820T221315Z;INTERVAL=1",
  "policy_update": {
    "es": [
      {
        "disabled": false,
        "policy_name":"compliance-scans",
        "older_than_days":"60"
      },
      {
        "disabled": false,
        "policy_name":"compliance-reports",
        "older_than_days":"60"
      }
    ]
  }
}'

chef-automate dev grpcurl ingest-service -- chef.automate.infra.data_lifecycle.api.Purge.Configure -d '{
  "enabled":true,
  "recurrence":"FREQ=DAILY;DTSTART=20190820T221315Z;INTERVAL=1",
  "policy_update": {
    "es": [
      {
        "disabled": false,
        "policy_name":"converge-history",
        "older_than_days":"30",
      },
      {
        "disabled": false,
        "policy_name":"actions",
        "older_than_days":"30",
      }
    ]
  }
}'

chef-automate dev grpcurl event-feed-service -- chef.automate.infra.data_lifecycle.api.Purge.Configure -d '{
  "enabled":true,
  "recurrence":"FREQ=DAILY;DTSTART=20190820T221315Z;INTERVAL=1",
  "policy_update": {
    "es": [
      {
        "disabled": false,
        "policy_name":"feed",
        "older_than_days":"7",
      }
    ]
  }
}'

```

To see the current retention policies for each service use the gRPC `Purge.Show` interface.
```bash
chef-automate dev grpcurl event-feed-service -- chef.automate.infra.data_lifecycle.api.Purge.Show
```

Substitute `event-feed-service` with `ingest-service` or `compliance-service`
to see each service's retention policies.

To immediately run a purge for a service use `Purge.Run` interface.
```bash
chef-automate dev grpcurl event-feed-service -- chef.automate.infra.data_lifecycle.api.Purge.Run
```

Substitute `event-feed-service` with `ingest-service` or `compliance-service`
to run each service's retention policies.

### Troubleshooting

Common syntax errors may cause issues in configuration files:

* Keys: Names use underscores, not dashes.
* Ports: Use the correct type. Single numbers are integers and don't need quotation marks. Ranges are strings and require quotation marks.
* Whitespace: Both tabs and spaces are whitespace.
* Arrays: Use square brackets with comma-separated entries of the same type.

See the [TOML README](https://github.com/BurntSushi/toml-1) for more details.
