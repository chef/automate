+++
title = "Configuration Overview"

date = 2018-05-08T18:54:09+00:00
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Overview"
    parent = "automate/configure"
    identifier = "automate/configure/configuration.md Configuration"
    weight = 10
+++

The `chef-automate` CLI provides commands to help you work with your existing Chef Automate configuration:

* `chef-automate config show` shows your current configuration, not including default settings
* `chef-automate config patch </path/to/partial-config.toml>` updates an existing Chef Automate configuration by merging the contents of`</path/to/partial-config.toml>` with your current Chef Automate configuration, and applying any changes. This command is enough in most situations
* `chef-automate config set </path/to/full-config.toml>` replaces the current Chef Automate configuration with the provided configuration and applies any changes. Use this command to replace your Chef Automate configuration.

Update your Chef Automate configuration by generating a configuration section and applying it with the `chef-automate config patch`. The rest of this document describes how to make common configuration changes.

## Use Cases

### Minimal Configuration

The `chef-automate init-config` command generates an annotated Chef Automate configuration file with the basic settings needed to deploy Chef Automate.
This section describes those settings and how to change them on an existing Chef Automate installation.

#### Chef Automate FQDN

Your Chef Automate fully qualified domain name (FQDN) is customizable. There isn't a maximum length for an FQDN, but the top-level domain length has a limit of 25 characters.

For reference, these are the parts of a URL:

```bash
https://automate.4thcafe.com
<scheme>://<subdomain>.<second-level domain>.<top-level domain>
```

To change the FQDN of your Chef Automate installation, create a TOML file that contains the partial configuration:

```TOML
[global.v1]
  fqdn = "{{< example_fqdn "automate" >}}"
```

Then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

#### Install Channel

Chef Automate consists of [Habitat]({{< relref "habitat">}}) packages installed from a release channel.
The default channel is `current`.

#### Upgrade Strategy

The upgrade strategy determines when a Chef Automate installation upgrades.
The upgrade strategy settings include:

* `at-once` (default) upgrades the installation after detecting new packages in the install channel
* `none` freezes the installation with its current set of packages

Changing the upgrade strategy from `none` to `at-once` will install the latest packages from the install channel.

To change the upgrade strategy of your Chef Automate installation, create a TOML file that contains the partial configuration:

```toml
[deployment.v1.svc]
upgrade_strategy = "at-once"
```

Then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

To upgrade a Chef Automate installation with `upgrade_strategy` set to `none`, run:

```bash
chef-automate upgrade run
```

This command will upgrade Chef Automate to the latest version from your install channel.

#### Deployment Type

Do not change `deployment_type`.
The only supported `deployment_type` is `local`.

#### Settings

During initial deployment, you cannot change the admin username, name, and password set.

To change the admin password after deployment, use the Chef Automate UI. Sign in as the admin user, and navigate to the _Users_ page under the **Settings** tab.
Select "Local Administrator" to show the admin's _User Details_ page. Navigate to the _Reset Password_ tab. Enter your previous password, and enter and confirm your new password in the interface. Select the **Reset Password** button to save your changes.

To change the admin password from the command-line, first [fetch the admin user record](/automate/api/), copy the User ID, and then use:

```bash
export TOKEN=`chef-automate iam token create admin-token-1 --admin`

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

You can apply for your Chef Automate license with the `chef-automate license apply` command in one of two ways:

* `chef-automate license apply </path/to/license-file.jwt>`
* `chef-automate license apply <content-of-license>`

After your initial deployment, you cannot apply for a license by patching the configuration file.

#### Proxy Settings

You can configure Chef Automate to use a proxy by setting environment variables or setting configuration options.

The command `chef-automate deploy` without a configuration file will respect the proxy environment variables:

* `HTTPS_PROXY`/`https_proxy`
* `HTTP_PROXY`/`http_proxy`
* `NO_PROXY`/`no_proxy` (See [Required Sites and Domains]({{< relref "#required-sites-and-domains" >}}).)

Setting these environment variables before the initial deployment of Chef Automate adds them to the configuration that Chef Automate generates.

If you provide a configuration file during deployment (`chef-automate deploy /path/to/config.toml`), you must specify any proxy settings in that configuration file.

```toml
[global.v1.proxy]
host = "<your proxy host>"
port = <your proxy port>
no_proxy = ["0.0.0.0", "127.0.0.1"]
# user = "<your proxy user>"
# password = "<your proxy password>"
```

To patch the proxy settings, create a TOML file that contains the `[global.v1.proxy]` section and settings.
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

Configure the log level for all Chef Automate services by creating a TOML file. By default, each service will initialize at the `info` level, but the following settings are available: `debug`, `info`, `warning`, `panic`, or `fatal`.

```toml
[global.v1.log]
level = "debug"
```

Then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

#### Sample Configuration

```toml
# This is a default Chef Automate configuration file. You can run
# 'chef-automate deploy' with this config file, and it should
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

#### General OpenSearch Configuration

To configure OpenSearch for your Chef Automate installation, create a TOML file that contains the partial configuration below.
Uncomment and change settings as needed, and then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

```toml
[opensearch.v1.sys.cluster]
# name = "chef-insights"
[opensearch.v1.sys.node]
# max_local_storage_nodes = 1
[opensearch.v1.sys.path]
# logs = "logs"
[opensearch.v1.sys.bootstrap]
# memory_lock = false
[opensearch.v1.sys.network]
# host = "0.0.0.0"
# port = 10141
[opensearch.v1.sys.transport]
# port = "10142"
[opensearch.v1.sys.action]
# destructive_requires_name = true
```

#### Setting OpenSearch Heap

The OpenSearch heap size can, and in most cases, should be set to 50% of the available system memory. However, you should consider the essential caveats covered in the [OpenSearch heap size documentation](https://opensearch.org/docs/latest/opensearch/install/important-settings/).

For example, a system with 32GB of memory can have its OpenSearch heap size set to `16g`; to do so, one would first create a TOML file that contains the partial
configuration below, and then run `chef-automate config patch </path/to/your-file.toml>` to deploy the change.

```toml
[opensearch.v1.sys.runtime]
heapsize = "16g"
```

#### PostgreSQL

To configure PostgreSQL for your Chef Automate installation, create a TOML file that contains the partial configuration below.
Uncomment and change settings as needed, with the following caveats:

* These configuration settings affect only the Chef Automate-deployed PostgreSQL database. They do not affect an [externally-deployed PostgreSQL database]({{< relref "install.md#configuring-an-external-postgresql-database" >}}).
* Chef Automate uses TLS mutual authentication to communicate with its PostgreSQL database.

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

To configure your Chef Automate installation's load balancer, create a TOML file that contains the partial configuration below.
Uncomment and change settings as needed, and then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

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
# ssl_ciphers = "ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384:!aNULL:!eNULL:!EXPORT:AES256-GCM-SHA384"
# ssl_protocols = "TLSv1.2"
# tcp_nodelay = "on"
# tcp_nopush = "on"
# enable_csp_header = false
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

#### Buffer Size

Configure message buffer ingest size:

```toml
[compliance.v1.sys.service]
message_buffer_size = 200
[ingest.v1.sys.service]
message_buffer_size = 200
```

#### Compliance Configuration

To configure your Chef Automate InSpec agent scans, create a TOML file that contains the partial configuration below.
Uncomment and change settings as needed, and then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

```toml
[compliance.v1.sys.agent]
## Max number of inspec workers to run in parallel for detect and scan jobs. Default: 10
# workers = 20
## Max number of detect and scan jobs that can be accepted in the jobs workers queue. Default: 1000
# buffer_size = 2000
## Option to specify the version of inspec to use for remote(e.g. AWS SSM) scan jobs
# remote_inspec_version = "4.3.2"
## A control result message that exceeds this character limit will be truncated. Default: 10000
# result_message_limit = 20000
## The array of results per control will be truncated at this limit to avoid large reports that cannot be processed. Default: 50
# control_results_limit = 100
## Control results that have a `run_time` (in seconds) below this limit will be stripped of the `start_time` and `run_time` fields. Default: 1.0
# run_time_limit = 0.5
```

#### Encrypt Cookies with Custom Secret Key in OC-ID Service

Now, you can configure and integrate an existing private Supermarket with Chef Automate. `secret_key_base` is an attribute introduced as optional setting in OC-ID service of Automate which will be used to encrypt the cookies and other information. By default a unique `secret_key_base` gets generated internally for the OC-ID service running as part of Chef Automate. If you want to set it to something custom you can assign it a random string which will be used by OC-ID as the `secret_key_base`. Below is the syntax to set the configuration for the OC-ID service.

```toml
[ocid.v1.sys.ocid]
    secret_key_base = ""
```

#### Configure Inflight Data Collector Request Maximum

You can specify the maximum number of inflight data collector requests. The default value is sixty times the number of the machine's available CPUs.

```toml
[gateway.v1.sys.data_collector.limiter]
# Setting disable to true will allow an unbounded number of
# data collector requests to remain inflight concurrently.
disable = false
# max_inflight_requests will set the maximum number of
# concurrent inflight data collector requests. By default,
# this value is 60 * runtime.CpuCount()
max_inflight_requests = 100
```

#### Sign-out on Browser Close

When you close the browser, the configuration to sign out from Chef Automate is:

```toml
[session.v1.sys.service]
  # Setting persistent to false will disable persistent sessions.
  # Users will be signed out when their browser closes.
  persistent = false
```

#### Disclosure Banner

Configuration displays a custom banner on every Chef Automate page, including the sign-in page. Default: `false`.

```toml
[global.v1]
  [global.v1.banner]
    show = true # Set 'show' to 'true' to enable the banner. Set to 'false' to disable the banner. Default: false.

    message = "Lorem ipsum dolor sit amet" # Add the Message for the banner
    background_color = "3864f2" # Set the background color using the Hex Color Code (Do not add # to the code)
    text_color = "FFF" # Set the color of the text using the Hex Color Code (Do not add # to the code)

    # Find valid HEX codes at https://htmlcolorcodes.com/
```

#### Disclosure Panel

Configuration to display a disclosure on the sign-in page. Requires a `.txt` or `.html` message stored in a location accessible to Chef Automate. Default: `false`.

```toml
[global.v1]
  [global.v1.disclosure]
    show = true # Set 'show' to 'true' to enable the disclosure panel on the sign in page. Set to 'false' to disable the disclosure panel. Default: false.

    message_file_path = "/src/dev/disclosure-panel-message.txt" # The location of the file containing the disclosure panel message.

    # Validate your HTML at https://validator.w3.org/
```

### Content Security Policy Header

Content-Security-Policy is the name of a HTTP response header that modern browsers use to enhance the document's security(or web page). The Content-Security-Policy header allows you to restrict how resources such as JavaScript, CSS, or anything that the browser loads.

Refer: https://owasp.org/www-community/controls/Content_Security_Policy

In Chef Automate, enable the Content Security Policy header by patching the following configuration.

```toml
[load_balancer.v1.sys.ngx.http]
  enable_csp_header = true
```

{{< warning >}}
Enabling the CSP header may break the SAML login. This may happen if the IDP Login page has inline javascript, which the CSP header prevents from getting evaluated by default.
{{< /warning >}}

### Troubleshooting

Common syntax errors may cause issues in configuration files:

* Keys: Names use underscores, not dashes.
* Ports: Use the correct type. Single numbers are integers and don't need quotation marks. Ranges are strings and require quotation marks.
* Whitespace: Both tabs and spaces are whitespaces.
* Arrays: Use square brackets with comma-separated entries of the same type.

See the [TOML README](https://github.com/BurntSushi/toml-1) for more details.
