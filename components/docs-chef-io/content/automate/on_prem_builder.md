+++
title = "Install Chef Habitat Builder On-prem"

date = 2019-11-19T14:10:15-08:00

draft = false

[menu]
  [menu.automate]
    title = "Install Chef Habitat Builder On-prem"
    parent = "automate/getting_started"
    identifier = "automate/getting_started/on_prem_builder.md Install Chef Habitat Builder On-prem"
    weight = 50
+++

[\[edit on GitHub\]](https://github.com/chef/automate/blob/master/components/docs-chef-io/content/automate/on_prem_builder.md)

Enterprise customers may wish to set up an on-premises Chef Habitat Builder to store Chef Habitat packages for use by their own Chef Habitat Studios and Supervisors.
This guide details how to install Chef Habitat Builder on-prem using the Chef Automate installer.
The Chef Automate installer includes everything necessary to get started with Chef Automate and Chef Habitat Builder on-prem.

Using the Chef Automate installer enables you to use Chef Automate's authentication stack, configuration, and backup/restore mechanisms to operate your Chef Habitat Builder installation.

For Chef Habitat Builder installations created prior to integration with the Chef Automate installer, see the [documentation in the github repo](https://github.com/habitat-sh/on-prem-builder) instead.

## System requirements

Outside the boundaries of a proof-of-concept project, we recommend against running installations of Chef Automate and Chef Habitat Builder on the same host. Please contact your Chef representative before using this implementation in production.

### Hardware Requirements

The minimum with which Chef Automate and Chef Habitat Builder can be deployed on a single host is:

* 16 GB of RAM.
* 130 GB of disk space, available to /hab.
* 4 vCPUs.
* Inbound network connectivity from LAN (HTTP/HTTPS) is required for internal clients to access the on-premises Chef Habitat Builder.

For deployments that are expected to see production-scale workload, we recommend:

* 64 GB of RAM.
* 200 GB of diskspace, available to /hab.
* 16 vCPUs.

Roughly 80 GB of the disk space is designated for Chef Automate; the rest is used for
Chef Habitat Builder and the artifacts it stores. The current implementation uses Minio for
Chef Habitat artifact storage; we do not support using Artifactory for artifact storage.

### Operating System

Chef Automate and Chef Habitat Builder require:

* a Linux kernel of version 3.2 or greater
* `systemd` as the init system
* `useradd`
* `curl` or `wget`
* The shell that starts Chef Automate should have a max open files setting of at least 65535
* Run the installation and bootstrapping procedures as the superuser or use `sudo` at the start of each command.

### Unsupported Topologies

* high-availability/DR/multinode Builder

## Get Started Installing Chef Habitat Builder

### Download the Chef Automate Installer

Download and unzip the installer:

```shell
curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
```

### Deploy Chef Habitat Builder

Deploying Chef Habitat Builder using the Chef Automate installer requires a Chef Automate license.
If you already have a Chef Automate license, you may use it for the deployment.
Otherwise, you can accept the 30-day trial license when you first sign in to Chef Automate.

If you are deploying Chef Habitat Builder in an airgapped environment, follow [the documentation on building an airgap bundle]({{< relref "airgapped_installation.md" >}}).

You can [deploy Chef Automate and Chef Habitat Builder at the same time]({{< relref "on_prem_builder.md#deploy-chef-automate-and-chef-habitat-builder" >}} ), or you can deploy [Chef Habitat Builder standalone]({{< relref "on_prem_builder.md#deploy-standalone-chef-habitat-builder" >}}). You can also [add Chef Automate to a standalone Chef Habitat Builder installation]({{< relref "on_prem_builder.md#add-chef-automate-to-a-standalone-chef-habitat-builder-installation" >}}), or [add Chef Habitat Builder to a Chef Automate installation]({{< relref "on_prem_builder.md#add-chef-habitat-builder-to-a-chef-automate-installation" >}}).

#### Deploy Chef Automate and Chef Habitat Builder

1. To customize the FQDN or other configuration values for your installation, create a `config.toml` file with default values for your Chef Automate and Chef Habitat Builder installation:

    ```shell
    sudo ./chef-automate init-config
    ```

    Edit the `config.toml` to set your desired FQDN and [other configuration values]({{< relref "configuration.md#minimal-configuration" >}}).

1. To deploy Chef Automate and Chef Habitat Builder at the same time, specify both the `builder` and `automate` products on the command line:

    ```shell
    ./chef-automate deploy --product builder --product automate config.toml
    ```

    If you changed no configuration values in the previous step, run

    ```shell
    ./chef-automate deploy --product builder --product automate
    ```

    Accept the license with `y`.

Chef Habitat Builder and Chef Automate use the same mechanisms for [user authentication]({{< relref "users.md" >}}), [configuration]({{< relref "configuration.md" >}}), [backups]({{< relref "backup.md" >}}), [log management]({{< relref "log_management.md" >}}), and [uninstalling]({{< relref "troubleshooting.md#uninstalling-chef-automate" >}}). In particular, Chef Automate backups contain Chef Habitat Builder data as well as packages stored in Chef Habitat Builder.

#### Deploy Standalone Chef Habitat Builder

1. To customize the FQDN or other configuration values for your installation, create a `config.toml` file with default values for your standalone Chef Habitat Builder installation:

    ```shell
    sudo ./chef-automate init-config
    ```

    Edit the `config.toml` to set your desired FQDN and [other configuration values]({{< relref "configuration.md#minimal-configuration" >}}).

1. To deploy standalond Chef Habitat Builder, specify the `builder` product on the command line.

    ```shell
    ./chef-automate deploy --product builder config.toml
    ```

    If you changed no configuration values in the previous step, run

    ```shell
    ./chef-automate deploy --product builder --product automate
    ```

    Accept the license with `y`.

Chef Habitat Builder uses the same mechanisms that Chef Automate does for [user authentication]({{< relref "users.md" >}}), [configuration]({{< relref "configuration.md" >}}), [backups]({{< relref "backup.md" >}}), [log management]({{< relref "log_management.md" >}}), and [uninstalling]({{< relref "troubleshooting.md#uninstalling-chef-automate" >}}). Please note that the Chef Automate UI will only support managing Users, Groups, and Authorization policies when deployed without the full Chef Automate stack.

#### Add Chef Automate to a Standalone Chef Habitat Builder Installation
If you wish to enable all of Chef Automate at a later time you can patch the product configuration to include the complete Chef Automate stack. For example:

1. Create a new patch configuration toml as `config.toml` and update the products
  to include both `builder` and `automate`:

    ```toml
    [deployment.v1.svc]
      products = ["builder", "automate"]
    ```

1. Patch the configuration to deploy the rest of the Chef Automate services:

    ```shell
    ./chef-automate config patch config.toml
    ```

    You should see output similar to:

    ```output
    Updating deployment configuration

    Applying deployment configuration
    ...
    Success: Configuration patched
    ```
    Access the Chef Automate web UI through the same FQDN as your original Chef Habitat Builder installation, e.g. https://{{< example_fqdn "builder" >}}.

Chef Habitat Builder and Chef Automate use the same mechanisms for [user authentication]({{< relref "users.md" >}}), [configuration]({{< relref "configuration.md" >}}), [backups]({{< relref "backup.md" >}}), [log management]({{< relref "log_management.md" >}}), and [uninstalling]({{< relref "troubleshooting.md#uninstalling-chef-automate" >}}). In particular, Chef Automate backups contain Chef Habitat Builder data as well as packages stored in Chef Habitat Builder.

#### Add Chef Habitat Builder to a Chef Automate Installation

Patch an existing Chef Automate installation to add Chef Habitat Builder:

1. Create a `patch.toml` file to add `builder` to the list of products to deploy:

    ```toml
       [deployment.v1.svc]
       products=["automate", "builder"]
    ```

1. Apply the patch to the Chef Automate installation:

    ```shell
       sudo chef-automate config ./patch.toml
    ```

    The command output shows the Chef Habitat Builder services being added:

    ```shell
       Updating deployment configuration

       Applying deployment configuration
         Installed automate-minio
         Installed automate-builder-memcached
         Installed automate-builder-api
         Installed automate-builder-api-proxy
         Started automate-minio
         Started automate-builder-memcached
         Started automate-builder-api
         Started automate-builder-api-proxy
         Started automate-load-balancer
       Success: Configuration patched
    ```

    Access the Chef Habitat Builder web UI through the same FQDN as your original Chef Automate installation, e.g.  https://{{< example_fqdn "automate" >}}.

Chef Habitat Builder and Chef Automate use the same mechanisms for [user authentication]({{< relref "users.md" >}}), [configuration]({{< relref "configuration.md" >}}), [backups]({{< relref "backup.md" >}}), [log management]({{< relref "log_management.md" >}}), and [uninstalling]({{< relref "troubleshooting.md#uninstalling-chef-automate" >}}). In particular, Chef Automate backups contain Chef Habitat Builder data as well as packages stored in Chef Habitat Builder.

### Sign in to Chef Habitat Builder

To sign in to Chef Habitat Builder, you must first sign in through the Chef Automate web UI, which is installed along with Chef Habitat Builder.

1. View your login credentials in the terminal with:

   ```bash
   cat automate-credentials.toml
   ```

    You should see output similar to:

   ```output
   url = "https://{{< example_fqdn "automate" >}}"
   username = "admin"
   password = "abcdefgh1234567890PASSWORDSTRING"
   ```

1. Navigate to `https://{{< example_fqdn "automate" >}}` in your browser. If you cannot access the site in Google Chrome, try Mozilla Firefox or Microsoft Edge.
1. Sign in to Chef Automate using the [credentials provided during deployment]({{< relref
   "install.md#open-chef-automate" >}}).
1. Select **Applications** in the top navigation bar
1. Select **Chef Habitat Builder** in the left sidebar, which redirects the browser to the Chef Habitat Builder login site
1. Select **Sign in with Chef Automate**
1. Sign into Chef Habitat Builder using the same credentials used with Chef Automate

### Generate a Chef Habitat Builder Personal Access Token

In order to bootstrap packages and to perform authenticated operations with the `hab` client, you must generate a Personal Access Token for Chef Habitat Builder:

1. Select your Gravatar icon on the top right corner of the Chef Habitat Builder web page.
1. Select Profile. This will take you to a page where you can generate your access token.
1. Make sure to save your access token securely.

### Create the Core Origin

Once you are signed in to the Chef Habitat Builder UI, select the **New Origin** button and enter `core` as the name for the origin.

## Access Chef Habitat Builder With Chef Habitat Command-Line Tools

Use the `https://{{< example_fqdn "automate" >}}/bldr/v1` URL when accessing your Chef Habitat Builder installation with the Chef Habitat command-line tools.
The Chef Habitat command-line tools recognize the [`HAB_BLDR_URL` environment variable](https://www.habitat.sh/docs/reference/), which you can set on the command line with:

```bash
export HAB_BLDR_URL=https://{{< example_fqdn "automate" >}}/bldr/v1/
```

Because you are using an on-prem installation of Chef Habitat Builder, you must specify the Builder API endpoint of your installation when following the [Habitat Builder documentation](https://www.habitat.sh/docs/using-builder/). This documentation covers [using origin keys](https://www.habitat.sh/docs/using-builder/#using-origin-secrets), [using origin secrets](https://www.habitat.sh/docs/using-builder/#using-origin-secrets), and [uploading and promoting packages](https://www.habitat.sh/docs/using-builder/#upload-and-promote-packages).

## Bootstrap Chef Habitat Builder With Core Habitat Packages
See [Bootstrap Chef Habitat Builder On-Prem]({{< relref "bootstrap_on_prem_builder.md" >}}) for directions on populating your Chef Habitat Builder installation with the necessary core packages from the public Chef Habitat Builder

## Access the Chef Habitat Builder REST API

To access the [REST API](https://www.habitat.sh/docs/api/builder-api/) for your installation of Chef Habitat Builder, you must specify your Builder authentication token as a bearer token in your request's
`Authorization` header.
For example:

```bash
curl -H "Authorization: Bearer <your-habitat-builder-auth-token>" https://{{< example_fqdn "automate" >}}/bldr/v1/...
```

## Logging errors

To change the log level for Chef Habitat Builder only, create a TOML file that contains the partial configuration below. Uncomment and change settings as needed, and then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

```toml
[builder_api.v1.sys.log]
level = "debug"
scoped_levels = ["tokio_core=error", "tokio_reactor=error", "zmq=error", "hyper=error" ]
```

## Setting up Automate as an OAuth Provider for Habitat Builder (Deprecated)

{{< warning >}}
These instructions have been deprecated in favor of using the Chef Automate installer to deploy Chef Habitat.
{{< /warning >}}

If you did not use the Chef Automate installer to deploy Chef Habitat Builder, you must
provide user authentication using OAuth instead of the Chef Automate authentication
stack. Configuring Chef Automate as an OAuth Provider for Chef Habitat Builder is a common
way to do so.

To configure Chef Automate as an OAuth Provider for Chef Habitat Builder, create a TOML file with the partial configuration below.
Run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

`bldr_client_id` and `bldr_client_secret` simply need to match what you configured for the corresponding
values in Chef Habitat Builder (see below). However, we strongly recommend those values follow
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

In addition, add Chef Automate's TLS certificate to Builder's list of accepted certificates.
Locate Chef Automate's default self-signed certificate by running `cat /hab/svc/automate-load-balancer/data/{{< example_fqdn "automate" >}}.cert`, copy this default certificate, and then add it to your Builder instance's list of accepted certificates.

```text
-----BEGIN CERTIFICATE-----
MIIDfDCCAmSgAcaSldKaf...
-----END CERTIFICATE-----
```

If you are using a certificate signed by a trusted certificate authority instead of the default certificate,
you can provide Builder with the root certificate authority for the signed certificate.

For more information, check out this further explanation on how to [configure Builder to authenticate via Chef Automate](https://github.com/habitat-sh/on-prem-builder).
