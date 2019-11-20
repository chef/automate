+++
title = "Deploying Builder with Chef Automate"
description = "use Chef Automate to deploy Builder"
date = 2019-11-19T14:10:15-08:00
weight = 20
draft = false
bref = ""
toc = true
+++

## Overview
Enterprise customers may wish to set up an on-premises Habitat Builder depot to store
Habitat packages for use by their own Habitat Studios and Supervisors. Use Chef Automate
to deploy and manage an on-premises Habitat Builder depot.

{{< warning >}}
The current implementation of Automate-deployed Habitat Builder is an early milestone
intended for use in proof-of-concept deployments. We do not consider it a fully-mature
feature, and you should not either.
{{< /warning >}}

### System requirements
Currently, Habitat Builder requires Chef Automate to be installed on the same host for
authentication purposes. Outside the boundaries of a proof-of-concept exercise, we
recommend against running installations of Chef Automate and Habitat Builder on
the same host.

#### Hardware
The minimum with which Chef Automate and Habitat Builder can be deployed on a single host
is:

* 16 GB of RAM.
* 130 GB of disk space, available to /hab.
* 4 vCPUs.
* Inbound network connectivity from LAN (HTTP/HTTPS) is required for internal clients to access the on-premises Chef Habitat Builder.

For deployments that are expected to see production-scale workload, we recommend:

* 64 GB of RAM.
* 200 GB of diskspace, available to /hab.
* 16 vCPUs.

Roughly 80 GB of the disk space is designated for Chef Automate; the rest is used for
Habitat Builder and the artifacts it stores. The current implementation uses Minio for
Habitat artifact storage; we do not support using Artifactory for artifact storage.

#### Operating system
Chef Automate and Habitat Builder require

* a Linux kernel of version 3.2 or greater
* `systemd` as the init system
* `useradd`
* `curl` or `wget`
* The shell that starts Automate should have a max open files setting of at least 65535

### Deploy on-premises Habitat Builder
Deploying Habitat Builder with Chef Automate requires a Chef Automate license. If you
already have a Chef Automate license, you may use it for the deployment. Otherwise, you
can download a trial license when you first sign in to Chef Automate.

If you are deploying Habitat Builder with Chef Automate in an airgapped environment,
follow [the documentation on building an airgap bundle]({{<relref "airgapped-installation.md">}}).

To deploy Chef Automate and Habitat Builder, specify both the `builder` and `automate`
products on the command line. For example,

```shell
sudo chef-automate deploy --product builder --product automate
```

### Sign in to Habitat Builder
1. Sign in to Chef Automate using the [credentials provided during deployment]({{<relref
   "install.md#open-chef-automate">}}).
1. Go to `https://<your_automate_fqdn>/bldr`.
1. Select the Sign In button in the upper right corner.
1. Select Sign In With Chef Automate.
1. Use the same credentials as in Step 1 to sign in to Habitat Builder.

The Automate-deployed Habitat Builder currently supports authentication with local users only.

### Create the core origin
Once you are signed in to the Habitat Builder UI, select the New Origin button and enter in `core` as the origin name.

### Generate a personal access token
You need a Personal Access Token to [bootstrap Habitat Builder with packages]({{<relref "on-prem-builder.md#bootstrap-habitat-builder-with-packages">}}) and to perform authenticated operations with the `hab` client.

Select your Gravatar icon on the top right corner of the Habitat Builder web page, and then select Profile. This will take you to a page where you can generate your access token. Make sure to save it securely.

### Bootstrap Habitat Builder with packages
Use [seed lists](https://github.com/habitat-sh/on-prem-builder/blob/master/package_seed_lists/README.md) to populate your on-premises Habitat Builder installation with the packages required by your builds.
[Sample seed lists](https://github.com/habitat-sh/on-prem-builder/tree/master/package_seed_lists) exist for the following scenarios:

* Full `core`: the full contents of the upstream `core` origin. The x86_64 Linux set
    expands to 12GB, the Linux kernel2 set to 1GB, and the Windows set to 3.5GB.
* Core deps: a subset of `core` consisting of commonly-used buildtime dependencies.
* Effortless: packages used to start with the [Effortless pattern](https://github.com/chef/effortless). A complete Effortless implementation requires the contents of both the `stable` and the `unstable` channel.

#### Download packages from public origins
You must be on an Internet-connected host to download packages from the *public* [Builder](https://bldr.habitat.sh/#/pkgs/core)
service.

Use the `hab pkg download` command with a seed list `</path/to/seed_list>` to download packages for
your desired architecture `<arch>` from a channel `<channel>` to a directory `<artifact-dir>`:

```shell
HAB_AUTH_TOKEN=<your_public_Builder_instance_token> hab pkg download --target <arch> --channel <channel> --file </path/to/seed_list> --artifact-directory <artifact-dir>
```

For example, to use the Effortless seed list to download `x86_64-linux` packages from the
`stable` channel to the `builder_bootstrap` directory:

```shell
HAB_AUTH_TOKEN=<your_public_Builder_instance_token> hab pkg download --target x86_64-linux --channel stable --file package_seed_lists/effortless_x86_64-linux_stable --artifact-directory builder_bootstrap
```

#### Bulk-upload packages to on-premises Habitat Builder
Make sure the directory `<artifact-dir>` and its contents are on a host that can
access your on-premises Habitat Builder at `https://<your_automate_fqdn>/bldr`.

1. If your on-premises Habitat Builder is using a self-signed certificate, copy the SSL
   public key certificate chain into `/hab/cache/ssl` on the host from which you will
   perform the bulk upload to Habitat Builder.
1. Inspect the contents of your `<artifact-dir>/artifacts` directory. For each origin
   listed in that directory, make sure that origin exists in your on-premises Habitat
   Builder. If that origin does not exist in your on-premises Habitat Builder, create it
   in the Habitat Builder UI.
1. Run the `bulkupload` command to upload artifacts from `<artifact-dir>` to the
   `<channel>` channel in on-premises Habitat Builder:

   ```shell
   HAB_AUTH_TOKEN=<your_on-prem_Builder_instance_token> hab pkg bulkupload --url https://<your_automate_fqdn>/bldr/v1 --channel <channel> <artifact-dir>
   ```

### Backup Habitat Builder
Builder backups are performed as part of the [Chef Automate backup process]({{<relref "backup.md">}}).

### Not currently supported
* high-availability/DR/multinode Builder

