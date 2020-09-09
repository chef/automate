+++
title = "Bootstrap Chef Habitat Builder On-prem"

date = 2019-11-19T14:10:15-08:00

draft = false

[menu]
  [menu.automate]
    title = "Bootstrap Chef Habitat Builder On-prem"
    parent = "automate/getting_started"
    identifier = "automate/getting_started/bootstrap_on_prem_builder.md Bootstrap Chef Habitat Builder On-prem"
    weight = 55
+++

[\[edit on GitHub\]](https://github.com/chef/automate/blob/master/components/docs-chef-io/content/automate/bootstrap_on_prem_builder.md)

This guide covers bootstrapping Chef Habitat Builder on-prem with curated core seed lists from the Chef Habitat public Builder.

Bootstrapping Chef Habitat Builder requires:

* An outward bound HTTPS connection
* GitHub account
* [Public Chef Habitat Builder](https://bldr.habitat.sh) account
* Public Chef Habitat Builder personal access token

Use [seed lists](https://github.com/habitat-sh/on-prem-builder/blob/master/package_seed_lists/README.md) to populate your on-premises Chef Habitat Builder installation with the packages required by your builds.
[Sample seed lists](https://github.com/habitat-sh/on-prem-builder/tree/master/package_seed_lists) exist for the following scenarios:

* Full `core`: the full contents of the upstream `core` origin. The x86_64 Linux set expands to 12GB, the Linux kernel2 set to 1GB, and the Windows set to 3.5GB.
* Core dependencies: a subset of `core` consisting of commonly-used buildtime dependencies.
* Effortless: packages used to start with the [Effortless pattern](https://github.com/chef/effortless). A complete Effortless implementation requires the contents of both the `stable` and the `unstable` channel.

## Clone the Chef Habitat Builder On-prem Repository

To access the curated seed lists for bootstrapping Chef Habitat Builder on-prem, you will need to clone the Chef Habitat Builder on-prem repository using https.

```shell
git clone https://github.com/habitat-sh/on-prem-builder.git
```

Once the repository is successfully cloned, move into the `on-prem-builder` repository:

```shell
cd on-prem-builder
```

The Chef Automate installer uses a self-signed certificate. Copy the SSL public key certificate chain from Chef Automate into `/hab/cache/ssl` with this command:

```shell
cp /hab/svc/automate-load-balancer/data/{{< example_fqdn "automate" >}}.cert /hab/cache/ssl/{{< example_fqdn "automate" >}}.cert
```

## Download Seed List Packages from the Public Chef Habitat Builder

Your host must have access to the internet to download the curated seed list packages from the **public** [Chef Habitat Builder](https://bldr.habitat.sh).
If you have not already done so, create a user account and personal access token on the **public** [Chef Habitat Builder](https://bldr.habitat.sh/).

Use the `hab pkg download` command with a seed list `</path/to/seed_list>` to download packages for
your desired architecture `<arch>` from a channel `<channel>` to a directory `<artifact-dir>`:

```shell
HAB_AUTH_TOKEN=<your_public_builder_personal_access_token> hab pkg download --target <arch> --channel <channel> --file </path/to/seed_list> --download-directory <artifact-dir>
```

For example, to use the Effortless seed list to download `x86_64-linux` packages from the
`stable` channel to the `builder_bootstrap` directory:

```shell
HAB_AUTH_TOKEN=<your_public_builder_personal_access_token> hab pkg download --target x86_64-linux --channel stable --file package_seed_lists/effortless_x86_64-linux_stable --download-directory builder_bootstrap
```

### Bulk-Upload Seed List Packages to Chef Habitat Builder on-prem

Run the `bulkupload` command to upload artifacts from `<artifact-dir>` to the `<channel>` channel in the on-premises Chef Habitat Builder using the [Builder API endpoint]({{< ref "on_prem_builder.md#access-chef-habitat-builder-on-prem-with-chef-habitat-command-line-tools" >}}):

```shell
HAB_AUTH_TOKEN=<your_on-prem_Builder_personal_access_token> hab pkg bulkupload --url https://{{< example_fqdn "automate" >}}/bldr/v1 --channel <channel> <artifact-dir> --auto-create-origins
```

For example,

```shell
HAB_AUTH_TOKEN=<your_on-prem_Builder_personal_access_token> hab pkg bulkupload --url https://{{< example_fqdn "automate" >}}/bldr/v1 --channel stable builder_bootstrap/ --auto-create-origins
```

Should produce the output:

```output
Preparing to upload artifacts to the 'stable' channel on https://{{< example_fqdn "automate" >}}/bldr/v1
Using builder_bootstrap/artifacts for artifacts and builder_bootstrap/keys signing keys
Found 46 artifact(s) for upload.
Discovering origin names from local artifact cache
Missing origin 'chef'
Origin 'core' already exists
Missing origin 'effortless'
Creating origin chef.
Created origin chef.
Creating origin effortless.
Created origin effortless.
75 B / 75 B | [===========================================] 100.00 % 1.31 MB/s d
Uploading public origin key chef-20160614114050.pub
...
```

The `--auto-create-origins` flag creates each origin listed in the
`<artifact-dir>/artifacts` directory. If you omit the `--auto-create-origins` flag,
use the Chef Habitat Builder UI to create the necessary origins before running the
`bulkupload` command.

To finish up, return to your Chef Habitat Builder on-prem installation and view the packages that you've added to your `core` origin at `https://{{< example_fqdn "automate" >}}/bldr/#/origins/core/packages`.

## Using Chef Habitat Builder

Because you are using an on-prem installation of Chef Habitat Builder, you must specify the [Builder API endpoint of your installation]({{< ref "on_prem_builder.md#access-chef-habitat-builder-with-chef-habitat-command-line-tools" >}}) when following the [Habitat Builder documentation](https://www.habitat.sh/docs/using-builder/).
This documentation covers [using origin keys](https://www.habitat.sh/docs/using-builder/#using-origin-secrets), [using origin secrets](https://www.habitat.sh/docs/using-builder/#using-origin-secrets), and [uploading and promoting packages](https://www.habitat.sh/docs/using-builder/#upload-and-promote-packages).
