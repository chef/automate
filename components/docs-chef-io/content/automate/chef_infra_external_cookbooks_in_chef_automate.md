+++
title = "Chef Infra External Cookbooks In Chef Automate"
draft = false
gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Chef Infra External Cookbooks In Chef Automate"
    parent = "automate/configure"
    identifier = "automate/configure/chef_infra_external_cookbooks_in_chef_automate.md Chef Infra External Cookbooks In Chef Automate"
    weight = 90
+++

The Chef Infra Server stores cookbooks, the policies that are applied to nodes, and the metadata that describes each registered node under Chef Infra Client's management. This page will tell you how you can configure your Infra Server to use S3 storage type of cookbooks.

{{< note >}}

- If you are swtiching for existing cookbooks, there will be some data loss. You will not be able to download or upload the existing cookbooks after adding config.
- This is strictly recommended to add the config at the time of setting up infra server.

{{< /note >}}

## Adding configuration

You can configure the Chef Automate Infra Server to use the S3 storage type for cookbooks by adding the following configurations into a `.toml` file.

```bash
[erchef.v1.sys.api]
s3_enabled=true  ## false in case you want to disable this config
s3_bucket_name="<name of your bucket>"
s3_external_url="<s3_url>"
[bookshelf.v1.sys.bookshelf]
aws_access_id="<aws-access-id>"
aws_secret_key="<aws-secret-token>"
```

Patch the above configuration using the following:

```bash
chef-automate config patch </path/to/your-file.toml>.toml
```

Refer to the [External Cookbooks](https://docs.chef.io/server/#external-cookbooks) section to learn about the cookbooks shored in Chef Infra Server.
