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

The Chef Infra Server stores cookbooks, the policies that are applied to nodes, and the metadata that describes each registered node that is under management by Chef Infra Client.

## Adding configuration

You can configure the chef automate infra server to use S3 storage type for cookbooks. Add below configurations in a toml file. 

```
[erchef.v1.sys.api]
s3_enabled=true  ## false in case you want to disbale this config
s3_bucket_name="<name of your bucket>"
s3_external_url="<s3_url>"


[bookshelf.v1.sys.bookshelf]
aws_access_id="<aws-access-id>"
aws_secret_key="<aws-secret-token>"
```

Patch the above configuration using:
```
chef-automate config patch </path/to/your-file.toml>.toml

```