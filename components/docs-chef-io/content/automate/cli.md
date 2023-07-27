+++
title = "Chef Automate CLI"
aliases = ['cli_chef_automate', 'cli_chef_automate_ha']

draft = false

gh_repo = "automate"
toc_layout = "cli_chef_automate_toc"

[menu]
  [menu.automate]
    title = "Automate CLI"
    parent = "automate/reference"
    identifier = "automate/reference/cli_chef_automate.md Automate CLI"
    weight = 24
+++

## `chef-automate` CLI Commands

{{< automate/automate_cli_commands data_path="automate/cli_chef_automate/commands" >}}

## Error Codes

If `chef-automate` encounters an error during execution, it exits with a non-zero
error code. Here's what our error codes mean:

{{< automate/automate_cli_status_errors data_path="automate/cli_chef_automate/status/errors" >}}
