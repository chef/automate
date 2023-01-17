+++
title = "Automate CLI (HA)"

date = 2018-03-26T15:29:24-07:00
draft = false

gh_repo = "automate"
toc_layout = "cli_chef_automate_toc"

[menu]
  [menu.automate]
    title = "Automate CLI (HA)"
    parent = "automate/reference"
    identifier = "automate/reference/cli_chef_automate_ha.md Automate CLI (HA)"
    weight = 25
+++

## Chef Automate CLI Commands

{{< automate/automate_cli_commands_ha data_path="automate/cli_chef_automate/commands" >}}

## Error Codes

If `chef-automate` encounters an error during execution, it exits with a non-zero
error code. Here's what our error codes mean:

{{< automate/automate_cli_status_errors data_path="automate/cli_chef_automate/status/errors" >}}
