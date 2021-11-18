+++
title = "Session Timeout"

weight = 70
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Session Timeout"
    parent = "automate/configure"
    identifier = "automate/configure/session_timeout.md Session Timeout"
+++

Chef Automate has an optional session timeout configuration for signing out idle workstations. Chef Automate measure session activity as any interaction from a workstation, from a mouse, keyboard, or touchpad. With session timeout enabled, Chef Automate signs out idle workstations after a set number of minutes.

To enable session_timeout in Chef Automate:

- Set the value of `enable_idle_timeout` property to **true** in *config.toml* file.
- Set the value of `idle_timeout_minutes` property to your desired time in minutes(for example **30**). (Setting the value of this property to **30** will set the session timeout time to 30 minutes).

{{< note >}}
The minimum value of `idle_timeout_minutes` can be 5 minutes.
{{< /note >}}

This example sets the session timeout to 30 minutes, which means that Chef Automate signs out any connected workstation after 30 minutes of inactivity:

```toml
[global.v1]
  [global.v1.session_settings]
    enable_idle_timeout = true
    idle_timeout_minutes = 30
```
