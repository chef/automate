+++
title = "Invalid Login Attempts"

weight = 70
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Invalid Login Attempts"
    parent = "automate/configure"
    identifier = "automate/configure/invalid_login_attempts.md Invalid Login Attempts"
+++

Chef Automate has an option to configure Invalid Login Attempts which is enabled by default to avoid multiple failed login attempts in short period of time. Chef Automate blocks user for specified duration in minutes, after allowed number of invalid login attempts is met.

Default configuration of invalid_login_attempts in Chef Automate:

```toml
[dex.v1.sys]
  [dex.v1.sys.invalid_login_attempts]
      enable_invalid_login_attempts = true
      blocked_duration = 30
      max_invalid_login_attempts_allowed = 5
```

Above default values can be changed:

- Set the value of `enable_invalid_login_attempts` property to **false** in *config.toml* file to disable this feature.
- Set the value of `blocked_duration` property to your desired time in minutes(for example **30**). (Setting the value of this property to **30** will block the user from trying to log in with invalid credentials for 30 minutes after `max_invalid_login_attempts_allowed` is consumed).
- Set the value of `max_invalid_login_attempts_allowed` to your desired number (for example **5**). (Setting the value of ths property to **5** will let user try for 5 times with failed login attempts beyond which user will be blocked for set `blocked_duration`. Only when user has made all 5 failed attempts within specified `blocked_duration`)

{{< note >}}
The failed login attempts will reset and same blocked user or user with some failed login attempts can start fresh again after specified `blocked_duration` 
{{< /note >}}