+++
title = "Invalid Login Attempts"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Invalid Login Attempts"
    parent = "automate/configure"
    identifier = "automate/configure/invalid_login_attempts.md Invalid Login Attempts"
    weight = 60
+++

Invalid Login Attempts is available only in case of local or LDAP users.

Chef Automate shows error message for invalid login attempts for local or LDAP user as shown below.

{{< figure src="/images/automate/invalid_login_attempts_error_msg.png" width="500" alt="Chef Automate Invalid Login Attempts">}}

Chef Automate shows error message for blocked local or LDAP user as shown below.

{{< figure src="/images/automate/blockd_user_login_error_msg.png" width="500" alt="Chef Automate Blocked User">}}

Chef Automate lets you configure **Invalid Login Attempts**, which is enabled (by default) to avoid multiple failed login attempts in a shorter time. Chef Automate also blocks the user for a specified duration (in minutes) once the maximum allowed number of invalid login attempts reached

The default configuration of `invalid_login_attempts` in Chef Automate is as follows:

```toml
[dex.v1.sys]
  [dex.v1.sys.invalid_login_attempts]
      enable_invalid_login_attempts = true
      blocked_duration_in_minutes = 30
      max_invalid_login_attempts_allowed = 5
```

To change the above default values, follow the steps given below:

- Set the value of `enable_invalid_login_attempts` property to **false** in *config.toml* file to disable the feature.
- Set the value of `blocked_duration_in_minutes` property to your desired time in minutes(for example, **30** mins). (Setting the value of this property to **30** will block the user from trying to log in with invalid credentials for 30 minutes after `max_invalid_login_attempts_allowed` is consumed).
- Set the value of `max_invalid_login_attempts_allowed` to your desired number (for example, **5**). (Setting the value of this property to **5** will let the user try five times with failed login attempts beyond which the user will be blocked for set `blocked_duration_in_minutes`. Only when the user has made all five failed attempts within specified `blocked_duration_in_minutes`)

{{< note >}} The failed login attempts will reset and same blocked user or user with the some failed login attempts can start fresh again after the specified `blocked_duration_in_minutes`. {{< /note >}}
