+++
title = "Session Timeout"

weight = 70
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Session Timeout"
    parent = "automate/configuring_automate"
    identifier = "automate/configuring_automate/session_timeout.md Session Timeout"
    weight = 70
+++

Set your Chef Automate session timeout to enable the idle timeout of your session. Session timeout is set by checking your activites like, mouse movement, click, keyboard key press, scrolling touchpad, etc. If there is no such activity upto the specified time (using session timeout), you get logged out from the Chef Automate Inferface.

To add the session timeout to Chef Automate:

- Set the value of `enable_idle_timeout` property to **true** in *config.toml* file.
- Set the value of `idle_time_minutes` property to your desired time in minutes(for example **30**). (Setting the value of this property to **30** will set the session timeout time to 30 minutes).

Your configuration in the *.toml* file looks like:

```toml
[global.v1]
  [global.v1.session_settings]
    enable_idle_timeout = true
    idle_timeout_minutes = 30
```

In the above snippet the session timeout has been set to 30 minutes. So, if you are inactive on the Chef Automate interface for 30 minutes, you will be logged out automatically.
