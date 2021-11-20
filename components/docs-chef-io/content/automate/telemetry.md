+++
title = "Telemetry"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Telemetry"
    parent = "automate/configure"
    identifier = "automate/configure/telemetry.md Telemetry"
+++

### About Telemetry

Users of this Chef Automate server may elect to share user-anonymized usage data with
Chef Software, Inc.
Chef uses this shared data to improve Chef Automate.
Please see the [Chef Privacy Policy](https://chef.io/privacy-policy) for more information about the information Chef collects, and how that information is used.

### Opting Out of Telemetry

Admins can opt out of telemetry for the Automate server and all of its users, by editing an existing `configuration.toml` or create a new TOML file. Add the following to the TOML file:

```toml
[license_control.v1.sys.telemetry]
    opt_out= true
```

Save the file and apply the changes with:

  ``chef-automate config patch configuration.toml``

This will apply the configuration change for telemetry.

Individual users can opt out of telemetry by unchecking the telemetry box on the welcome pop-up the first time they log into Automate.

To opt out of telemetry at any later point in time, navigate to the profile icon, select "About Chef Automate" from the drop-down, and uncheck the telemetry checkbox.
