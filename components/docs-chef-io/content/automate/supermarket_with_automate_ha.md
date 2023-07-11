+++
title = "Supermarket with Automate HA"
draft = false
gh_repo = "automate"
[menu]

  [menu.automate]
    title = "Supermarket with Automate HA"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/supermarket_with_automate_ha.md Supermarket with Automate HA"
    weight = 75
+++

This page will discuss the integration of Supermarket with Automate HA setup. The page will guide you to register an existing private Supermarket with [on-premises deployment](/automate/ha_onprim_deployment_procedure/) of Automate HA. Refer to the configuration and integrate an existing private Supermarket with an Airgapped installation of Chef Automate.

Before starting this page, refer to the [Supermarket Integration](/automate/supermarket_integration_with_automate/) page with Automate for basic understanding.

## Register Supermarket with Automate HA

The steps to register a supermarket with Automate HA is the same as that of [Standalone](/automate/supermarket_integration_with_automate/#register-supermarket-with-automate-embedded-chef-identity). The patching of the `.toml` file is different for Automate HA.

Trigger the `config patch` command with the `.toml` file you created in the previous step on all the automate and chef server nodes from the bastion node as follows:

```bash
chef-automate config patch ocid-apps.toml -f
```

Once the patch is completed, the new app should be registered with OC-ID as part of Automate embedded chef-server.

You can get the details of the registered applications from the bastion node by running the following command:

```bash
chef-automate config oc-id-show-app
```

The output of the above command looks like as shown below:

{{< figure src="/images/automate/ha_output_ocid_app.png" alt="OC-ID Application Output">}}

Refer to the [Supermarket Integration](/automate/supermarket_integration_with_automate/#supermarket-configuration) page to view the Supermarket configuration steps.
