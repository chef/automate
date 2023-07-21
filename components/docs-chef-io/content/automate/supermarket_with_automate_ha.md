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

This page will discuss the integration of Supermarket with Automate HA setup. The page will guide you to register an existing private Supermarket with [on-premises deployment](/automate/ha_onprim_deployment_procedure/) of Automate HA.

Before starting this page, refer to the [Supermarket Integration](/automate/supermarket_integration_with_automate/) page with Automate for basic understanding.

## Register Supermarket with Automate HA

1. The overall steps to register an existing private instance of Supermarket with Automate HA is same as the steps to register it with [Standalone](/automate/supermarket_integration_with_automate/#register-supermarket-with-automate-embedded-chef-identity) Automate. The only difference is in the way we patch the `.toml` file in Automate HA.

1. Now, patch the configuration on all the frontend nodes from the Bastion node using the command below:

    ```bash
    // Frontend Nodes
    chef-automate config patch ocid-apps.toml -f
    ```

    Once the patch is completed, the new app should be registered with OC-ID as part of Automate embedded chef-server.

1. You can get the details of the registered applications from the bastion node by running the following command:

    ```bash
    chef-automate config oc-id-show-app
    ```

    The output of the above command looks like as shown below:

    {{< figure src="/images/automate/ha_output_ocid_app.png" alt="OC-ID Application Output">}}

1. The configuration of supermarket in Chef Automate HA can be done in the same way as Standalone Automate. Refer to the [Supermarket Integration](/automate/supermarket_integration_with_automate/#supermarket-configuration) page to view the configuration steps.

Refer to the [Configuration](/automate/configuration/#encrypt-cookies-with-custom-secret-key-in-oc-id-service) page, to check the optional settings for integration of private Supermarket in Chef Automate.
