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

This guide will show you how to register a supermarket with OC-ID embedded in Automate HA in a [on-premise setup](/automate/ha_onprim_deployment_procedure/). Before starting this page, refer to the [Supermarket Integration](/automate/supermarket_integration_with_automate/) page with Automate for basic understanding.

## Register Supermarket with Automate HA Embedded OC-ID

The steps to register a supermarket with Automate HA embedded OC-ID is as follows:

1. SSH into the Bastion node of Automate HA On-Premises setup.

    ```bash
    sudo su
    ```

1. Create a file named `ocid-apps.toml`, and mention the **name** and the **redirect_uri** for the application, e.g., *Supermarket*. The content of the created file should be in the following format:

    ```cd
    [ocid.v1.sys.ocid.oauth_application_config]
        [[ocid.v1.sys.ocid.oauth_application_config.oauth_applications]]
            name = ""
            redirect_uri = ""
    ```

    Update the URL of the supermarket website in the `redirect_uri` as per the actual supermarket URL. Refer to the code below:

    ```toml
    [ocid.v1.sys.ocid.oauth_application_config]
        [[ocid.v1.sys.ocid.oauth_application_config.oauth_applications]]
            name = "supermarket"
            redirect_uri = "https://example-supermarket.com/auth/chef_oauth2/callback"
    ```

1. run the command: `config patch` with the `.toml` file you created in the previous step on all the automate and chef-server nodes from the bastion node. The commands to patch are as follows:

    ```bash
    # Run patch on Automate nodes
    chef-automate config patch ocid-apps.toml -a
    ```

    ```bash
    # Run patch on Chef Server nodes
    chef-automate config patch ocid-apps.toml -c
    ```

    Once the patch is completed, the new app should be registered with OC-ID as part of Automate embedded chef-server.

1. Get the details of the registered applications from the bastion node, and run the following command:

    ```bash
    chef-automate config oc-id-show-app -a
    ```

    The above command will fetch the details of all the registered OC-ID applications from all the Automate nodes. To know about the different flags to be used with this command, run the following command(help command):

    ```bash
    chef-automate config oc-id-show-app -h.
    ```

    The output of the above command looks like as shown below:

    {{< figure src="/images/automate/ha_output_ocid_app.png" alt="OC-ID Application Output">}}

## Configuration of Supermarket

1. Now, SSH into the supermarket instance and run the following commands in sequence:

    ```bash
    sudo su
    cd /etc/supermarket
    ```

1. Once you are in the path `/etc/supermarket`, edit the file `supermarket.rb` to update the details of the **UID**, **secret**, **chef-server-endpoint**, and **ssl-verify-mode** as per the details of the registered OC-ID apps found in the steps above where you have registered the supermarket application. Refer to the snippet below:

    ```bash
    default['supermarket']['chef_oauth2_app_id'] = "<UID>"
    default['supermarket']['chef_oauth2_secret'] = "<Secret>"
    default['supermarket']['chef_oauth2_url'] = "<Automate HA load balancer FQDN>"
    default['supermarket']['chef_oauth2_verify_ssl'] = false
    ```

    The datatype of the attribute: `chef_oauth2_verify_ssl` is a boolean. The value will depend on whether your automate runs with a valid SSL certificate. If Automate runs with a valid SSL certificate, it should be set as `true`; otherwise, specify it as `false`.

1. Now run the `reconfigure` command to reflect the above changes in the running supermarket application configuration:

    ```bash
    supermarket-ctl reconfigure
    ```

1. Once the reconfiguration is completed, visit the supermarket website on the browser. Refer to the image below:

    {{< figure src="/images/automate/ha_supemarket_landing_page.png" alt="Supermarket Landing Page">}}

1. Select Sign In. You should see the OC-ID login page from the Automate HA website if the configuration is done properly. Refer to the image below:

    {{< figure src="/images/automate/ha_supermaket_sign-in.png" alt="Supermarket Sign In Page">}}

1. Put the correct credentials of `chef-server` running as part of Automate HA and select Sign In. A screen will appear to authorize the supermarket application. Select **Yes**.

    {{< figure src="/images/automate/ha_supermaket_authorization.png" alt="Supermarket Credentials">}}

1. Once the supermarket application is successfully authorized, the screen looks like as shown in the image below:

    {{< figure src="/images/automate/ha_supermarket_app_board.png" alt="Supermarket Board">}}

You have now successfully logged in to the supermarket using the credentials of `chef-server` through the **OC-ID** service running as part of Automate HA.
