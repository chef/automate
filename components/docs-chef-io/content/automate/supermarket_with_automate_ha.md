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

This guide will show you how to register supermarket with OC-ID embedded in Automate HA in a [on-premise setup](/automate/ha_onprim_deployment_procedure/). Before we start with this page, you can refer to the [Supermarket Integration](/automate/supermarket_integration_with_automate/) page with Automate for basic understanding.

## Register Supermarket with Automate HA Embedded OC-ID

The steps to register supermarket with Automate HA embedded OC-ID is as follows:

1. SSH into the Bastion node of Automate HA On-Premises setup.

    ```bash
    sudo su
    ```

1. Create a file named `ocid-apps.toml`, mention the **name** and the **redirect_uri** for the application e.g. *Supermarket*. The content of the created file should be in the following format:

    ```cd
    [ocid.v1.sys.ocid.oauth_application_config]
        [[ocid.v1.sys.ocid.oauth_application_config.oauth_applications]]
            name = ""
            redirect_uri = ""
    ```

    Update the URL of the supermarket website in the `redirect_uri` as per the actual supermarket URL. efe to the code below:

    ```cd
    [ocid.v1.sys.ocid.oauth_application_config]
        [[ocid.v1.sys.ocid.oauth_application_config.oauth_applications]]
            name = "supermarket"
            redirect_uri = "https://example-supermarket.com/auth/chef_oauth2/callback"
    ```

1. Now, run the command: `config patch` with the `.toml` file you created in previous step on all the automate and chef-server nodes from the bastion node. The commands to patch are as follows:

    ```bash
    # Run patch on Automate nodes
    chef-automate config patch ocid-apps.toml -a
    ```

    ```bash
    # Run patch on Chef Server nodes
    chef-automate config patch ocid-apps.toml -c
    ```

    Once the patch is successfully completed the new app should be registered with OC-ID running as part of Automate embedded chef-server.

1. Get the details of the registered applications from the bastion node, run the following command:

    ```bash
    chef-automate config oc-id-show-app -a
    ```

    The above command will fetch the details of all the registered OC-ID applications from all the Automate nodes. To know about the different flags to be used with this command, run the following command(help command):

    ```bash
    chef-automate config oc-id-show-app -h.
    ```

    The output of the above command looks like as shown below:

    IMAGE

## Configuration of Supermarket

1. Now, SSH into the supermarket instance and run the following command in sequence:

    ```bash
    sudo su
    cd /etc/supermaket
    ```

1. Once you are in the path `/etc/supermarket`, edit the file `supermarket.rb` to update the details of the **UID**, **secret**, **chef-server-endpoint** and **ssl-verify-mode** as per the details of the registered OC-ID apps found in the steps above where you have registered the supermarket application. Refer to the snippet below:

    ```bash
    default['supermarket']['chef_oauth2_app_id'] = "<UID>"
    default['supermarket']['chef_oauth2_secret'] = "<Secret>"
    default['supermarket']['chef_oauth2_url'] = "<Automate HA load balancer FQDN>"
    default['supermarket']['chef_oauth2_verify_ssl'] = false
    ```

    The datatype of the attribute: `chef_oauth2_verify_ssl` is a boolean. The value will depend on where your automate is running with a valid SSL certificate or not. If automate is running with a valid SSL certificate then this should be set as `true` otherwise set is as `false`.

1. Now run `reconfigure` command to reflect the above changes in the running supermarket application configuration:

    ```bash
    supermarket-ctl reconfigure
    ```

1. Once the reconfiguration is completed visit the supermarket website on the browser. Refer to the image below:

    IMAGE

1. Select Sign In. If the configuration is done properly you should see the OC-ID login page from the Automate HA website. Refer to the image below:

    IMAGE

1. Put the correct credentials of `chef-server` running as part of Automate HA and select Sign In. A screen will appear to authorize the supermarket application. Select **Yes**.

    IMAGE

1. Once the supermarket application is successfully authorized the screen looks like as shown in the image below:

    IMAGE

You have now successfully logged in to supermarket using the credentials of `chef-server` through the **OC-ID** service running as part of Automate HA.
