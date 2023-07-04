+++
title = "Supermarket Integration"
date = 2020-02-11T14:24:00-08:00
weight = 20
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Supermarket Integration"
    parent = "automate/install"
    identifier = "automate/install/supermarket_integration_with_automate.md Supermarket Integration"
    weight = 70
+++

Chef Supermarket is the site for cookbooks. It provides a searchable cookbook repository and a friendly web UI. This page will discuss the integration of Supermarket with Automate HA setup. The setup will be done using On-Premises, AWS Managed Services, OpenSearch, and PostgreSQL.

## Pre-requisites

1. To start with the supermarket integration, firstly, use the `chef-automate` binary to create an **Airgap Installation Bundle (`.aib`)** on an internet-connected host. Refer to the [System Requirement](/automate/system_requirements/) page for the hardware and software requirements. You can also refer to the [Airgapped Installation](/automate/airgapped_installation/) page for the validation of airgapped installation.

1. SSH into to your ec2 instance as the root user and download the Chef Automate command-line tool from the current release channel.

    ```cd
    curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
    ```

1. Download the airgapped installation bundle of the latest automate version to an internet-connected machine using the following command:

    ```bash
    curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o </path/to/airgap-install-bundle>
    ```

    Or download the airgapped bundle of a specific version of automate using the command below:

    ```bash
    curl https://packages.chef.io/airgap_bundle/current/automate/<version>.aib -o </path/to/airgap-install-bundle>

    ```

1. Deploy the Airgap Installation Bundle using the following command:

    ```bash
    chef-automate deploy --airgap-bundle automate-<version>.aib --product automate --product infra-server
    ```

1. Check the status of all the components using the following command:

    ```bash
    chef-automate status
    ```

1. Check the logs using the following command:

    ```bash
    journalctl -u chef-automate -f
    ```

1. Create a user using the following command:

    ```bash
    chef-server-ctl command
    ```

## Register Supermarket with Automate Embedded Chef Identity

When you install Chef Automate, it bundles the Chef-Server OC-ID component as an Oauth provider. Users can use the Oauth provider to log in to another application (e.g. Supermarket) using their Chef-Server credentials. Follow the steps below to register the applications to use OC-ID as a medium to log in to the respective applications. Once you finish the registration, you will be authorized to use the application if the Che-Server login credentials are correct.

1. Create a file to list down the details of the application you want to register with OC-ID. In the file `ocid-apps.toml`, mention the application's **name** and the **redirect_uri**. The content of the created file looks like as shown below:

    ```cd
    [ocid.v1.sys.ocid.oauth_application_config]
        [[ocid.v1.sys.ocid.oauth_application_config.oauth_applications]]
            name = ""
            redirect_uri = ""
    ```

    Update the URL of the supermarket website in the `redirect_uri` as per the actual supermarket URL. Refer to the code below:

    ```cd
    [ocid.v1.sys.ocid.oauth_application_config]
        [[ocid.v1.sys.ocid.oauth_application_config.oauth_applications]]
            name = "supermarket"
            redirect_uri = "https://example-supermarket.com/auth/chef_oauth2/callback"
    ```

    To add more than one application with the OC-ID service, keep repeating the above code in the file with the respective application details. For example:

    ```cd
    [ocid.v1.sys.ocid.oauth_application_config]
        [[ocid.v1.sys.ocid.oauth_application_config.oauth_applications]]
            name = "application-1"
            redirect_uri = "https://application-1.com/auth/chef_oauth2/callback"
        [[ocid.v1.sys.ocid.oauth_application_config.oauth_applications]]
            name = "application-2"
            redirect_uri = "https://application-2.com/auth/chef_oauth2/callback"
    ```

    Using the above snippet, you can register two applications to the OC-ID.

1. Now, patch the above file by running the below command from your habitat studio `/src` directory or your bastion system:

    ```bash
    chef-automate config patch ocid-apps.toml
    ```

Once the patch is successfully applied, the new application will get registered with Chef Identity.

1. Verify whether the new configuration has been applied or not by running the following command:

    ```bash
    chef-automate config show
    ```

    The output of the above command will contain the values from the file you patched.

1. Run the following `ctl` command to get the details of the applications registered with OC-ID.

    ```cd
    chef-automate config oc-id-show-app
    ```

    The output of the above command is as shown below:

    ```cd
    supermarket:
    -   name: supermarket
        redirect_uri: https://example-supermarket.com/auth/chef_oauth2/callback
        uid: 735c44e423787134839ce1bdb6b2ab8bd9eca5b656f0f4e69df3641ea494cdda
        secret: 4c371ceb46465b162c0b4a670573d80ac1d6adeebaa2638db53bb9f94d432340
        id:
    ```

## Supermarket Configuration

To configure the supermarket in Chef Automate, follow the steps given below:

1. SSH into the ec2 instance where the supermarket is installed. Then run the following commands:

    ```bash
    sudo su
    cd /etc/supermarket
    ```

1. Update the `supermarket.rb` file in the `/etc/supermarket` directory with the application details retrieved from the automate instance after registering supermarket as an oauth application with OC-ID:

    ```cd
    default['supermarket']['chef_oauth2_app_id'] = "<uid>"
    default['supermarket']['chef_oauth2_secret'] = "<secret>"
    default['supermarket']['chef_oauth2_url'] = "<automate_url>"
    default['supermarket']['chef_oauth2_verify_ssl'] = false
    ```

    The flag `chef_oauth2_verify_ssl` value is boolean and should be based on whether you have a valid(non self-signed certificate) certificate for automate. If you have a valid certificate, set it as `true`, or else set it as `false`.

1. Now, run the following `reconfigure` command to reflect the above changes in the running supermarket application:

    ```bash
    appsupermarket-ctl reconfigure
    ```

1. Once reconfiguring is completed, visit the supermarket website on the browser. Refer to the image given below:

    {{< figure src="/images/automate/standalone_supemarket_landing_page.png" alt="Supermarket Landing Page">}}

1. Hit the supermarket URL and select Sign In. You will be redirected to the Chef Identity page running inside Automate. Refer to the image below:

    {{< figure src="/images/automate/standalone_supermaket_sign-in.png" alt="Supermarket Sign In Page">}}

1. Sign in with the already created user credentials to authorize the supermarket app. Refer to the image below:

    {{< figure src="/images/automate/standalone_supermarket_credentials_signin.png" alt="Supermarket Credentials">}}

1. Authorize the supermarket to use your Chef account. Refer to the image below:

    {{< figure src="/images/automate/standalone_supermarket_credentials_signin.png" alt="Supermarket Authorization Page">}}

1. Once the supermarket application is successfully authorized, the screen looks like as shown in the below image:

    {{< figure src="/images/automate/standalone_supermarket_app_board.png" alt="Supermarket Board">}}

You have successfully logged in to the supermarket using the credentials of `chef-server` through the **Chef Identity** service running as part of Airgapped Automate.
