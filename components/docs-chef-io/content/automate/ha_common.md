+++
title = "Reference Topics"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Reference Topics"
    parent = "automate/deploy_high_availability/reference"
    identifier = "automate/deploy_high_availability/reference/ha_common.md Reference Topics"
    weight = 240
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

This page details the topics that are common to Chef Automate High Availability (HA) deployment models and aid you in deploying it in your network infrastructure.

## Certificates

A security certificate is a small data file used as an Internet security technique through which the identity, authenticity and reliability of a website or Web application is established.

Certificates should be rotated periodically, to ensure optimal security.

### How is Certificate Rotation Helpful?

Certificate rotation means the replacement of existing certificates with new ones when any certificate expires or based on your organization policy. A new CA authority is substituted for the old requiring a replacement of root certificate for the cluster.

The certificate rotation is also required when key for a node, client, or CA is compromised. Then, you need to modify the contents of a certificate, for example, to add another DNS name or the IP address of a load balancer through which a node can be reached. In this case, you  would need to rotate only the node certificates.

#### How to Rotate the Certificates?

You can generate the required certificates on your own or you can use the existing certificates of your organization. Ensure you execute all the below commands from the `cd /hab/a2_deploy_workspace` path.

Follow these steps to rotate your certificates that are to be used in Chef Automate High Availability (HA):

1. Navigate to your workspace folder. For example, `cd /hab/a2_deploy_workspace`.
1. Type the command, `./scripts/credentials set ssl --rotate all` and press **Enter**. This command rotates all the certificates of your organization.

    {{< note >}}

    When you run this command first time, a series of certificates are created and saved in `/hab/a2_deploy_workspace/certs` location. You need to identify the appropriate certificates. For example, to rotate certificates for PostgreSQL, use certificate values into *pg_ssl_private.key*,  *pg_ssl_public.pem*, and *ca_root.pem*. Likewise, to rotate certificates for Elasticsearch, use certificate values into *ca_root.pem*, *es_admin_ssl_private.key*, *es_admin_ssl_public.pem*, *es_ssl_private.key*, *es_ssl_public.pem*, *kibana_ssl_private.key*, *kibana_ssl_public.pem*.

    {{< /note >}}

1. For rotating the PostgreSQL certificates, type the command `./scripts/credentials set ssl --pg-ssl` and press **Enter**. .

1. For rotating the Elasticsearch certificates, type the command, `./scripts/credentials set ssl --es-ssl` and press **Enter**.

    <!-- 4. Copy your *x.509 SSL certs* into the appropriate files in `certs/` folder. -->

    <!-- - Place your root certificate into `ca_root.pem file`. -->

    <!-- - Place your intermediate CA into the `pem` file. -->

1. If your organization issues certificate from an intermediate CA, then place the respective certificate after the server certificate as per order listed. For example, in `certs/pg_ssl_public.pem`, paste it as them as listed:

    - Server Certificate
    - Intermediate CA Certificate 1
    - Intermediate CA Certificate n

1. Type the command, `./scripts/credentials set ssl` (with the appropriate options) and press **Enter**. This command deploys the nodes.

1. Type the command, `./scripts/credentials set ssl  --help` and press **Enter**. This command provides you information and list of commands related to certificate rotation.

1. For rotating the PostgreSQL credentials, type the command `./scripts/credentials set postgresql --auto` and press **Enter**. .

1. For rotating the Elasticsearch credentials, type the command, `./scripts/credentials set elasticsearch --auto` and press **Enter**.

### What are Self Signed Certificates?

A self signed certificate is a digital certificate that is not signed by a publicly trusted certificate authority (CA). They are created, issued, and signed by the company or developer who is responsible for the website or software being signed. The private key used in such certificate is not validated by a third party and is generally used in low-risk internal networks or in the software development phase. In addition, unlike CA-issued certificates, self-signed certificates cannot be revoked.

#### Certificate Creation

You can create a self-signed key and certificate pair with OpenSSL utility, a command line tool for creating and managing OpenSSL certificates, keys, and other files.

Follow these steps to create a self-sign certificate:

1. Install an *openssl* utility.

1. Navigate to your workspace folder. For example, `cd /hab/a2_deploy_workspace`.

1. Type the command, `./scripts/credentials set ssl --rotate-all`. This command creates a skeleton of certificates.

1. Copy the below *bash script* to a new file:

    ```bash
    # !/bin/bash

    echo extendedKeyUsage = clientAuth, serverAuth > server_cert_ext.cnf

    echo extendedKeyUsage = clientAuth, serverAuth > client_cert_ext.cnf

    openssl genrsa -out ca_root.key 2048

    openssl req -x509 -new -key ca_root.key -sha256 -out ca_root.pem -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefrootca'

    openssl genrsa -out admin-pkcs12.key 2048

    openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "admin-pkcs12.key" -topk8 -out "es_admin_ssl_private.key" -nocrypt

    openssl req -new -key es_admin_ssl_private.key -out admin.csr -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefadmin'

    openssl x509 -extfile <(printf "subjectAltName=DNS:chefadmin") -req -in admin.csr -CA ca_root.pem -CAkey ca_root.key -CAcreateserial -out es_admin_ssl_public.pem -sha256 -extfile server_cert_ext.cnf

    openssl genrsa -out ssl-pkcs12.key 2048

    openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "ssl-pkcs12.key" -topk8 -out  es_ssl_private.key -nocrypt

    openssl req -new -key es_ssl_private.key -out ssl.csr -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefnode'

    openssl x509 -extfile <(printf "subjectAltName=DNS:chefnode") -req -in ssl.csr -CA ca_root.pem -CAkey ca_root.key -CAcreateserial -out es_ssl_public.pem -sha256 -extfile client_cert_ext.cnf

    cp ca_root.pem /hab/a2_deploy_workspace/certs/ca_root.pem

    cp es_admin_ssl_public.pem /hab/a2_deploy_workspace/certs/es_admin_ssl_public.pem

    cp es_admin_ssl_private.key /hab/a2_deploy_workspace/certs/es_admin_ssl_private.key

    cp es_ssl_public.pem /hab/a2_deploy_workspace/certs/es_ssl_public.pem

    cp es_ssl_private.key /hab/a2_deploy_workspace/certs/es_ssl_private.key

    cp es_admin_ssl_private.key /hab/a2_deploy_workspace/certs/kibana_ssl_private.key

    cp es_admin_ssl_public.pem /hab/a2_deploy_workspace/certs/kibana_ssl_public.pem

    cp es_ssl_private.key /hab/a2_deploy_workspace/certs/pg_ssl_private.key

    cp es_ssl_public.pem /hab/a2_deploy_workspace/certs/pg_ssl_public.pem
    ```

1. Navigate to your bastion host.

1. Execute the new file that has the copied bash script. The script generates the certificates at `/hab/a2_deploy_worspace/certs` directory. For example, `bash cert.sh`, where *cert.sh is the name of the newly created bash script file.

1. Again, navigate to your workspace folder. For example, `cd /hab/a2_deploy_workspace`.

1. Execute following commands in the same order as listed to apply the generated certificates:

    - ./scripts/credentials set ssl --es-ssl
    - ./scripts/credentials set ssl --pg-ssl
    - ./scripts/credentials set ssl --kibana-ssl

    Once the certificates are applied successfully, the following confirmation message appears as shown in the screen:

    ```bash
        STDERR:
        EXIT_STATUS: 0
        I, [2021-12-20T16:44:20.979703 #411121] INFO - - : STDOUT: >> Setting new configuration version 1640018660 for automate-ha-postgresql.default
        Creating service configuration
        Applying via peer 127.0.0.1:9632
        * Applied configuration

        STDERR:
        EXIT_STATUS: 0
        I, [2021-12-20T16:44:20.979815 #411121] INFO - - : * SSL Certificates Rotated *
    ```

1. Navigate to the Chef Automate and Chef Server instances and check the Chef Service health status. If the service is down or critical, then  wait for three to four minutes for the instances to be up.

## Destroying Chef Automate HA Infrastructure

Follow any of the step to destroy terraform or infrastructure created while deploying Chef Automate High Availability (HA) using AWS or Bare-metal model.

- If the *provision-infra* command fails, execute the following command to clear the space utilized by this command before it fails, execute the command:

`for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/;terraform destroy -state=/hab/a2_deploy_workspace/terraform/destroy/aws/terraform.tfstate;cd $i;done`

- If the *provision-infra* command ran successfully and if you want to clear the space utilized by this command, execute these commands in the order listed:

1. `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done`.

1. `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done`.

    - If you have deployed the Chef Automate HA successfully and wanted to destroy the deployment part alone, execute the command:

    `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/;terraform destroy;cd $i;done`

    {{< note spaces=4 >}}

    The deployment destroy does not remove any remote server configuration made, however it taints the terraform and thus you need to redo the configurations.

    {{< /note >}}

    - If you have deployed the Chef Automate HA successfully and wanted to destroy the entire infrastructure instances, execute these commands:

    `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done`.

    `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done`.

## Validation Commands

This section elaborates the validation procedure that checks the firewall rules and ports before Chef Automate High Availability (HA) backend cluster deployment in your network infrastructure.

Follow these steps to examine the firewall rules are stateful, and ports are open before Chef Automate High Availability (HA) backend cluster deployment in air-gapped environment (means no access to the internet):

1. Download hab, *hab-x86_64-linux.tar.gz* by executing the command, `sudo wget https://packages.chef.io/files/stable/habitat/latest/hab-x86_64-linux.tar.gz`.

1. Install hab package in your internet environment by executing the following commands that generate *netcate package*:

    ```bash

    sudo tar -xvzf /tmp/hab-x86_64-linux.tar.gz -C /usr/local/bin --strip-components 1
    export HAB_LICENSE=accept-no-persist
    hab pkg install core/netcat -bf
    ls -dtr1 /hab/cache/artifacts/core-netcat-*
    ```

1. Provide the path of the `config.toml` file, `hab-utitlity` and `netcate` package in the command, ./chef-automate validate-ha-infrastructure */path/to/config.toml* */path/to/hab.tar.gz* */path/to/netcat.hart*  as parameters.

    ```bash
    ./chef-automate validate-ha-infrastructure /root/config.toml /root/hab-x86_64-linux.tar.gz /hab/cache/artifact/core-netcat-<version>.hart
    ```

This command show the status of the set firewall rules and the ports configured.

### Validation Procedure for Airgap Environment

You need to execute the following command to examine the firewall rules are stateful, and ports are open before Chef Automate High Availability (HA) backend cluster deployment in your network environment which has access to the internet:

`./chef-automate validate-ha-infrastructure /path/to/config.toml`

where you need to provide the path of the *config.toml* file in */path/to/config.toml* in the above command.

This command show the status of the set firewall rules and the ports configured.
