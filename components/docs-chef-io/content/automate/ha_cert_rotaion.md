+++
title = "Certificate Rotation"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Certificate Rotation"
    parent = "automate/deploy_high_availability/certificates"
    identifier = "automate/deploy_high_availability/certificates/ha_cert_rotaion.md Certificate Rotation"
    weight = 220
+++

Certificate rotation is the replacement of existing certificates with new ones when any certificate expires or is based on your organization's policy. A new CA authority is substituted for the old, requiring a replacement of the root certificate for the cluster.

The certificate rotation is also required when the key for a node, client, or CA is compromised. If compromised, you need to change the contents of a certificate. For example, to add another DNS name or the IP address of a load balancer to reach a node, you have to rotate only the node certificates.

## Rotate using OpenSSL

You can generate the required certificates or use your organization's existing certificates. To rotate your certificates(from `cd /hab/a2_deploy_workspace` path), that are used in Chef Automate High Availability (HA), follow the steps below:

-   Navigate to your workspace folder(example: `cd /hab/a2_deploy_workspace`) and execute the `./scripts/credentials set ssl --rotate all` command. The command rotates all the certificates of your organization.

    {{< note >}} When you run the above command for the first time, a series of certificates are created and saved in `/hab/a2_deploy_workspace/certs` location. Identify the appropriate certificate. For example, to rotate certificates for **PostgreSQL**, use certificate values into _pg_ssl_private.key_, _pg_ssl_public.pem_, and _ca_root.pem_. Similarly, to rotate certificates for **OpenSearch**, use certificate values into _ca_root.pem_, _oser_admin_ssl_private.key_, _oser_admin_ssl_public.pem_, _oser_ssl_private.key_, _oser_ssl_public.pem_, _kibana_ssl_private.key_, and _kibana_ssl_public.pem_. {{< /note >}}

-   To rotate the PostgreSQL certificates, execute the `./scripts/credentials set ssl --pg-ssl` command.

-   To rotate the OpenSearch certificates, execute the `./scripts/credentials set ssl --oser-ssl` command.

<!-- 4. Copy your *x.509 SSL certs* into the appropriate files in `certs/` folder. -->

<!-- - Place your root certificate into `ca_root.pem file`. -->

<!-- - Place your intermediate CA into the `pem` file. -->

-   If your organization issues a certificate from an intermediate CA, place the respective certificate after the server certificate as per the order listed. In the `certs/pg_ssl_public.pem`, place the following ordered list:

    -   Server Certificate
    -   Intermediate CA Certificate 1
    -   Intermediate CA Certificate n

-   Execute the `./scripts/credentials set ssl` command (with the appropriate options). This command deploys the nodes.

-   Execute the `./scripts/credentials set ssl --help` command. The command will provide an information and a list of commands related to certificate rotation.

-   For rotating the PostgreSQL credentials, execute the `./scripts/credentials set postgresql --auto` command.

-   For rotating the OpenSearch credentials, execute the `./scripts/credentials set opensearch --auto` command.

{{< figure src="/images/automate/ha_cert_rotation.png" alt="Certificate Rotation">}}

## Rotate using Own Organization Certificates

To use existing certificates of your organization, follow the steps given below:

{{< note >}} All the commands will run from `/hab/a2_deploy_workspace`. {{< /note >}}

-   Run `./scripts/credentials set ssl --rotate-all`

    -   Run this command (for the first time) to create a skeleton of certificates. The certificate can be located in `/hab/a2_deploy_workspace/certs` directory. For example: to rotate the certificates for **PostgreSQL**, save the content of the certificate in `pg_ssl_private.key`, `pg_ssl_public.pem`, and `ca_root.pem`. Similarly, tto rotate the PostgreSQL certificate, run the command. The `ca_root` will remain the same for all the certificates if the `ca_root` is same for all the other certificates like opensearch, kibana, or frontend. The only thing which changes is the content of the appropriate certificate.

    -   To rotate OpenSearch certificates, insert the content of the certificate of CA to `ca_root.pem`, `oser_admin_ssl_private.key`, `oser_admin_ssl_public.pem`, `oser_ssl_private.key`, `oser_ssl_public.pem`, `kibana_ssl_private.key`, and `kibana_ssl_public.pem`.

    -   To rotate a specific certificate, run the following commands:

    `./scripts/credentials set ssl --pg-ssl` (This will rotate PostgreSQL Certificates)

    `./scripts/credentials set ssl --es-ssl`

    -   To change all the certificates, add contents to the appropriate file and run the following command:

    `./scripts/credentials set ssl --rotate-all`

    -   The following command will give you all the information about certificate rotation:

    `./scripts/credentials set ssl --help`

If your organization issues a certificate from an Intermediate CA, add the certificate in the sequence as shown below:

```sh
Server Certificate
Intermediate CA Certificate 1
Intermediate CA Certificate n
```

Copy the above content to `certs/pg_ssl_public.pem`.
