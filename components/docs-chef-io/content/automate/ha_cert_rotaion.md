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

## What are Certificates?

A security certificate is a small data file used as an Internet security technique through which the identity, authenticity, and reliability of a website or Web application are established.

Rotate the certificates periodically to ensure optimal security.

## Certificate Rotation

Certificate rotation means the replacement of existing certificates with new ones when any certificate expires or is based on your organization's policy. A new CA authority is substituted for the old, requiring a replacement of the root certificate for the cluster.

The certificate rotation is also required when the key for a node, client, or CA is compromised. If compromised, you need to change the contents of a certificate, for example, to add another DNS name or the IP address of a load balancer through which can reach a node. In this case, you would need to rotate only the node certificates.

## Rotate using OpenSSL

You can generate the required certificates or use your organization's existing certificates. Ensure you execute all the below commands from the `cd /hab/a2_deploy_workspace` path.

Follow these steps to rotate your certificates that are used in Chef Automate High Availability (HA):

- Navigate to your workspace folder. For example, `cd /hab/a2_deploy_workspace`.
- Execute the command, `./scripts/credentials set ssl --rotate all`. This command rotates all the certificates of your organization.

  {{< note >}}

  When you run this command first time, a series of certificates are created and saved in `/hab/a2_deploy_workspace/certs` location. You need to identify the appropriate certificates. For example, to rotate certificates for PostgreSQL, use certificate values into *pg_ssl_private.key*,  *pg_ssl_public.pem*, and *ca_root.pem*. Likewise, to rotate certificates for OpenSearch, use certificate values into *ca_root.pem*, *es_admin_ssl_private.key*, *es_admin_ssl_public.pem*, *es_ssl_private.key*, *es_ssl_public.pem*, *kibana_ssl_private.key*, *kibana_ssl_public.pem*.

  {{< /note >}}

- For rotating the PostgreSQL certificates, execute the command `./scripts/credentials set ssl --pg-ssl`.

- For rotating the opensearch certificates, execute the command, `./scripts/credentials set ssl --es-ssl` .

<!-- 4. Copy your *x.509 SSL certs* into the appropriate files in `certs/` folder. -->

<!-- - Place your root certificate into `ca_root.pem file`. -->

<!-- - Place your intermediate CA into the `pem` file. -->

- If your organization issues a certificate from an intermediate CA, place the respective certificate after the server certificate as per the order listed. For example, in `certs/pg_ssl_public.pem`, paste it as them as listed:

   - Server Certificate
   - Intermediate CA Certificate 1
   - Intermediate CA Certificate n

- Execute the command, `./scripts/credentials set ssl` (with the appropriate options). This command deploys the nodes.

- Type the command, `./scripts/credentials set ssl  --help`. This command provides you with information and a list of commands related to certificate rotation.

- For rotating the PostgreSQL credentials, execute the command `./scripts/credentials set postgresql --auto`.

- For rotating the opensearch credentials, execute the command, `./scripts/credentials set opensearch --auto`.

{{< figure src="/images/automate/ha_cert_rotation.png" alt="Certificate Rotation">}}

## Rotate using Own Organization Certificates

To use existing certificates of your organization, follow the steps given below:

{{< note >}} This all command will run from the /hab/a2_deploy_workspace. {{< /note >}}

- Run `./scripts/credentials set ssl --rotate-all`

  - Run this command (for the first time) to create a skeleton of certificates. The certificate can be location in `/hab/a2_deploy_workspace/certs` directory. 
  For example: To rotate the certificates for **PostgreSQL**, save the content of the certificate in `pg_ssl_private.key`, `pg_ssl_public.pem` and `ca_root.pem`. Now, to rotate the PostgreSQL certificate, run the command. The `ca_root` will remain the same for all the certificates if the  `ca_root` is same for all the other certificates like opensearch, kibana, or frontend. The only thing which changes is the content of the appropriate certificate.

  - To rotate OpenSearch certificates, insert the content of the certificate of CA to `ca_root.pem`, `es_admin_ssl_private.key`, `es_admin_ssl_public.pem`, `es_ssl_private.key`, `es_ssl_public.pem`, `kibana_ssl_private.key`, and `kibana_ssl_public.pem`.

  - To rotate a specific certificate, run the following commands:

  `./scripts/credentials set ssl  --pg-ssl` (This will rotate PostgreSQL Certificates)

  `./scripts/credentials set ssl  --es-ssl`

  - To change all the certificates, add contents to the appropriate file and run the following command:

  `./scripts/credentials set ssl --rotate-all`

  - The following command will give you all the information about certificate rotation:

    `./scripts/credentials set ssl  --help`

If your organization issues a certificate from an Intermediate CA, add the certificate in the sequence shown below:

```sh
Server Certificate 

Intermediate CA Certificate 1 

Intermediate CA Certificate n 
```

Copy the above content to `certs/pg_ssl_public.pem`.
