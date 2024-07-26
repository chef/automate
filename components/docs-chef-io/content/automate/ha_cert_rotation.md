+++
title = "Certificate Rotation"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Certificate Rotation"
    parent = "automate/deploy_high_availability/certificates"
    identifier = "automate/deploy_high_availability/certificates/ha_cert_rotation.md Certificate Rotation"
    weight = 230
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

Certificate rotation replaces existing certificates with new ones when any certificate expires or is based on your organization's policy. A new CA authority is substituted for the old, requiring a replacement of the root certificate for the cluster.

The certificate rotation is also required when the key for a node, client, or CA is compromised. If compromised, you need to change the contents of a certificate. For example, to add another DNS name or the IP address of a load balancer to reach a node, you have to rotate only the node certificates.

## Prerequisites

[Existing certificates](https://docs.chef.io/automate/ha_cert_selfsign/) can be used to generate new ones.

Generate the certificates using recommended tools and supported algorithms and versions mentioned below:

- OpenSSL: 1.0.2zb-fips
- OpenSSL Algorithms: PBE-SHA1-3DES, RSA (2048), SHA-256
- Certificate Format: X509 V3(PEM format) ,Private key is in PKCS8 format

To understand how to generate certificates, refer to the [Certificate Generation](/automate/ha_cert_selfsign/#creating-a-certificate) documentation.

## Rotate using Cert-Rotate Command

{{< note >}}

- Below `cert-rotate` commands can only be executed from `bastion host`.
- If you want to use certificates stored in another node of the HA cluster, you can provide the remote path to the certificates using the `<IP_ADDRESS_OF_NODE>:<ABSOLUTE_PATH_TO_THE_CERT_FILE>` format instead of the local path.
- `--wait-timeout` This flag sets the operation timeout duration (in seconds) for each individual node during the certificate rotation process.
- Certificate rotation should be done in down-time window as service will restart.
{{< /note >}}

### Rotate Cluster Certificates

If you want to rotate certificates of the entire cluster using single command, then you can follow the below commands:

To rotate certificates of entire cluster using single command, we need a certificate template.

- To generate certificate template use below command

    ```sh
    chef-automate cert-rotate generate-certificate-config certificate-config.toml
    ```

    Find the certificate template and add the required certificate paths in `certificate-config.toml` file.

- To rotate the certificates use below command

    ```sh
    chef-automate cert-rotate --certificate-config certificate-config.toml
    ```

#### Sample Certificate Template

```toml
[automate]
  root_ca = "full path of root-ca.pem"

  [[automate.ips]]
    ip = "10.1.0.130"
    public_key = "full path of automate1.pem"
    private_key = "full path of automate1-key.pem"

[chef_server]
  root_ca = "full path of root-ca.pem"

  [[chef_server.ips]]
    ip = "10.1.0.16"
    public_key = "full path of  cs1.pem"
    private_key = "full path of cs1-key.pem"

[postgresql]
  root_ca = "full path of root-ca.pem"

  [[postgresql.ips]]
    ip = "10.1.0.141"
    public_key = "full path of pg1.pem"
    private_key = "full path of pg1-key.pem"

  [[postgresql.ips]]
    ip = "10.1.1.190"
    public_key = "full path of pg2.pem"
    private_key = "full path of pg2-key.pem"

  [[postgresql.ips]]
    ip = "10.1.2.130"
    public_key = "full path of pg3.pem"
    private_key = "full path of pg3-key.pem"

[opensearch]
  root_ca = "full path of root-ca.pem"
  admin_public_key = "full path of os-admin.pem"
  admin_private_key = "full path of os-admin-key.pem"
  [[opensearch.ips]]
    ip = "10.1.0.176"
    public_key = "full path of os1.pem"
    private_key = "full path of os1-key.pem"

  [[opensearch.ips]]
    ip = "10.1.1.125"
    public_key = "full path of os2.pem"
    private_key = "full path of os2-key.pem"

  [[opensearch.ips]]
    ip = "10.1.2.247"
    public_key = "full path of os3.pem"
    private_key = "full path of os3-key.pem"
```

### Rotate Automate Load Balancer Root CA

To rotate the Automate Load balancer root certificate:

1. Create a root_ca.toml file with the following content. Replace server_name with Automate Fqdn and root_cert with Automate Load balancer root certificate. 

    ```toml
    [cs_nginx.v1.sys.ngx.http]
      ssl_verify_depth = 6
    [global.v1.external.automate.ssl]
      server_name = "https://<automatefqdn.example.com>"
      root_cert = """<Root_CA_Content>"""
    ```

1. Run the following command to apply your configuration on Chef-Server from bastion:

    ```sh
    chef-automate config patch root_ca.toml --cs
    ```
