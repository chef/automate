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

- Either existing certificates can be used or to generate new ones, click [here](https://docs.chef.io/automate/ha_cert_selfsign/)

## Rotate using Cert-Rotate Command

{{< note >}}

- Below `cert-rotate` commands can only be executed from `bastion host`.
- If you want to use certificates stored in another node of the HA cluster, you can provide the remote path to the certificates using the `<IP_ADDRESS_OF_NODE>:<ABSOLUTE_PATH_TO_THE_CERT_FILE>` format instead of the local path.
- `--wait-timeout` This flag sets the operation timeout duration (in seconds) for each individual node during the certificate rotation process.

{{< /note >}}

### Rotate Cluster Certificates
If you want to rotate certificates of the entire cluster using single command, then you can follow the below commands:

To rotate certificates of entire cluster using single command, we need a certificate template.
- To generate certificate template use below command
```
  chef-automate cert-rotate generate-certificate-config certificate-config.toml
```

now we can find our certificate template in `certificate-config.toml` file, please edit the file and put required certificate paths

- To rotate the certificates use below command

```bash
  chef-automate cert-rotate --certificate-config certificate-config.toml
```

You can also use `--cc` instead of `--certificate-config` as a sort form.


#### Sample Certificate template

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


### Rotate Certificates of each service

If you want to rotate certificates of the entire cluster, then you can follow the below commands:

- To rotate certificates of automate cluster:

    ```cmd 
    chef-automate cert-rotate --public-cert <path of public certificate of automate node> --private-cert <path of private certificate of automate node> --a2
    ```

    You can also use `--automate` or `-a` instead of a2 flag

- To rotate certificates of chef server cluster:

    ```cmd
    chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --cs
    ```

    You can also use `--chef_server`or `-c` instead of the cs flag.

- To rotate certificates of the PostgreSQL cluster:

    ```cmd
    chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --root-ca <path of root certificate> --pg
    ```

    You can also use `--postgresql` or `-p` instead of the pg flag.

- To rotate certificates of OpenSearch cluster:

    ```cmd
    chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --root-ca <path of root certificate> --admin-cert <path of admin certificate> --admin-key <path of admin key> --os
    ```

    You can also use `--opensearch` or `-o` instead of the os flag.

### Rotate Certificates of Particular Node

{{< note >}} If you want to apply the unique certificates which are generated from different root certificate (which is not applied on the cluster), then you have to first run the above cluster command. After that, you can run the commands below so the connection will not break. But if it is not the case, i.e., you want to apply the certificates generated from the same root certificate, then you can directly run the below commands. {{< /note >}}

If you want to rotate certificates of a particular node, then you can follow the below commands:

- To rotate the certificates of particular automate node:

    ```cmd
    chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --a2 --node <IP of a particular automate node>
    ```

    You can also use `--automate` or `-a` instead of a2 flag

- To rotate the certificates of particular chef server node:

    ```cmd
    chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --cs --node <IP of a particular chef server node>
    ```

    You can also use `--chef_server` or `-c` instead of the cs flag.

- To rotate the certificates of a particular PostgreSQL node:

    {{< note >}} While rotating cert for PG on node level, make sure to wait for sometime before executing cert-rotate for next node. {{< /note >}}


    ```cmd
    chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --pg --node <IP of a particular postgresql node>
    ```

    You can also use `--postgresql` or `-p` instead of the pg flag.

- To rotate the certificates of a particular OpenSearch node:

    ```cmd
    chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --os --node <IP of a particular opensearch node>
    ```

    You can also use `--opensearch` or `-o` instead of the os flag

{{< note >}} Since admin-cert and admin-key are common in all nodes, So if you want to rotate admin-cert and admin-key, you must first run this open search cluster command: 
`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --root-ca <path of root certificate> --admin-cert <path of admin certificate> --admin-key <path of admin key> --os`{{< /note >}}


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

    ```shell
    chef-automate config patch root_ca.toml --cs
    ```
