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

### Rotate Certificates of each service

If you want to rotate certificates of the entire cluster, then you can follow the below commands:

- To rotate certificates of automate cluster:

`chef-automate cert-rotate --public-cert <path of public certificate of automate node> --private-cert <path of private certificate of automate node> --a2`

You can also use `--automate` or `-a` instead of a2 flag

- To rotate certificates of chef server cluster:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --cs`

You can also use `--chef_server`or `-c` instead of the cs flag.

- To rotate certificates of the PostgreSQL cluster:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --root-ca <path of root certificate> --pg`

You can also use `--postgresql` or `-p` instead of the pg flag.

- To rotate certificates of OpenSearch cluster:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --root-ca <path of root certificate> --admin-cert <path of admin certificate> --admin-key <path of admin key> --os`

You can also use `--opensearch` or `-o` instead of the os flag.

### Rotate Certificates of Particular Node

{{< note >}} If you want to apply the unique certificates which are generated from different root certificate (which is not applied on the cluster), then you have to first run the above cluster command. After that, you can run the commands below so the connection will not break. But if it is not the case, i.e., you want to apply the certificates generated from the same root certificate, then you can directly run the below commands. {{< /note >}}

If you want to rotate certificates of a particular node, then you can follow the below commands:

- To rotate the certificates of particular automate node:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --a2 --node <IP of a particular automate node>`

You can also use `--automate` or `-a` instead of a2 flag

- To rotate the certificates of particular chef server node:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --cs --node <IP of a particular chef server node>`

You can also use `--chef_server` or `-c` instead of the cs flag.

- To rotate the certificates of a particular PostgreSQL node:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --pg --node <IP of a particular postgresql node>`

You can also use `--postgresql` or `-p` instead of the pg flag.

- To rotate the certificates of a particular OpenSearch node:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --os --node <IP of a particular opensearch node>`

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
