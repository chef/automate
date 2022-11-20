+++
title = "Certificate Rotation"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Certificate Rotation"
    parent = "automate/deploy_high_availability/certificates"
    identifier = "automate/deploy_high_availability/certificates/ha_cert_rotaion.md Certificate Rotation"
    weight = 230
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

Certificate rotation is the replacement of existing certificates with new ones when any certificate expires or is based on your organization's policy. A new CA authority is substituted for the old, requiring a replacement of the root certificate for the cluster.

The certificate rotation is also required when the key for a node, client, or CA is compromised. If compromised, you need to change the contents of a certificate. For example, to add another DNS name or the IP address of a load balancer to reach a node, you have to rotate only the node certificates.


## Prerequisites 
- Either existing certificates can be used or to generate new one Click [here](https://docs.chef.io/automate/ha_cert_selfsign/)
## Rotate using Cert-Rotate Command

{{< note >}} Below `cert-rotate` commands can only be executed from `bastion host` {{< /note >}}

### Rotate Certificates of each service

If you want to rotate certificates of entire cluster, then you can follow the below commands:

- To rotate certificates of automate cluster:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --root-ca <path of root certificate> --a2`

You can also use `--automate` or `-a` instead of a2 flag

- To rotate certificates of chef server cluster:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --cs`

You can also use `--chef_server`or `-c` instead of cs flag

- To rotate certificates of postgresql cluster:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --root-ca <path of root certificate> --pg`

You can also use `--postgresql` or `-p` instead of pg flag

- To rotate certificates of opensearch cluster:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --root-ca <path of root certificate> --admin-cert <path of admin certificate> --admin-key <path of admin key> --os`

You can also use `--opensearch` or `-o` instead of os flag

### Rotate Certificates of Particular Node

{{< note >}} If you want to apply the unique certificates which are generated from different root certificate (which is not applied on cluster), then you have to first run the above cluster command, and after that you can run the below commands so that the connection will not break. But if it is not the case i.e. you want to apply the certificates generated from same root certificate, then you can directly run the below commands. {{< /note >}}

If you want to rotate certificates of particular node, then you can follow the below commands:

- To rotate the certificates of particular automate node:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --a2 --node <IP of a particular automate node>`

You can also use `--automate` or `-a` instead of a2 flag

- To rotate the certificates of particular chef server node:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --cs --node <IP of a particular chef server node>`

You can also use `--chef_server` or `-c` instead of cs flag

- To rotate the certificates of particular postgresql node:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --pg --node <IP of a particular postgresql node>`

You can also use `--postgresql` or `-p` instead of pg flag

- To rotate the certificates of particular opensearch node:

`chef-automate cert-rotate --public-cert <path of public certificate> --private-cert <path of private certificate> --os --node <IP of a particular opensearch node>`

You can also use `--opensearch` or `-o` instead of os flag