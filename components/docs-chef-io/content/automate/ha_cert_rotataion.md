+++
title = "Certificate Rotation"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA - Certificate Rotation"
    parent = "automate/High_Availability"
    identifier = "automate/reference/ha_cert_rotataion.md Chef Automate Certificate Rotation"
    weight = 20
+++

## Certificate Rotation 

Certificate rotation means the replacement of existing certificates with new ones. It is needed when any certificate expires. A new CA authority is substituted for the old requiring a replacement of root certificate for the cluster.

## How to Rotate the Certificate

You can generate certificate freshly and then rotate them using  ?? or if you want to use existing certificates of your organization, follow below steps.

Follow these steps to generate certificate files and replace the actual one by actual certificates signed by a Certificate Authority (CA).

?? why??

1. Navigate to your workspace folder. For example, `cd /hab/a2_deploy_workspace`.
2. Type the command, `./scripts/credentials set ssl --help `.

3. Type the command, `./scripts/credentials set ssl` with the appropriate options to generate list of skeleton files. ??

4. Copy your *x.509 SSL certs* into the appropriate files in `certs/` folder.

    - Place your root certificate into `ca_root.pem file`. 

    - Place your intermediate CA into the `pem` file. 

5. If your organization issues certificate from an intermediate CA, then place the respective certificate after the server certificate as per order listed. For example, in `certs/pg_ssl_public.pem`, paste it as them as listed:

   - Server Certificate 
   - Intermediate CA Certificate 1 
   - Intermediate CA Certificate n 

6. Type the command, `./scripts/credentials set ssl` (with the appropriate options) and press **Enter**. This command deploys the nodes. You can check the options using --help command. 

Have to cover the below ones?

- For rotating the postgresql certificates, ./scripts/credentials set postgresql --auto

- For rotating the elasticsearch certificates, ./scripts/credentials set elasticsearch --auto

- And to rotate all certificates in one command, ./scripts/credentials set ssl --rotate-all
