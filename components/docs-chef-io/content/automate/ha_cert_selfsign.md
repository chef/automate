+++
title = "Self Sign Certificates"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Self Sign Certificates"
    parent = "automate/deploy_high_availability/certificates"
    identifier = "automate/deploy_high_availability/certificates/ha_cert_selfsign.md Self Sign Certificates"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

## What are Certificates?

A security certificate is a small data file used as an Internet security technique through which the identity, authenticity, and reliability of a website or web application are established.

To ensure optimal security, rotate the certificates periodically.

## What are Self Signed Certificates?

A self-signed certificate is a digital certificate that is not signed by a publicly trusted certificate authority (CA). They are created, issued, and signed by the company or developer responsible for the website or software. The third party in such certificates does not validate the private keys. It is used in low-risk internal networks or the software development phase. So, you cannot revoke the CA-issues and the self-signed certificates.

## Certificate Creation

You can create a self-signed key and certificate pair with the **OpenSSL** utility, a command-line tool for creating and managing OpenSSL certificates, keys, and other files.

### Prerequisites

-   Install an OpenSSL utility.

### Creating a Certificate

1. Navigate to your workspace folder. For example, `cd /hab/a2_deploy_workspace`.

2. Execute the `./scripts/credentials set ssl --rotate-all` command. This command creates a skeleton of certificates.

3. Copy the below bash script to a new file

```bash
# !/bin/bash

echo extendedKeyUsage = clientAuth, serverAuth > server_cert_ext.cnf
echo extendedKeyUsage = clientAuth, serverAuth > client_cert_ext.cnf
openssl genrsa -out ca_root.key 2048
openssl req -x509 -new -key ca_root.key -sha256 -out ca_root.pem -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefrootca'
openssl genrsa -out admin-pkcs12.key 2048
openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "admin-pkcs12.key" -topk8 -out "oser_admin_ssl_private.key" -nocrypt
openssl req -new -key oser_admin_ssl_private.key -out admin.csr -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefadmin'
openssl x509 -req -in admin.csr -CA ca_root.pem -CAkey ca_root.key -CAcreateserial -out oser_admin_ssl_public.pem -sha256 -extfile server_cert_ext.cnf
openssl genrsa -out ssl-pkcs12.key 2048
openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "ssl-pkcs12.key" -topk8 -out  oser_ssl_private.key -nocrypt
openssl req -new -key oser_ssl_private.key -out ssl.csr -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefnode'
openssl x509 -req -in ssl.csr -CA ca_root.pem -CAkey ca_root.key -CAcreateserial -out oser_ssl_public.pem -sha256 -extfile client_cert_ext.cnf

cp ca_root.pem /hab/a2_deploy_workspace/certs/ca_root.pem
cp oser_admin_ssl_public.pem /hab/a2_deploy_workspace/certs/oser_admin_ssl_public.pem
cp oser_admin_ssl_private.key /hab/a2_deploy_workspace/certs/oser_admin_ssl_private.key
cp oser_ssl_public.pem /hab/a2_deploy_workspace/certs/oser_ssl_public.pem
cp oser_ssl_private.key /hab/a2_deploy_workspace/certs/oser_ssl_private.key
cp oser_admin_ssl_private.key /hab/a2_deploy_workspace/certs/kibana_ssl_private.key
cp oser_admin_ssl_public.pem /hab/a2_deploy_workspace/certs/kibana_ssl_public.pem
cp oser_ssl_private.key /hab/a2_deploy_workspace/certs/pg_ssl_private.key
cp oser_ssl_public.pem /hab/a2_deploy_workspace/certs/pg_ssl_public.pem
```

4. Navigate to your bastion host and execute the new file containing the copied bash script. The script generates the certificates at `/hab/a2_deploy_workspace/certs` directory. For example, `bash cert.sh`, where _cert.sh_ is the name of the newly created bash script file.

5. Again, navigate to your workspace folder. For example, `cd /hab/a2_deploy_workspace`.

6. To apply the generated certificates, execute the following command:

-   `./scripts/credentials set ssl --oser-ssl`
-   `./scripts/credentials set ssl --pg-ssl`

A confirmation message appears once the certificates are applied successfully, as shown below:

{{< figure src="/images/automate/ha_self_sign_certificate.png" alt="Certification Creation using openssl utility">}}

7. Navigate to the Chef Automate and Chef Server instances and check the Chef Service health status. If the service is down or critical, wait for three to four minutes for the instances to be up.
