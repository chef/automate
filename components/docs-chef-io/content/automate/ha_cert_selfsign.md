+++
title = "Self Sign Certificates"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Self Sign Certificates"
    parent = "automate/install"
    identifier = "automate/install/ha_cert_selfsign.md Self Sign Certificates"
    weight = 335
+++

## What are Certificates?

A security certificate is a small data file used as an Internet security technique through which the identity, authenticity and reliability of a website or Web application is established.

Certificates should be rotated periodically, to ensure optimal security.

## What are Self Signed Certificates?

A self signed certificate is a digital certificate that is not signed by a publicly trusted certificate authority (CA). They are created, issued, and signed by the company or developer who is responsible for the website or software being signed. The private key used in such certificate is not validated by a third party and is generally used in low-risk internal networks or in the software development phase. In addition, unlike CA-issued certificates, self-signed certificates cannot be revoked.

## Certificate Creation

You can create a self-signed key and certificate pair with OpenSSL utility, a command line tool for creating and managing OpenSSL certificates, keys, and other files.

### Prerequisites

Install an *openssl* utility.

### Creating a Certificate

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

openssl x509 -req -in admin.csr -CA ca_root.pem -CAkey ca_root.key -CAcreateserial -out es_admin_ssl_public.pem -sha256 -extfile server_cert_ext.cnf

openssl genrsa -out ssl-pkcs12.key 2048

openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "ssl-pkcs12.key" -topk8 -out  es_ssl_private.key -nocrypt

openssl req -new -key es_ssl_private.key -out ssl.csr -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefnode'

openssl x509 -req -in ssl.csr -CA ca_root.pem -CAkey ca_root.key -CAcreateserial -out es_ssl_public.pem -sha256 -extfile client_cert_ext.cnf

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

{{< figure src="/images/automate/ha_self_sign_certificate.png" alt="Certification Creation using openssl utility">}}

1. Navigate to the Chef Automate and Chef Server instances and check the Chef Service health status. If the service is down or critical, then  wait for three to four minutes for the instances to be up.
