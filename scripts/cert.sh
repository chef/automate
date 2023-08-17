#!/bin/bash
 
#First, create a private key for the CA: 
openssl genrsa -out MyRootCA.key 2048
 
#Create the CA and enter the Organization details: 
openssl req -x509 -new -key MyRootCA.key -sha256 -out MyRootCA.pem -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefrootca'
 
#the rsa keys 
openssl genrsa -out admin.key 2048
 
#Create the CSR and enter the organization and server details for the node key 
openssl req -new -key admin.key -out admin.csr -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefadmin'
 
#Use the CSR to generate the signed node Certificate:
openssl x509 -extfile <(printf "subjectAltName=DNS:chefadmin") -req -in admin.csr -CA MyRootCA.pem -CAkey MyRootCA.key -CAcreateserial -out admin.pem -sha256 -days 2042
 
# root pem cert that signed the below cert/key pairs below
# Used for hab_sup_http_gateway_ca_cert 

touch terraform/cert.tf

echo "cert.tf is created and will be added with valuse"

cat <<EOF >> terraform/cert.tf

variable "hab_sup_http_gateway_ca_cert" {
  default = <<CERT
$(cat MyRootCA.pem)
CERT


  description = "Issuer of the TLS cert used for the HTTP gateway in PEM format."
}

variable "hab_sup_http_gateway_priv_key" {
  default = <<CERT
$(cat admin.key)
CERT


  description = "Issuer of the TLS cert used for the HTTP gateway in PEM format."
}

variable "hab_sup_http_gateway_pub_cert" {
  default = <<CERT
$(cat admin.pem)
CERT


  description = "Issuer of the TLS cert used for the HTTP gateway in PEM format."
}

EOF
 
rm MyRootCA.key MyRootCA.pem MyRootCA.srl admin.csr admin.key admin.pem
