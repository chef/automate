+++
title = "Update Non-SAN Certificates for 4.7.52 Version"
date = 2023-05-29T12:02:46-08:00
draft = false
gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Update Non-SAN Certificates for 4.7.52 Version"
    identifier = "automate/upgrade/upgrade_san_certificates.md Update Non-SAN Certificates for 4.7.52 Version"
    parent = "automate/upgrade"
    weight = 40
+++

## Overview

[RFC 2818](https://datatracker.ietf.org/doc/html/rfc2818#section-3.1) published in May 2000, deprecates using the Common Name (CN) field in HTTPS certificates for subject name verification. Instead, it recommends using the "Subject Alternative Name" extension (SAN) of the "DNS name" type.

Automate is moving the product builds from Golang version 1.15 to 1.19 to keep the language features up to date. The change in the Golang version will impact the custom certificates used for interaction with external systems. The common name field of X.509 certificates will no longer be considered the hostname when the Subject Alternative Name (SAN) is absent. Refer to [Go 1.15 Release Notes](https://go.dev/doc/go1.15#commonname) and our [blog](https://www.chef.io/blog/upgrading-golang-version-in-the-early-june-23-automate-release) for more information.

From version 4.7.52 onwards, Chef Automate requires public certificates as SAN certificates. Automate or Automate HA may stop working if you upgrade to this version with certificates that don't contain the SAN field.

Update non-SAN Certificates involves:

- Update internal Public Certificates
- Update the public certificates of external PostgreSQL/OpenSearch servers, if using

## Update Internal Public Certificates

1. Copy the [SAN validator script]({{< relref "#san-validator-script" >}}) and store it in a file, say san_validator.sh
1. Run the SAN validator script to validate whether all the internal public certificates are SAN-based.

    ```sh
    bash san_validator.sh
    ```

1. If your setup has non-SAN certificates, the above script returns the list of non-SAN certificates as a response, as shown below.
   ![invalid_san_certificates](/images/automate/invalid_san_certificates.png)
1. If the script does not return any values as invalid certificates, as below, your setup has valid internal certificates.
   ![valid_san_certificates](/images/automate/valid_san_certificates.png)
1. If you have non-SAN certificates, run the command

    ```sh
    chef-automate internal-ca regenerate root
    ```

## Update PG/OS Public Certificate

1. Get the existing PG/OS public certificate.
1. [Check]({{< relref "#how-to-check-whether-the-certificate-is-san-based-or-non-san-based" >}}) whether the certificate is a SAN certificate or not.
1. If the certificate is a non-SAN certificate, regenerate the new set of certificates and apply it to the external PG/OS server.
1. Update the corresponding Root-CA configuration in Automate configuration.

## How to check whether the certificate is SAN-based or non-SAN based

1. Copy the [SAN validator script]({{< relref "#san-validator-script" >}}) and store it in a file, say san_validator.sh
1. Get the certificate and store it in a file, say `my_cert.crt`
1. Run the script by providing the certificate as an argument to the script.

    ```sh
    bash san_validator.sh <path/my_cert.crt>
    ```

1. If the certificate is a SAN certificate, the above script gives the response as `Pass: your certificate is a valid SAN certificate`
1. If the certificate is a non-SAN certificate, the above script gives the response as `Fail: your certificate is not a valid SAN certificate`

## SAN Validator Script

   ```sh
   #!/bin/bash

   cert_path="$1"

   validate_cert() {
   input_path="$1"
   cert_info=$(openssl x509 -in "$input_path" -noout -text 2>/dev/null)
   if echo "$cert_info" | grep -q "Subject Alternative Name"; then
   return 0
   else
   return 1
   fi
   }

   is_root_ca() {
   input_path="$1"

       #Check the certificate is RootCA is not
       is_self_signed=$(openssl verify -CAfile "$cert" "$cert" 2>&1)

       #enable case insensitive match for reg-ex
       shopt -s nocasematch
       if [[ ! -z $(cat "$cert") ]] && [[ $is_self_signed == *error* ]]; then
               #disable case insensitive match for reg-ex
               shopt -u nocasematch
               return 1
       else
               #disable case insensitive match for reg-ex
               shopt -u nocasematch
               return 0
       fi

   }

   # If the certificate path provides validation only that certificate
   if [ "$cert_path" ]; then
   # Check for the certificate file exists
   if [ ! -f "$cert_path" ]; then
   echo "Error: Certificate file not found."
   exit 1
   fi

       if validate_cert "$cert_path"; then
           echo "Pass: your certificate is a valid SAN certificate"
       else
           echo "Fail: your certificate is not a valid SAN certificate"
       fi
       exit 1
   fi

   # Get all the internal certificates with extension(.crt), gets the RootCA and public certificates
   certs=$(find /hab/svc -type f -name "*.crt")
   invalid_certs=()

   for cert in $certs; do
   if ! is_root_ca "$cert" && ! validate_cert "$cert"; then
   invalid_certs+=("$cert")
   fi
   done

   # Print the list of invalid certs
   echo "Invalid certificates:"
   for cert in "${invalid_certs[@]}"; do
   echo "$cert"
   done
   ```
