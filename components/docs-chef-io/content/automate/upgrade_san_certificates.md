+++
title = "Upgrade to SAN Certificates"
date = 2023-05-29T12:02:46-08:00
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Upgrade to SAN Certificates"
    identifier = "automate/upgrade/upgrade_san_certificates.md"
    parent = "automate/upgrade"
    weight = 30
+++

From version 4.7.x onwards, Chef Automate requires the public certificates as SAN certificates.

Upgrade to SAN certificates involves
- Upgrade internal Public Certificates
- Upgrade the public certificates of external Postgres/Open Search servers, if using

## Upgrade Internal Public Certificates
1. Get the SAN Validator script
   ```sh
   curl https://raw.githubusercontent.com/chef/automate/main/dev/san_validator.sh -o san_validator.sh
   ```
2. Run the SAN validator script
    ```sh
    bash san_validator.sh
    ```
3. If your setup has non-SAN certificates, the above script returns them as a response. If the script does not return any value, which means your setup has valid internal certificates.
4. If you have non-SAN certificates, run the command
    ```sh
    chef-automate internal-ca regenerate root
    ```

## Upgrade PG/OS Public Certificate
1. Get the existing PG/OS public certificate.
2. [Check]({{< relref "#how-to-check-the-certificate-is-san-based-or-non-san-based" >}}) whether certificate is SAN certificate or not.
3. If the certificate is a non-SAN certificate, regenerate the new set of certificates and apply to the external PG/OS server
4. Update the corresponding Root-CA configuration in Automate configuration   


## How to check the certificate is SAN based or non-SAN based
1. Get the SAN Validator script
    ```sh
    curl https://raw.githubusercontent.com/chef/automate/main/dev/san_validator.sh -o san_validator.sh
    ```
2. Get the certificate and store in a file, say `my_cert.crt`
3. Run the script by providing the certificate as an argument to the script. 
    ```sh
    bash san_validator.sh <path/my_cert.crt>
    ```
4. 1. If the certificate is a SAN certificate, the above script gives the response as `Pass: your certificate is a valid SAN certificate`
   2. If the certificate is a non-SAN certificate, the above script gives the response as `Fail: your certificate is not a valid SAN certificate`