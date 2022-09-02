+++
title = "AWS Certificate Manager"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "AWS Certificate Manager"
    parent = "automate/deploy_high_availability/reference"
    identifier = "automate/deploy_high_availability/reference/ha_aws_cert_mngr.md AWS Certificate Manager"
    weight = 230
+++

{{< warning >}}
{{% automate/4x-warn %}}
{{< /warning >}}

This page explains how to use the **AWS Certificate Manager (ACM)** console to generate a public ACM certificate for your domain.

You can either generate a new public certificate or copy an existing Amazon Resource Name (ARN) value of the certificate of your selected region in the AWS console.

{{< figure src="/images/automate/ha_aws_cert_mngr_arn.png" alt="AWS ACM ARN Value">}}

## Requesting a Public Certificate using AWS console

Follow these steps if you want to request an ACM public certificate:

1. Sign in to the **AWS Management Console**.

2. Open the [ACM console](https://console.aws.amazon.com/acm/home).

3. Select your **region** from the top-right corner of the console.

4. Select **Request a certificate**.

{{< figure src="/images/automate/ha_aws_cert_mngr_console.png" alt="AWS ACM">}}

5. Select the **Request a public certificate** option from the **Certificate type** page, and select **Next**.

6. In the **Domain names** section, enter your domain name. You can use a fully qualified domain name (FQDN), such as `www.example.com`, or a bare domain name such as `example.com`.

{{< note >}} Before ACM issues a certificate, it validates that you own or control the domain names in your certificate request. You can use either email validation or DNS validation. {{< /note >}}

7. In the **Select validation method** section, select either *DNS validation* or *Email validation*.

8. From the **Tags** page, tag your certificate. Tags are key-value pairs that serve as metadata for identifying and organizing AWS resources.

  {{< figure src="/images/automate/ha_cert.png" alt="Certificates">}}

9. Select **Request**. The console returns you to your certificate list after processing the request, where your new certificate displays with status, *Pending validation*.

You can also request a public certificate using the Command Line Interface (CLI). For detailed information on requesting these certificates, refer  [AWS documentation on requesting a public certificate](https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-public.html).

## Providing Certificate ARN Value in `config.toml` Configuration file

Follow these steps if you want to use the AWS certificates in your network infrastructure:

1. Log in as a Sudo user by using `su-` command.

2. Navigate to the `hab` workspace directory.

3. Open the `config.toml` file in any editor of your choice.

{{< figure src="/images/automate/ha_load_balancer.png" alt="AWS Certificate ARN Value">}}

4. Copy the existing or generated ARN value from AWS console to the `chef_server_lb_certificate_arn` and `automate_server_lb_certificate_arn` fields in the *config.toml* file.

{{< figure src="/images/automate/ha_a2rb_lb_certificate.png" alt="AWS Certificate ARN Value">}}

5. Save and close the file.
