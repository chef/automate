+++
title = "AWS Certificate Manager"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "AWS Certificate Manager"
    parent = "automate/install"
    identifier = "automate/install/ha_aws_cert_mngr.md AWS Certificate Manager"
    weight = 330
+++

This page explains how to use the AWS Certificate Manager (ACM) console to generate a public ACM certificate, which is required to create a Load Balancer (LB).

You can either generate a new public certificate or copy an existing Amazon Resource Name (ARN) value of the certificate of your selected region in the AWS console.

{{< figure src="/images/automate/ha_aws_cert_mngr_arn.png" alt="AWS ACM ARN Value">}}

## Requesting a Public Certificate using AWS console

Follow these steps if you want to request an ACM public certificate:

1. Sign in to the AWS Management Console.

1. Open the [ACM console](https://console.aws.amazon.com/acm/home).

1. Select your region from the top-right corner of the console.

1. Select Request a certificate.

{{< figure src="/images/automate/ha_aws_cert_mngr_console.png" alt="AWS ACM">}}

1. Select a Request a public certificate option from the Certificate type page, and click Next.

1. In the Domain names section, type your domain name. You can use a fully qualified domain name (FQDN), such as www.example.com, or a bare domain name such as example.com.

1. In the Select validation method section, select either DNS validation or Email validation, depending on your needs. Before ACM issues a certificate, it validates that you own or control the domain names in your certificate request. You can use either email validation or DNS validation.

1. On the Tags page, you can optionally tag your certificate. Tags are key-value pairs that serve as metadata for identifying and organizing AWS resources.

1. Select Request. After the request is processed, the console returns you to your certificate list, where your new certificate is displayed with status, *Pending validation*.

You can also request a public certificate using the Command Line Interface (CLI). For detailed information on requesting these certificates, refer  [AWS documentation on requesting a public certificate](https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-request-public.html).

## Providing Certificate ARN Value in a2ha.rb Configuration file

Follow these steps if you want to utilize AWS certificates in your network infrastructure:

1. Login as a sudo user by using `su-` command.

1. Navigate to `hab` workspace directory.

1. Open the `a2ha.rb` file in any editor of your choice.

{{< figure src="/images/automate/ha_load_balancer.png" alt="AWS Certificate ARN Value">}}

1. Copy the existing or generated ARN value from AWS console to the `chef_server_lb_certificate_arn` and `automate_server_lb_certificate_arn` fields in the *a2ha.rb* file.

{{< figure src="/images/automate/ha_a2rb_lb_certificate.png" alt="AWS Certificate ARN Value">}}

1. Save and close the file.
