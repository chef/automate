+++
title = "License Chef Automate"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "License"
    parent = "automate/install"
    identifier = "automate/install/chef_automate_license.md License"
    weight = 80
+++

Before running Chef Automate, you must accept the Chef EULA.

Chef Automate offers two license tiers that have different entitlements:

* **Trial:** A trial license is for users or organizations interested in exploring the product before buying. Generate the license from https://www.chef.io/license-generation-free-trial
* **Commercial:** A commercial license is for customers who have purchased and are entitled to use it according to the license terms.

{{< warning >}}

The Chef Server deployed and running with Automate will also require a license. The Chef Automate license will cover the Chef Server license.

{{< /warning >}}

Chef Automate is built around a web user interface that provides visibility into all aspects of your infrastructure. The licensing types will affect your UI journey.

For more information on Chef licenses, see [Chef licensing documentation](https://docs.chef.io/licensing/).

Following are some of the scenarios that will occur when you use the Chef Automate web user interface:

## When the trial license is about to expire

If you are using a trial license, Chef Automate web UI will notify you when your license is about to expire.

{{< figure src="/images/automate/trial-license-expire-banner.png" alt="Chef Automate Trial License Expire Banner">}}

The banner in the above image says:

`Your Progress Chef Automate trial license is set to expire on DD/MM/YYYY! Contact the Account Team or email chef-account-team@progress.com for help or update with the new license key.`

To apply for a new Chef Automate license, follow the steps:

1. [Contact us](https://www.chef.io/contact-us) to get a license.
1. If you already have a license key, select the `update with the new license key` content from the banner. A popup window will open.
1. Paste the license key in the box and check the I agree to the Terms and Service box.
1. Select **Apply License**.

{{< figure src="/images/automate/trial-license-about-to-expire-popup.png" alt="Chef Automate Trial License about to Expire Popup">}}

## When the trial license is expired

Once a license expires, you cannot use Chef Automate features and capabilities. All trial licenses will cease to operate once the expiry date has elapsed. To continue using Chef Automate, you must upgrade to an Enterprise License.

To apply for the license, follow the steps:

1. [Contact us](https://www.chef.io/contact-us) to get a license.
1. If you already have a license key, paste it in the popup box and check the I agree to the Terms and Service box.
1. Select **Apply License**.

{{< figure src="/images/automate/trial-license-expire-popup.png" alt="Chef Automate Trial License Expire Popup">}}

## When the enterprise license is about to expire

In the new version of Chef Automate, capabilities, and features will be restricted according to the tenure and purpose of the existing license. The new version will help us better serve both community and commercial users.

If you are using an enterprise license, Chef Automate web UI will notify you when your license is about to expire.

{{< figure src="/images/automate/enterprise-license-about-to-expire-banner.png" alt="Chef Automate Enterprise License Expire Banner">}}

The banner in the above image says:

`Your Progress Chef Automate license is set to expire on DD/MM/YYYY! Contact the Account Team or email chef-account-team@progress.com for help or update with the new license key.`

To apply for a new Chef Automate license, follow the steps:

1. [Contact us](https://www.chef.io/contact-us) to get a license.
1. If you already have a license key, select the `update with the new license key` content from the banner. A popup window will open.
1. Paste the license key in the box and check the I agree to the Terms and Service box.
1. Select **Apply License**.

{{< figure src="/images/automate/enterprise-license-about-to-expire-popup.png" alt="Chef Automate Enterprise License about to Expire Popup">}}

## When the enterprise license is expired

On expiry of the license, you can continue to use the product for a grace period of 60 days. You can apply for the new license during the grace period.

{{< figure src="/images/automate/enterprise-license-expire-grace-period-banner.png" alt="Chef Automate Enterprise License Grace Period Banner">}}

The banner in the above image says:

`Your Progress Chef Automate license expired on DD/MM/YYYY! and you are currently on a limited extension period. Contact the Account Team or email chef-account-team@progress.com for help or update with the new license key.`

To apply again for the Chef Automate license, follow the steps:

1. [Contact us](https://www.chef.io/contact-us) to get a license.
1. If you already have a license key, select the `update with the new license key` content from the banner. A popup window will open.
1. Paste the license key in the box and check the I agree to the Terms and Service box.
1. Select **Apply License**.

{{< figure src="/images/automate/enterprise-license-expire-grace-period-popup.png" alt="Chef Automate Enterprise License Grace Period Popup">}}

Once an enterprise license expires, the web UI throws the following popup:

{{< figure src="/images/automate/enterprise-license-expire-popup.png" alt="Chef Automate Enterprise License Expire Popup">}}

You cannot use Chef Automate features and capabilities. To continue using Chef Automate, apply again for the Chef Automate license using the following steps:

1. [Contact us](https://www.chef.io/contact-us) to get a license.
1. If you already have a license key, paste it in the popup box and check the I agree to the Terms and Service box.
1. Select **Apply License**.

## Chef Server under Automate License

Chef Server, when deployed with Automat,e will abide by the Automate license.

The `chef-server-ctl` command will not work if the commercial/trial license has not been applied to Automate or has expired. In the case of `knife` or `Infra Client` execution, the Chef Server will not respond if the Automate license has not been applied or expired.
 