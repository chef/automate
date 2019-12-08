+++
title = "OAuth"
description = "Configuring Automate as an OAuth Provider for Habitat Builder"
date = 2018-05-11T09:27:09+00:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "authentication"
    weight = 20
+++

## Alpha: Setting up Automate as an OAuth Provider for Habitat Builder

{{% warning %}}
Authentication via Automate for Habitat Builder is still in Alpha.
{{% /warning %}}

To configure Chef Automate as an OAuth Provider for Habitat Builder, create a TOML file
that contains the partial configuration below.
Run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

`bldr_client_id` and `bldr_client_secret` simply need to match what you configured for the corresponding
values in Habitat Builder (see below). However, we strongly recommend those values follow
[best practices](https://www.oauth.com/oauth2-servers/client-registration/client-id-secret/)
for `client_id` and `client_secret` in the Oauth2 standard.

```toml
[session.v1.sys.service]
bldr_signin_url = "<your Builder fqdn>" # for example, "http://builder.test/"

# This needs to match what you configured OAUTH_CLIENT_ID as when you configured Habitat Builder.
bldr_client_id = "<your Habitat Builder Oauth2 Client ID>"

# This needs to match what you configured OAUTH_CLIENT_SECRET as when you configured Habitat Builder.
bldr_client_secret = "<your Habitat Builder Oauth2 Client Secret>"
```

You'll need to add Automate's TLS certificate to Builder's list of accepted certificates
in addition to these configuration changes.
Locate Automate's default self-signed certificate by running `cat /hab/svc/automate-load-balancer/data/{{< example_fqdn "automate" >}}.cert`
You can copy this default certificate, and then add it to your Builder instance's list of accepted certs.

```text
-----BEGIN CERTIFICATE-----
MIIDfDCCAmSgAcasldkaf...
-----END CERTIFICATE-----
```

If you are using a certificate signed by a trusted certificate authority instead of the default certificate,
you can provide Builder with the root certificate authority for the signed certificate.

For more information, check out this further explanation on how to [configure Builder to authenticate via Chef Automate]((https://github.com/habitat-sh/on-prem-builder).
