+++
title = "Setting up the Applications Dashboard"
description = "Configuring Chef Automate and Chef Habitat to work together"
date = 2019-10-18T18:54:09+00:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "applications"
    weight = 20
+++
<!-- ## Health Checks

To maximize the utility of the Chef Automate EAS Applications feature, it is recommended to implement meaningful health check hooks for your services.
When a health check hook returns a status other than "OK", Chef Automate will display the output of the hook along with the status, so it is recommended to include useful debugging information in the output of the health checks when returning a "critical", "warning", or "unknown" status result.
Further information about writing Chef Habitat health check hooks can be found in the (https://www.Chef Habitat.sh/docs/reference/#related-article-runtime-settings "Chef Habitat documentation"). -->

This Enterprise Application Stack (EAS) integration gives you immediate insight into the status of your Chef Habitat services, even when scaling out to large numbers of services.

The Chef Automate Applications Dashboard gives you operational visibility into your Chef Habitat systems, so you can monitor and respond quickly to changes in your Chef Habitat environments.
The Applications Dashboard _Service Groups_ table is an overview of your Chef Habitat network, grouped together by package, environment, service group, and application. The right-hand sidebar contains detailed status report cards for each individual service.

## Requirements and Capacity Planning

Chef Enterprise Application Stack with Chef Automate + Chef Habitat can scale up to 15,000 services on a system using:

* 4 CPU
* 16GB RAM
* 50 GB disk space
* Chef Automate, current version
* Chef Habitat, minimum version 0.89.47

Chef Automate's Compliance and Infrastructure visibility features require additional computing and memory capacity.
Larger scale systems (15,000+ services) consume significant CPU resources for processing applications data.

## Chef Habitat Service Group Best Practices

A service group contains all of the services for a single package. The services in a group share a single relationship structure (topology) and are connected within a Chef Habitat supervisor network.

For best results:

* Use a separate Chef Habitat network for each application deployment environment ("Development", "Test", "Acceptance", "Production")
* Use the default Chef Habitat service group names

For services that do not follow this layout, you should select environment and application names that help you find and filter the application data and are relevant to your typical job tasks.

For more information on setting up Chef Habitat, see the Chef Habitat [Service Groups](https://www.habitat.sh/docs/using-habitat/#service-groups) documentation.

## Setting up the Applications Dashboard

To get started, you need:

1. Running installations of Chef Automate and Chef Habitat Builder SasS or Builder on-prem
1. A [Chef Automate API token]({{< ref "api-tokens#creating-a-standard-api-token" >}}) specifically for this integration

## Sending Event Data to Chef Automate

If you have not already done so, [create an API token]({{< ref "api-tokens#creating-a-standard-api-token" >}}) in Chef Automate. Save it somewhere safe and accessible to you for use in this step.

### Customize the Event Stream Command

Copy the following event stream command into your editor and replace `MY_APP`, `MY_ENV`, `MY_SITE`, `AUTOMATE_HOSTNAME`, and `API_TOKEN` with the appropriate values.

```shell
HAB_FEAT_EVENT_STREAM=1
hab sup run \
  --event-stream-application="MY_APP" \
  --event-stream-environment="MY_ENV" \
  --event-stream-site="MY_SITE" \
  --event-stream-url="AUTOMATE_HOSTNAME:4222" \
  --event-stream-token="API_TOKEN"
```

* `HAB_FEAT_EVENT_STREAM=1` enables the event stream
* [hab sup run](https://www.habitat.sh/docs/habitat-cli/#hab-sup-run) is the hab cli commant to start the Habitat supervisor.
* `MY_APP` is the name of your application. Chef Automate groups services by application name in the Applications Dashboard
* `MY_ENV` is the application environment for this supervisor. Chef Automate groups services by environment in the Applications Dashboard
* `MY_SITE` describes the physical (for example, datacenter) or cloud-specific (for example, the AWS region) location where your services are deployed. The site field is a value filtering for services in the Applications Dashboard.
* `AUTOMATE_HOSTNAME:4222` is the Chef Automate URL with port 4222 specified.
* `API_TOKEN` is the token you created in Chef Automate.

### Run the Event Stream Command

Paste your customized event stream command into the Chef Habitat command line.

For example:

```shell
HAB_FEAT_EVENT_STREAM=1
hab sup run \
  --event-stream-application="AmazingEnterpriseApp" \
  --event-stream-environment="acceptance" \
  --event-stream-site="us-west-2" \
  --event-stream-url="automate.company.com:4222" \
  --event-stream-token="1234567890abcdefgh"
```

## Authorize Communication from Chef Habitat to Chef Automate

The Transport Layer Security (TLS) protocol provides privacy and data integrity during communication between Chef Automate and Chef Habitat. Provide Chef Habitat with Chef Automate's TLS certificate to authorize communication between the two.

### Retrieve Chef Automate's TLS key

Chef Automate's TLS certificate file is located at `/hab/svc/automate-load-balancer/data/<servername>.crt`.

From Chef Automate:

1. On the command line, run `chef-automate config show`
1. Copy the `[[global.1.frontend_tls]]` certificate contents from the certificate list
1. Make sure to copy it entirely, including `-----BEGIN CERTIFICATE...END CERTIFICATE-----`
1. Save the file as `automate.cert` somewhere safe and accessible to you

### Share the TLS Key with Chef Habitat

Share the automate certificate with the Chef Habitat supervisor.

Use **one** of these three options:

#### Copy the Chef Automate Certificate into the `/hab/cache/ssl` Directory

Chef Habitat automatically searches that directory and uses the certificate at start up.

* On Linux systems, copy the cert to`/hab/cache/ssl` if you are the root user and `~/.hab/cache/ssl` if you are a non-root user. If you are a non-root user, the full file path is `/Users/username/.hab/cache/ssl/automate.cert`, located in your local user's home directory.
* On Windows systems, store your certs in `C:\hab\cache\ssl`

#### Add the Tls Certificate to Your Event Stream Command

Pass the certificate file as a parameter by adding it to your event stream command:

```shell
--event-stream-server-certificate=/path/to/automate.cert
```

For example:

```shell
HAB_FEAT_EVENT_STREAM=1 hab sup run \
--event-stream-application=MY_APP \
--event-stream-environment=MY_ENV \
--event-stream-site=MY_SITE \
--event-stream-url=AUTOMATE_HOSTNAME:4222 \
--event-stream-token=API_TOKEN
--event-stream-server-certificate=/path/to/automate.cert
```

#### Add the TlS Certificate to Your Certificate Store

Add the certificate to your systems platform-specific certificate store.
These are: `SChannel` on Windows, `Secure Transport` on OSX, and `OpenSSL` on all other platforms. Please follow the instructions for your specific operating system.

## Troubleshooting

### Create a New TLS Key Pair

You can change Chef Automate's automate front-end TLS key pair by following the [load balancing configuration](https://automate.chef.io/docs/configuration/#load-balancer-certificate-and-private-key) documentation.

### Re-Enable TLS on Chef Automate

In some cases, front-end TLS communication may be previously disabled.
To re-enable front-end TLS communication, which allows Chef Automate to receive encrypted event data from Chef Habitat, on the Chef Automate host:

1. Create a file with the following content:

```TOML
[event_gateway]
  [event_gateway.v1]
    [event_gateway.v1.sys]
      [event_gateway.v1.sys.service]
        disable_frontend_tls = false
```

1. Save the file in the `.toml` file format. Use any name.
1. Apply the configuration change with `chef-automate config patch FILENAME.TOML`.
You should see output similar to:

```output
Updating deployment configuration

Applying deployment configuration
  Started event-gateway
```

#### Disable TLS Encryption on Chef Automate

To disable front-end TLS encryption on Chef Automate and allow un-encrypted event data from Chef Habitat, on the Chef Automate host:

1. Create a file with the following content:

```TOML
[event_gateway]
  [event_gateway.v1]
    [event_gateway.v1.sys]
      [event_gateway.v1.sys.service]
        disable_frontend_tls = true
```

1. Save the file in the `.toml` file format. Use any name.
1. Apply the configuration change with `chef-automate config patch FILENAME.TOML`.
You should see output similar to:

```output
Updating deployment configuration

Applying deployment configuration
  Started event-gateway
```

## Related Resources

For more information, see the [Chef Habitat documentation](https://www.habitat.sh/docs).
In particular, see the entries on:

* [Chef Habitat Best Practices](https://www.habitat.sh/docs/best-practices/)
* [Service Groups](https://www.habitat.sh/docs/using-habitat/#service-groups)
* [Services](https://www.habitat.sh/docs/glossary/#glossary-services)
* [Supervisor](https://www.habitat.sh/docs/glossary/#glossary-supervisor)
* [Topology](https://www.habitat.sh/docs/glossary/#topology)
