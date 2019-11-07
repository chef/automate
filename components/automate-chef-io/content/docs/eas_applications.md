+++
title = "EAS Applications"
description = "EAS Applications"
date = 2019-10-18T18:54:09+00:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "eas_applications"
    weight = 10
+++

# Overview

The Chef Automate EAS Applications feature introduces several concepts in addition to those already present in habitat in order to organize your applications data in an intuitive way.

The term application, as used here, refers to the full set of habitat services that work together to provide some business value. In a typical enterprise software lifecycle, an application is deployed multiple times throughout the lifecycle to allow for multiple stages of verification and phased rollout of new changes to production. In Chef Automate, these stages are referred to as environments, and each instance of an application deployed into an environment is called a deployment.

In order for Chef Automate to organize your applications data, you must configure the Habitat supervisor with application and environment tagging data (see below for instructions). If your services/applications follow the standard enterprise application layout and lifecycle as described above, you should use the application and environment names you use normally for these values. For services that do not follow this layout, you should select environment and application names that will enable you to find and filter the applications data relevant to your typical job tasks using environment and application criteria.

![EAS Concepts](/images/docs/eas-concepts.png)

## Health Checks
To maximize the utility of the Chef Automate EAS Applications feature, it is recommended to implement meaningful health check hooks for your services. When a health check hook returns a status other than “OK”, Chef Automate will display the output of the hook along with the status, so it is recommended to include useful debugging information in the output of the health checks when returning a “critical”, “warning”, or “unknown” status result. Further information about writing habitat health check hooks can be found in the (https://www.habitat.sh/docs/reference/#related-article-runtime-settings "habitat documentation").

## Habitat Named Service Groups
To minimize Habitat network gossip overhead, it is currently recommended that each deployment (as defined above) use a separate Habitat network. When using this architecture, the Habitat named service groups feature is only useful in some edge case situations, such as when running multiple discrete clusters of the same software within a single application. Therefore, in most cases, it is recommended to not use this feature and leave the service group name at the default setting (default).

## Service Groups in Chef Automate
The Chef Automate interface will group any services with the same package name, environment, service group name, and application together in the main table view. This allows you to quickly understand the status of your habitat services even when scaling out to large numbers of services, with detailed reporting of each individual service shown in a card on the right-hand sidebar. In the table view, the status reported for a service group will reflect the “most critical” status reported by any individual instance of that service (e.g. if one instance is in “critical” status, the entire group is in “critical” status). By default, the table rows are sorted by percentage of OK services from the smallest to the largest. The sidebar cards are sorted by criticality, from the “most critical” status to the “least critical” status (“critical,” “warning,” “unknown”,  and “OK”). The most critical issues are shown at the top of the list, allowing you to quickly determine the next steps for remediation.

## Requirements and Capacity Planning
Based on the latest testing, the EAS Applications system can scale up to 15,000 services on a system with 4(v)CPUs, 16GB of RAM, and SSD disks.

You must use habitat 0.83 or greater.

At a larger scale, the subsystems responsible for processing applications data will consume significant CPU resources. If the Compliance and Infrastructure visibility features of Automate will also be used on this Chef Automate instance, the host system will need additional compute and memory capacity.

## Configuring Chef Habitat to Send Events to Chef Automate
In the Chef Automate UI, create an API token as described in the Chef Automate documentation. Save this token somewhere safe.

On the systems that runs Chef Habitat, you will need to launch the habitat supervisor with the following options:

```
HAB_FEAT_EVENT_STREAM=1 hab sup run \
--event-stream-application=MY_APP \
--event-stream-environment=MY_ENV \
--event-stream-site=MY_SITE \
--event-stream-url=AUTOMATE_HOSTNAME:4222 \
--event-stream-token=API_TOKEN
```

Replace MY_APP, MY_ENV, MY_SITE, AUTOMATE_HOSTNAME, and API_TOKEN with the appropriate values. MY_APP, MY_ENV, MY_SITE are user-defined and can be set to any string. MY_APP, MY_ENV are used to group services in the Chef Automate interface and are intended to be used to describe your applications and application lifecycle, as explained in the concepts section of this document. MY_SITE is intended to describe the physical (i.e., datacenter) or cloud-specific (i.e., AWS region) location where your services are deployed. These values are search enabled, allowing for filtering of services. Please see (https://www.habitat.sh/docs/habitat-cli/#hab-sup-run "habitat documentation") for more habitat options.

## Lifecycle management
A future release will enable you to manage the lifecycle of disconnected services via the UI. Currently the functionality is available by using the API directly. The API is used to manage the activity of periodic jobs to assess disconnection status and removal.

Disconnected services are defined as services known to the EAS Applications backend storage that have not received a health check message for a user-defined period of time.

### Periodic disconnection checks

Automate assesses the status of the habitat services based on the receipt of a health check message. If a health check message has not been received for a configurable amount of time the service is marked as disconnected. The default amount of time is 5 minutes and this check cannot be disabled. Habitat send the health check messages every 30 seconds by default, this is also configurable using the `--health-check-interval` option with habitat supervisor. Please see (https://www.habitat.sh/docs/habitat-cli/#hab-sup-run "habitat documentation") for more information

To configure the threshold for marking a disconnected service, you can do this via the UI or the API directly. To access the configuration settings in the UI, go to the settings tab, then Data Lifecycle on the sidebar. Under service groups you will find settings for Marking and Removing disconnected services.

Below is an example of using the API directly:

```bash
curl -sSX POST "https://automate-url/apis/v0/retention/service_groups/disconnected_services/config" -d
'{
  "threshold": "15m"
}'
-H "api-token: $TOKEN"
```

### Periodic removal

In addition to the disconnection check, services that have been disconnected for a period of time can be removed. This helps to keep the dashboard populated with the most relevent information and saves on storage space over time. By default this removal threshold is 7 days, but can be disabled if you wish to keep a record of services.

To configure the threshold for removing a disconnected service, you can do this via the UI or the API directly. The threshold is the time since it was marked as disconnected. You can also disable removal by setting running to false. To access the configuration settings in the UI, go to the settings tab, then Data Lifecycle on the sidebar. Under service groups you will find settings for Marking and Removing disconnected services.

Below is an example of using the API directly:

```bash
curl -sSX POST "https://automate-url/apis/v0/retention/service_groups/delete_disconnected_services/config" -d
'{
  "threshold": "1d",
  "running":true
}'
-H "api-token: $TOKEN"
```

## Configure encrypted communication

### Disable TLS For the Event Stream Protocol
If you do not wish to configure encrypted communication between habitat and automate you can disable TLS.

On the host where Chef Automate is deployed, create a file with the following content:
```
[event_gateway]
  [event_gateway.v1]
    [event_gateway.v1.sys]
      [event_gateway.v1.sys.service]
        disable_frontend_tls = true
```

Save the file with any name, then run chef-automate config patch FILENAME to apply the configuration change. You should see output similar to the following:

Updating deployment configuration

Applying deployment configuration
  Started event-gateway

### Configure automate for encrypted communication

By default frontend TLS is enabled. If it was previously disabled it can be re-enabled. On the host where Chef Automate is deployed, create a file with the following content:
```
[event_gateway]
  [event_gateway.v1]
    [event_gateway.v1.sys]
      [event_gateway.v1.sys.service]
        disable_frontend_tls = false
```

Save the file with any name, then run chef-automate config patch FILENAME to apply the configuration change. You should see output similar to the following:

Updating deployment configuration

Applying deployment configuration
  Started event-gateway

Once you have done that, retrieve the automate frontend TLS key to use for configuring your habitat supervisor.

You can cat the cert file that is located at `/hab/svc/automate-load-balancer/data/<servername>.crt` to retrieve the current frontend tls certificates.

You can also change the automate frontend key by following the configuration instructions referenced
(https://automate.chef.io/docs/configuration/#load-balancer-certificate-and-private-key "here").

### Configure habitat supervisor for encrypted communication

There are three ways to share the automate certificate with the habitat supervisor to use when connecting to automate.

1) Pass the certificate file as a parameter to the --event-stream-server-certificate flag.

```
HAB_FEAT_EVENT_STREAM=1 hab sup run \
--event-stream-application=MY_APP \
--event-stream-environment=MY_ENV \
--event-stream-site=MY_SITE \
--event-stream-url=AUTOMATE_HOSTNAME:4222 \
--event-stream-token=API_TOKEN
--event-stream-server-certificate=/path/to/automate.cert
```

2) Copy the automate certificate into the hab cache ssl directory. Habitat automatically searches that directory and uses the certificate from that when it is trying to start up.

Your keys will be stored in ```/hab/cache/ssl``` on Linux as the root user and ```~/.hab/cache/ssl``` on Linux systems if using the non-root user. Note that this is user dependent, so if you are not running as root it will be in your local user's home directory. On windows it is in ```C:\hab\cache\ssl```

3) Add the certificate to your systems platform-specific certificate store:  SChannel on Windows, Secure Transport on OSX, and OpenSSL on all other platforms. Please follow your operating system specific instructions.
