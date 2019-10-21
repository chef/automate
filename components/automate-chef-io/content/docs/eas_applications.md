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


## Health Checks
To maximize the utility of the Chef Automate EAS Applications feature, it is recommended to implement meaningful health check hooks for your services. When a health check hook returns a status other than “OK”, Chef Automate will display the output of the hook along with the status, so it is recommended to include useful debugging information in the output of the health checks when returning a “critical”, “warning”, or “unknown” status result. Further information about writing habitat health check hooks can be found in the habitat documentation.

## Habitat Named Service Groups
To minimize Habitat network gossip overhead, it is currently recommended that each deployment (as defined above) use a separate Habitat network. When using this architecture, the Habitat named service groups feature is only useful in some edge case situations, such as when running multiple discrete clusters of the same software within a single application. Therefore, in most cases, it is recommended to not use this feature and leave the service group name at the default setting (default).

## Service Groups in Chef Automate
The Chef Automate interface will group any services with the same package name, environment, service group name, and application together in the main table view. This allows you to quickly understand the status of your habitat services even when scaling out to large numbers of services, with detailed reporting of each individual service shown in a card on the right-hand sidebar. In the table view, the status reported for a service group will reflect the “most critical” status reported by any individual instance of that service (e.g. if one instance is in “critical” status, the entire group is in “critical” status). By default, the table rows are sorted by percentage of OK services from the smallest to the largest. The sidebar cards are sorted by criticality, from the “most critical” status to the “least critical” status (“critical,” “warning,” “unknown”,  and “OK”). The most critical issues are shown at the top of the list, allowing you to quickly determine the next steps for remediation.


## Configuring Chef Habitat to Send Events to Chef Automate
In the Chef Automate UI, create an API token as described in the Chef Automate documentation. Save this token somewhere safe.

On the systems that runs Chef Habitat, you will need to launch the habitat supervisor with the following options:

HAB_FEAT_EVENT_STREAM=1 hab sup run \
--event-stream-application=MY_APP \
--event-stream-environment=MY_ENV \
--event-stream-site=MY_SITE \
--event-stream-url=AUTOMATE_HOSTNAME:4222 \
--event-stream-token=API_TOKEN

Replace MY_APP, MY_ENV, MY_SITE, AUTOMATE_HOSTNAME, and API_TOKEN with the appropriate values. MY_APP, MY_ENV, MY_SITE are user-defined and can be set to any string. MY_APP, MY_ENV are used to group services in the Chef Automate interface and are intended to be used to describe your applications and application lifecycle, as explained in the concepts section of this document. MY_SITE is intended to describe the physical (i.e., datacenter) or cloud-specific (i.e., AWS region) location where your services are deployed. These values are search enabled, allowing for filtering of services.

## Lifecycle management
### Listing and Removing Disconnected Services via CLI
A future release will enable you to manage the lifecycle of disconnected services via the UI. Currently the functionality is available only via the command line.

Disconnected services are defined as services known to the EAS Applications backend storage that have not received a health check message for a user-defined period of time. The command line tools allow you to list these services and remove them if desired.
Listing Disconnected Services
Usage:
chef-automate applications list-disconnected-services [flags]
Flags:
  -f, --format string           Format to display data. [ json | pretty ] (default "pretty")
  -h, --help                    help for list-disconnected-services
  -m, --threshold-minutes int   Number of minutes since last event received (default 10)
Examples:
Show all services that are considered disconnected with a threshold of 10 minutes (the default):

chef-automate applications list-disconnected-services

Return the results as JSON, with a threshold of 5 minutes:

chef-automate applications list-disconnected-services \
  --format json \
  --threshold-minutes 5

### Deleting Disconnected Services
Usage:
chef-automate applications delete-disconnected-services [flags]
Flags:
  -f, --format string           Format to display data. [ json | pretty ] (default "pretty")
  -h, --help                    help for delete-disconnected-services
  -m, --threshold-minutes int   Number of minutes since last event received (default 10)
  -y, --yes                     Delete services without a confirmation prompt.
Examples:
Delete services that are considered disconnected with a threshold of 10 minutes (the default), with an interactive confirmation prompt:

chef-automate applications delete-disconnected-services -m 5

Delete services that are considered disconnected with a threshold of 5 minutes with no confirmation prompt:

chef-automate applications delete-disconnected-services --threshold-minutes 5 --yes
