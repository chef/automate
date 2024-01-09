+++
title = "Chef Automate Overview"
weight = 10
draft = false
gh_repo = "automate"

[cascade]
  product = ["automate"]

[menu]
  [menu.automate]
    title = "Overview"
    parent = "automate"
    identifier = "automate/overview"
    weight = 10
+++

Chef Automate is an enterprise dashboard that provides a full suite of capabilities for maintaining continuous visibility into application, infrastructure, and security automation, and an auditable history of changes to your infrastructure.

Chef Automate integrates with:

- [Chef Infra Server](/server/) and [Chef Infra Client](/chef_client_overview/) for infrastructure automation
- [Chef InSpec](/inspec/) for compliance and security automation
- [Chef Habitat](/habitat/) for application automation

You can integrate Chef Automate with any or all of these open-source tools to consolidate their data for actionable insights.
Chef Automate comes with comprehensive 24x7 support services for the entire platform, including open-source components.

## Components

### Web UI

Chef Automate is built around a web user interface that provides visibility into all aspects of your infrastructure.
This includes Chef Infra Client nodes, Infra Client runs, Chef Infra Servers, policy groups, profiles, and Cookbooks.

Filterable dashboards collect configuration and compliance details for every datacenter, cloud provider, and environment under management.

You can dig into the causes of compliance failures or configuration errors across your entire fleet, or filter reports by the systems that matter most.
Use Chef Automate's trend graphs, rich query language and other sophisticated interfaces to quickly sift through the noise and prioritize solutions.

Get a real-time view of all changes happening in your environment, whether user- or machine-initiated. See how configuration and compliance policy changes impact each other over time.

{{< figure src="/images/automate/event-feed.png" alt="Chef Automate Event Feed showing recent events in your Chef infrastructure." >}}

### Compliance monitoring

Chef Automate creates customizable [compliance reports]({{< relref "/automate/reports" >}}) that identify compliance issues, security risks, and outdated software.

You can write your own compliance rules in Chef InSpec, or get started by using built-in profiles -- which are predefined rule sets for a variety of security frameworks, such as Center for Internet Security (CIS) benchmarks or DISA Security Technical Implementation Guides (STIGs). Use these profiles to scan individual machines, VMs, and cloud environments for policy conformance without installing any agents.

Examine past failures and trends to identify areas for targeted process improvement. Use reports to satisfy auditors' questions about current and historical compliance.

### Application monitoring

Use the [Application dashboard]({{< relref "/automate/applications_dashboard" >}}) to monitor the status and health of applications that are deployed with [Chef Habitat](/habitat/) on infrastructure monitored by Automate.

### Role-based access control

[Role-based access control]({{< relref "/automate/roles" >}}) allows you to use pre-defined or custom user roles to allow access to data and functions based on those role types.
For example, you may want to give your developers administrative access to update your infrastructure, while your audit team can only view the results of a compliance audit.

Use SAML or LDAP to authenticate your users and provide access to your Chef Automate deployment.

### API

Use the [Chef Automate API]({{< relref "/automate/api" >}}) to view and update your Automate deployment -- including compliance monitoring, node management, events, secrets, and users.

### High availability

Deploy Chef Automate in a [high availability configuration]({{< relref "/automate/ha" >}}) that limits failures and downtime, and allows you to scale your Automate deployment to match your infrastructure.

### Notifications

[Webhooks]({{< relref "/automate/notifications" >}}) notify you about Chef Infra Client run failures and Chef InSpec compliance scan failures.
You can create a webhook with Chef Automate's Slack and ServiceNow integrations, or create a custom webhook for another service.

[Data Feeds]({{< relref "/automate/datafeed">}}) send Chef Infra Client run data and compliance scan data to a 3rd-party service.
Chef Automate includes data integrations for ServiceNow, Splunk, and ELK/Kibana. You can also create a custom data feed.

## Additional Resources

### Learning

- [Learn Chef: Secure Your Infrastructure with Chef Automate](https://learn.chef.io/courses/course-v1:chef+Automate101+Perpetual/about)
- [Chef Automate webinars](https://www.chef.io/webinars?products=chef-automate&page=1)
- [Resource Library](https://www.chef.io/resources?products=chef-automate&page=1)

### Community

- [Chef Automate on Discourse](https://discourse.chef.io/c/automate/7)
- [Chef Automate in the Chef Blog](https://www.chef.io/blog/category/chef-automate)
- [Chef Automate Community Resources](https://community.chef.io/tools/chef-automate)

### Support

- [Chef Support](https://www.chef.io/support)
- [Chef Help Center](https://supportlink.chef.io/s/topic/0TO4Q0000009cLrWAI/chef-automate)

### GitHub Repositories

- [chef/automate repository](https://github.com/chef/automate/)
