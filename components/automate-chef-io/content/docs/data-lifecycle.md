+++
title = "Data Lifecycle"
aliases = [
    "/docs/node-lifecycle/"
]
description = "Chef Automate Data Lifecycle: Data Management and Data Retention"
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "settings"
    weight = 20
+++

## Overview

Data Lifecycle manages the retention of events, service groups, Chef Client runs, compliance reports and scans in Chef Automate. Chef Automate stores data from the ingest-service, event-feed-service, compliance-service and applications-service in ElasticSearch or PostgreSQL. Over time you may wish to remove that data from Chef Automate by using the data lifecycle settings.  

{{% warning %}}
Note: Chef Automate data retention processes changed in 20191129172405. The [upgrade documentation]({{< ref "install/#upgrades" >}}) covers configuring your system to install new Chef Automate versions. For guidance, contact your customer support agent. You can also use the [previous data retention documentation](https://github.com/chef/automate/blob/20191104205453/components/automate-chef-io/content/docs/configuration.md#data-retention) for help with configuring data retention on older Chef Automate installations.
{{% /warning %}}

## Data Lifecycle UI  

Navigate to _Settings_ > _Data Lifecycle_ and adjust any settings you would like to change. After making changes use the **Save Changes** button to apply your changes.

Only users with `dataLifecycle:*` IAM access are able to see the data lifecycle job statuses, configure jobs, or run jobs.

![](/images/docs/data-lifecycle.png)

### Event Feed

The event feed data lifecycle settings allow you to remove all event feed data and chef server actions after a set amount of days. The default is to remove event feed data after 7 days, and Chef Infra Server actions after 30 days.

### Service Groups

The service group data lifecycle settings are shown in the UI, but cannot be changed in the UI. Instead, you can use the API to adjust these settings. The default is to label health check reports as disconnected after 5 minutes, and remove disconnected services after 5 days.

### Client Runs

The Client Runs data lifecycle settings allow you to remove data after a set amount of days. They also allow you to label nodes as missing and automatically remove them after a set amount of days. The default is to remove Chef Client run data after 1 day, to label nodes as missing after 30 days, and to remove nodes labeled as missing after 30 days.

### Compliance

The Compliance data lifecycle settings allow you to remove compliance reports and compliance scans after a set amount of days. The default is to remove compliance reports after 60 days, and to remove compliance scans after 60 days.

## Data Lifecycle API

Chef Automate stores data from the `ingest-service`, `event-feed-service`,
`compliance-service` and `applications-service` in ElasticSearch or PostgreSQL.

The `data-lifecycle` API allows configuring and running lifecycle jobs by data type:

* `infra` Chef Infra Server actions and Chef Client converge data
* `compliance` Chef Inspec reports and Chef Compliance scans
* `event-feed` Event metadata that powers the operational visibility and query language


An [api token]({{< relref "api-tokens.md" >}}) with `dataLifecycle:*` IAM access is required to see the data lifecycle job statuses, configure jobs, or run jobs.


### Status

To see the combined status and configuration for all data lifecycle jobs you can use the global status endpoint
```bash
curl -s -H "api-token: $TOKEN" https://{{< example_fqdn "automate" >}}/api/v0/data-lifecycle/status
```

To see individual statuses by data type you can access the data type sub-status endpoints:

```bash
curl -s -H "api-token: $TOKEN" https://{{< example_fqdn "automate" >}}/api/v0/data-lifecycle/event-feed/status
```

Swap `event-feed` for `infra` or `compliance` to see their corresponding jobs.

The status in an aggregate of the job configuration, details about it's next scheduled run, and details about
any previous runs.

### Configuration

Configure the data lifecycle job settings by creating a JSON file with the desired configuration.

```json
{ "infra": {
    "job_settings": [
      { "name":"delete_nodes",
        "disabled": true,
        "recurrence": "FREQ=MINUTELY;DTSTART=20191106T180240Z;INTERVAL=15",
        "threshold": "365d"
      },
      { "name":"missing_nodes",
        "disabled": false,
        "recurrence": "FREQ=MINUTELY;DTSTART=20191106T180240Z;INTERVAL=15",
        "threshold": "1d"
      },
      { "name":"missing_nodes_for_deletion",
        "disabled": false,
        "recurrence": "FREQ=MINUTELY;DTSTART=20191106T180240Z;INTERVAL=15",
        "threshold": "30d"
      },
      { "name":"periodic_purge_timeseries",
        "disabled": false,
        "recurrence": "FREQ=DAILY;DTSTART=20191106T180240Z;INTERVAL=1",
        "purge_policies": {
          "elasticsearch": [
            {
              "policy_name": "actions",
              "older_than_days": 30,
              "disabled": false
            },
            {
              "policy_name": "converge-history",
              "older_than_days": 30,
              "disabled": false
            }
          ]
        }
      }
    ]
  },
  "compliance": {
    "job_settings": [
      {
        "name": "periodic_purge",
        "disabled": false,
        "recurrence": "FREQ=DAILY;DTSTART=20191106T180323Z;INTERVAL=1",
        "purge_policies": {
          "elasticsearch": [
            {
              "policy_name": "compliance-reports",
              "older_than_days": 100,
              "disabled": false
            },
            {
              "policy_name": "compliance-scans",
              "older_than_days": 100,
              "disabled": false
            }
          ]
        }
      }
    ]
  },
  "event_feed": {
    "job_settings": [
      {
        "name": "periodic_purge",
        "disabled": false,
        "recurrence": "FREQ=DAILY;DTSTART=20191106T180243Z;INTERVAL=2",
        "purge_policies": {
          "elasticsearch": [
            {
              "policy_name": "feed",
              "older_than_days": 90,
              "disabled": false
            }
          ]
        }
      }
    ]
  }
}
```

Configure the jobs by sending the JSON payload to the `configure` endpoint. Save the JSON file as `config.json`
in the current working directory.
```bash
curl -s -H "api-token: $TOKEN" -X PUT --data "@config.json" https://{{< example_fqdn "automate" >}}/api/v0/data-lifecycle/config
```

If you wish to configure a specific endpoint only, you can specify the `job_settings` for that
data type and configure it using data types sub-resource. For example, if you want to configure
compliance settings create a smaller JSON payload:

```json
{ "job_settings": [
    {
      "name": "periodic_purge",
      "disabled": false,
      "recurrence": "FREQ=DAILY;DTSTART=20191106T180323Z;INTERVAL=1",
      "purge_policies": {
        "elasticsearch": [
          {
            "policy_name": "compliance-reports",
            "older_than_days": 100,
            "disabled": false
          },
          {
            "policy_name": "compliance-scans",
            "older_than_days": 100,
            "disabled": false
          }
        ]
      }
    }
  ]
}
```

And update it using the `compliance` sub-resource
```bash
curl -s -H "api-token: $TOKEN" -X PUT --data "@config.json" https://{{< example_fqdn "automate" >}}/api/v0/data-lifecycle/compliance/config
```

#### Job Settings

All jobs have the following options:

* `recurrence` (string) A recurrence rule that determines how often, at what interval, and when to initially start a scheduled
  job. Any valid recurrence rule [as defined in section 4.3.10 of RFC 2445](https://www.ietf.org/rfc/rfc2445.txt) is valid in this field.
* `disabled` (bool) Whether or not this job should be enabled

Infra node lifecycle jobs have the following options:

* `threshold` (string) setting that allows the user to use `1w` style notation to denote how long before the infra job is trigged.

Purge jobs have the following options:

* `purge_polices` (map) configures how old the corresponding data must be in the configured storage backend before it is purged.
  * `elasticsearch` (array) an array of ElasticSearch purge policies
    * `disabled` (bool) Whether or not this job should be enabled
    * `policy_name` (string) The name of the purge policy you wish to update
    * `older_than_days` (int) The name of the purge policy you wish to update

##### Infra Job Settings

The `infra` data type has four data lifecycle jobs, three are for node lifecycle and one is purge job with two ElasticSearch purge policies.

```json
{ "job_settings": [
    { "name":"delete_nodes",
      "disabled": true,
      "recurrence": "FREQ=MINUTELY;DTSTART=20191106T180240Z;INTERVAL=15",
      "threshold": "365d"
    },
    { "name":"missing_nodes",
      "disabled": false,
      "recurrence": "FREQ=MINUTELY;DTSTART=20191106T180240Z;INTERVAL=15",
      "threshold": "1d"
    },
    { "name":"missing_nodes_for_deletion",
      "disabled": false,
      "recurrence": "FREQ=MINUTELY;DTSTART=20191106T180240Z;INTERVAL=15",
      "threshold": "30d"
    },
    { "name":"periodic_purge_timeseries",
      "disabled": false,
      "recurrence": "FREQ=DAILY;DTSTART=20191106T180240Z;INTERVAL=1",
      "purge_policies": {
        "elasticsearch": [
        {
          "policy_name": "actions",
          "older_than_days": 30,
          "disabled": false
        },
        {
          "policy_name": "converge-history",
          "older_than_days": 30,
          "disabled": false
        }
        ]
      }
    }
  ]
}
```

* `delete_nodes` how long a node can exist before it is deleted.
* `missing_nodes` how long between a node's last check-in before it
  is marked as missing.
* `missing_nodes_for_deletion` how long a node can be missing before it is deleted
* `periodic_purge_timeseries` how often to run the purge job
  * `actions` Chef Infra Server actions
  * `converge-history` Chef Infra Client converge data

##### Compliance Job Settings

The `compliance` data type has one compliance purge job with two ElasticSearch purge policies.

```json
{ "job_settings": [
    {
      "name": "periodic_purge",
      "disabled": false,
      "recurrence": "FREQ=DAILY;DTSTART=20191106T180323Z;INTERVAL=1",
      "purge_policies": {
        "elasticsearch": [
          {
            "policy_name": "compliance-reports",
            "older_than_days": 100,
            "disabled": false
          },
          {
            "policy_name": "compliance-scans",
            "older_than_days": 100,
            "disabled": false
          }
        ]
      }
    }
  ]
}
```

* `periodic_purge` how often to run the purge job
  * `compliance-reports` Chef Inspec reports
  * `compliance-scans` Chef Compliance scans

##### Event Feed Job Settings

The `event_feed` data type has one event feed purge job with one ElasticSearch purge policy.

```json
{ "job_settings": [
    { "name": "periodic_purge",
      "disabled": false,
      "recurrence": "FREQ=DAILY;DTSTART=20191106T180243Z;INTERVAL=2",
      "purge_policies": {
        "elasticsearch": [
        {
          "policy_name": "feed",
          "older_than_days": 90,
          "disabled": false
        }
        ]
      }
    }
  ]
}
```

* `periodic_purge` how often to run the purge job
  * `feed` Queryable event feed

### Run

As with `status` and `configure`, you can run data lifecycle jobs globally across all data or by using the data type sub-resource.

To run all data lifecycle jobs immediately run the following command:

```bash
curl -s -H "api-token: $TOKEN" -X POST https://{{< example_fqdn "automate" >}}/api/v0/data-lifecycle/run
```

To only run jobs for a specific data type you can make the request to the sub-resource:

```bash
curl -s -H "api-token: $TOKEN" -X POST https://{{< example_fqdn "automate" >}}/api/v0/data-lifecycle/infra/run
```

Swap `infra` for `event-feed` or `compliance` to run their corresponding jobs.
