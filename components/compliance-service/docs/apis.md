### Used on Automate's `/compliance/reports/overview` page, `Overview` tab, `Report Metadata` section
 * GRPC API => `Stats::StatsService, :read_summary, Stats::Query.new(...)`
 * HTTP API => `/compliance/reporting/stats/summary`
 * HTTP RESPONSE:
```json
{
  "controls_summary": null,
  "node_summary": null,
  "report_summary": {
    "stats": {
      "nodes": "3",
      "platforms": 2,
      "environments": 1,
      "profiles": 1,
      "nodes_cnt": 3,
      "controls": 92
    },
    "status": "failed",
    "duration": 0,
    "start_date": ""
  }
}
```

### Used on Automate's `/compliance/reports/overview` page, `Overview` tab, `Node Status` toggle, `Severity of Node Failures` and `Node Status` sections
 * GRPC API => `GRPC Stats::StatsService, :read_summary, Stats::Query.new(type: "nodes", ...)`
 * HTTP API => `/compliance/reporting/stats/summary` (`{"type":"nodes", ...}`)
 * HTTP RESPONSE:
```json
{
  "controls_summary": null,
  "node_summary": {
    "compliant": 0,
    "skipped": 0,
    "noncompliant": 3,
    "high_risk": 0,
    "medium_risk": 3,
    "low_risk": 0
  },
  "report_summary": null
}
```
 * **NOTE:** Same API is used with HTTP params (`{"type":"controls", ...}`) when togging to `Profile Status` on the `Overview` page. It feeds the `Severity of Control Failures` and `Control Status` sections

### Used on Automate's `/compliance/reports/overview`, `Overview` tab, `Node Status` toggle, `Top Platform Failures` and `Top Environment Failures` bubble graphs
 * GRPC API => `GRPC Stats::StatsService, :read_failures, Stats::Query.new()`
 * HTTP API => `/compliance/reporting/stats/failures` (`{"filters":[{"type":"types","values":["platform","environment"]}]}`)
 * HTTP RESPONSE:
```json
{
  "profiles": [],
  "platforms": [
    {
      "name": "redhat",
      "failures": 2,
      "id": "",
      "profile": ""
    },
    {
      "name": "ubuntu",
      "failures": 1,
      "id": "",
      "profile": ""
    }
  ],
  "controls": [],
  "environments": [
    {
      "name": "unknown",
      "failures": 3,
      "id": "",
      "profile": ""
    }
  ]
}
```

 * **NOTE:** Same API is used with HTTP params (`{"filters":[{"type":"types","values":["profile","control"]}]}`) when togging to `Profile Status` on the `Overview` page. It feeds the `Top Profile Failures` and `Top Control Failures` bubble graphs

### Used on Automate's `/compliance/reports/overview`, `Overview` tab, `Node Status` toggle, `Node Status Over Time` trend graph
 * GRPC API => `GRPC Stats::StatsService, :read_trend, Stats::Query.new(type: "nodes", ...)`
 * HTTP API => `/compliance/reporting/stats/trend` (`{"type":"nodes", ...}`)
 * HTTP RESPONSE:
```json
{
  "trends": [
    {
      "report_time": "2019-12-19T23:59:59Z",
      "passed": 0,
      "failed": 4,
      "skipped": 0,
      "waived": 0
    },
    {
      "report_time": "2019-12-20T23:59:59Z",
      "passed": 0,
      "failed": 3,
      "skipped": 0,
      "waived": 0
    }
  ]
}
```
 * **NOTE:** Same API is used with HTTP params (`{"type":"controls", ...}`) when togging to `Profile Status` on the `Overview` page. It feeds the `Control Status Over Time` trend graph

### Used on Automate's `/compliance/reports/nodes` page, nodes list and status filter buttons (`Failed Nodes`, etc) . Also used when clicking `Scan Results` for a profile or control.
 * GRPC API => `GRPC Reporting::ReportingService, :list_nodes, Reporting::Query.new(...)`
 * HTTP API => `/compliance/reporting/nodes/search`
 * HTTP RESPONSE:
```json
{
  "nodes": [
    {
      "id": "9f0795ea-ada0-4ada-ac3d-ee1fec7060ea",
      "name": "localhost",
      "platform": {
        "name": "ubuntu",
        "release": "16.04",
        "full": "ubuntu 16.04"
      },
      "environment": "unknown",
      "latest_report": {
        "id": "e1bb719f-e35c-4559-a4ec-b932e75c3d0b",
        "end_time": "2019-12-20T00:28:45Z",
        "status": "failed",
        "controls": {
          "total": 92,
          "passed": {
            "total": 0
          },
          "skipped": {
            "total": 27
          },
          "failed": {
            "total": 65,
            "minor": 0,
            "major": 65,
            "critical": 0
          }
        }
      },
      "tags": [],
      "profiles": [
        {
          "name": "cis-azure-foundations",
          "version": "1.0.0-11",
          "id": "ad23aac80f2fdbea30b81ac8852a03c4082fa55fe9ef5ce87c8695044c9c4391",
          "status": "failed",
          "full": "CIS Azure Foundations Benchmark (Beta), v1.0.0-11"
        }
      ]
    }
  ],
  "total": 1,
  "total_passed": 0,
  "total_failed": 1,
  "total_skipped": 0
}
```

### Used on Automate's `/compliance/reports/profiles` page for the profiles list
 * GRPC API => `GRPC Reporting::ReportingService, :list_profiles, Reporting::Query.new(...)`
 * HTTP API => `/compliance/reporting/profiles` (``)
 * HTTP RESPONSE:
```json
{
  "profiles": [
    {
      "name": "cis-azure-foundations",
      "title": "CIS Azure Foundations Benchmark (Beta)",
      "id": "ad23aac80f2fdbea30b81ac8852a03c4082fa55fe9ef5ce87c8695044c9c4391",
      "version": "1.0.0-11",
      "status": "failed"
    }
  ],
  "counts": {
    "total": 1,
    "failed": 1,
    "skipped": 0,
    "passed": 0
  }
}
```

### Used on Automate's `/compliance/reports/controls` page, controls list
 * GRPC API => `GRPC Reporting::ReportingService, :list_control_items, Reporting::ControlItemRequest.new(...)`
 * HTTP API => `/compliance/reporting/controls` (``)
 * HTTP RESPONSE:
```json
{
  "control_items": [
    {
      "id": "1.1",
      "title": "Ensure that multi-factor authentication is enabled for all privileged users",
      "profile": {
        "name": "",
        "title": "CIS Azure Foundations Benchmark (Beta)",
        "id": "ad23aac80f2fdbea30b81ac8852a03c4082fa55fe9ef5ce87c8695044c9c4391",
        "version": "1.0.0-11",
        "status": ""
      },
      "impact": 0.5,
      "end_time": "2019-12-20T00:28:45Z",
      "control_summary": {
        "total": 3,
        "passed": {
          "total": 0
        },
        "skipped": {
          "total": 3
        },
        "failed": {
          "total": 0,
          "minor": 0,
          "major": 0,
          "critical": 0
        }
      }
    }
  ]
}
```

### Used on Automate's `/compliance/compliance-profiles` page, `N Available` tab
 * GRPC API => `GRPC Profiles::ProfilesService, :list, Profiles::Query.new(...)`
 * HTTP API => `/compliance/profiles/search`
 * HTTP RESPONSE:
```json
{
  "profiles": [
    {
      "name": "cis-aix-5.3-6.1-level1",
      "title": "CIS AIX 5.3 and AIX 6.1 Benchmark Level 1",
      "maintainer": "Chef Software, Inc.",
      "copyright": "Chef Software, Inc.",
      "copyright_email": "support@chef.io",
      "license": "Proprietary, All rights reserved",
      "summary": "CIS AIX 5.3 and AIX 6.1 Benchmark Level 1 translated from SCAP",
      "version": "1.1.0-4",
      "owner": "",
      "supports": [
        {
          "os_name": "",
          "os_family": "",
          "release": "",
          "inspec_version": "",
          "platform": ""
        }
      ],
      "depends": [],
      "sha256": "b8f7bcafbf7d3793d2397438d52c5541d1d0e6da681b286b57262e8f45935f0e",
      "groups": [],
      "controls": [],
      "attributes": [],
      "latest_version": ""
    }
  ],
  "total": 1
}
```
 * **NOTE:** Same API is used with HTTP params (`{"owner":"john@example.com"}`) to populate the `N Profiles` tab on the same page.



### Used on Automate's `/compliance/reports/profiles/ad23aac80f2...` page (Compliance > Reports > N Profiles > click profile)
 * GRPC API => `GRPC Stats::StatsService, :read_profiles, Stats::Query.new()`
 * HTTP API => `/compliance/reporting/stats/profiles` (`{"type":"summary","id":"ad23aac80f2...", ...}`)
 * go function called: `stats::ReadProfiles::GetProfileSummaryByProfileId`
 * HTTP RESPONSE:
```json
{
  "profile_list": [],
  "profile_summary": {
    "name": "cis-azure-foundations",
    "title": "CIS Azure Foundations Benchmark (Beta)",
    "version": "1.0.0-11",
    "license": "Proprietary, All rights reserved",
    "maintainer": "Chef Software, Inc.",
    "copyright": "Chef Software, Inc.",
    "copyright_email": "support@chef.io",
    "summary": "A profile to test coverage of the CIS Azure Foundations Benchmark Level 1",
    "supports": [],
    "stats": {
      "failed": 195,
      "passed": 0,
      "skipped": 81,
      "failed_nodes": 0,
      "total_nodes": 0
    },
    "depends": []
  },
  "control_stats": []
}
```

 * **NOTE:** Same API is used with HTTP params (`{"type":"controls","id":"ad23aac80f2...", ...}`) to populate the list of controls for the selected profile.
 * go function called: `stats::ReadProfiles::GetControlListStatsByProfileID`
 * HTTP RESPONSE:
```json
{
  "profile_list": [],
  "profile_summary": null,
  "control_stats": [
   {
      "control": "1.1",
      "title": "Ensure that multi-factor authentication is enabled for all privileged users",
      "passed": 0,
      "failed": 0,
      "skipped": 3,
      "impact": 0.5
   },
   {
     "control": "1.10",
     "title": "Ensure that 'Users can add gallery apps to their Access Panel' is set to 'No'",
     "passed": 0,
     "failed": 0,
     "skipped": 3,
     "impact": 0.5
   }
  ]
}
```

 * **NOTE:** Same API is used with no "type" in the HTTP params when selecting a node and hitting `Scan Results` for it.
 * go function called: `stats::ReadProfiles::GetProfileListWithAggregatedComplianceSummaries`
 * HTTP RESPONSE:
```json
{
  "profile_list": [
    {
      "name": "cis-amazonlinux2-level1",
      "id": "9ed6d61fe646c9ad2a73854fc3ad781dce24fb2344b973e328ae0e6fc50011e9",
      "failures": 0,
      "majors": 0,
      "minors": 0,
      "criticals": 0,
      "passed": 0,
      "skipped": 0
    },
    {
      "name": "linux-baseline",
      "id": "477f53f8f6867a0f1abe7c199a569d00c9d38d8f2a8b85dbb9cc361ca435a2b6",
      "failures": 25,
      "majors": 0,
      "minors": 0,
      "criticals": 25,
      "passed": 27,
      "skipped": 2
    }
  ],
  "profile_summary": null,
  "control_stats": []
}
```


### Used on Automate's `/compliance/reports/profiles` page (`Compliance` > `Reporting` > `N Profiles` > Click `...` for a profile > `Scan Results` > Tap on a node. This is called just to find out the most recent report, take the report id and get that report with another API call.
 * GRPC API => `GRPC Reporting::ReportingService, :list_reports, Reporting::Query.new()`
 * HTTP API => `/compliance/reporting/reports`
 * HTTP RESPONSE:
```json
{
  "reports": [
    {
      "id": "e1bb719f-e35c-4559-a4ec-b932e75c3d0b",
      "node_id": "9f0795ea-ada0-4ada-ac3d-ee1fec7060ea",
      "node_name": "localhost",
      "end_time": "2019-12-20T00:28:45Z",
      "status": "failed",
      "controls": {
        "total": 92,
        "passed": {
          "total": 0
        },
        "skipped": {
          "total": 27
        },
        "failed": {
          "total": 65,
          "minor": 0,
          "major": 65,
          "critical": 0
        }
      },
      "environment": "",
      "version": "",
      "platform": null,
      "statistics": null,
      "profiles": [],
      "job_id": "",
      "ipaddress": "",
      "fqdn": "",
      "chef_server": "",
      "chef_organization": "",
      "roles": [],
      "chef_tags": [],
      "projects": []
    },
    ...
  ]
}
```

### Used on Automate's `/compliance/reports/profiles` page (`Compliance` > `Reporting` > `N Profiles` > Click `...` for a profile > `Scan Results` > Tap on a node.
 * GRPC API => `GRPC Reporting::ReportingService, :read_report, Reporting::Query.new(id: 'e1bb719f-e35c...'`
 * HTTP API => `/compliance/reporting/reports/id/e1bb719f-e35c...`
 * HTTP RESPONSE:
```json
{
  "id": "e1bb719f-e35c-4559-a4ec-b932e75c3d0b",
  "node_id": "9f0795ea-ada0-4ada-ac3d-ee1fec7060ea",
  "node_name": "localhost",
  "end_time": "2019-12-20T00:28:45Z",
  "status": "failed",
  "controls": null,
  "environment": "unknown",
  "version": "4.18.51",
  "platform": {
    "name": "ubuntu",
    "release": "16.04",
    "full": "ubuntu 16.04"
  },
  "statistics": {
    "duration": 0.23399033
  },
  "profiles": [
    {
      "name": "cis-azure-foundations",
      "title": "CIS Azure Foundations Benchmark (Beta)",
    ...
    }
  ]
}
```
