# Automate Compliance Reports via ElasticSearch

This file documents the ElasticSearch queries need for the Automate Compliance reports.
Other findings related to performance and maintenance over time.

## Pre-condition

Load data with `./make generate`

Install required gems by the scripts that query ElasticSearch via Nginx

```bash
$ bundle install
```

## Report 1: Top failures by platform:

![](https://cloud.githubusercontent.com/assets/107378/23868006/bee3b93e-0815-11e7-8c2f-94a4721df885.png)

Reasoning: Top 10ish platforms with the most non-compliant nodes.

Requirements: nothing special

* API: `/stats/failures?types=platform`
* Ruby script:
```bash
$ bundle exec elastic_failures_by_platform.rb 'compliance-20*'
$ bundle exec elastic_failures_by_platform.rb 'compliance-2017.03.23'
*** Scans.report_ids response 200 OK, returned 2 reports in 0.013 seconds
*** Failures by platform response 200 OK in 0.007 seconds
{"took"=>7,
 "aggregations"=>
  {"platforms"=>
    {"buckets"=>
      [{"key"=>"centos", "failed_nodes"=>{"doc_count"=>1}},
       {"key"=>"debian", "failed_nodes"=>{"doc_count"=>1}}]}}}
```

## Report 2: Top failures by environment:

![](https://cloud.githubusercontent.com/assets/107378/23868007/bee9ff60-0815-11e7-9d48-1642a446abc6.png)

Reasoning: Top 10ish environment with the most non-compliant nodes.

Requirements: enrich the data with environment value

* API: `/stats/failures?types=environment`
* Ruby script:
```bash
$ bundle exec elastic_failures_by_environment.rb 'compliance-20*'
$ bundle exec elastic_failures_by_environment.rb 'compliance-2017.03.23'
*** Scans.report_ids response 200 OK, returned 2 reports in 0.011 seconds
*** Failures by environment response 200 OK in 0.011 seconds
{"took"=>11,
 "aggregations"=>
  {"environments"=>
    {"buckets"=>
      [{"key"=>"DevSec Prod Alpha", "failed_nodes"=>{"doc_count"=>1}},
       {"key"=>"DevSec Prod Zeta", "failed_nodes"=>{"doc_count"=>1}}]}}}
```

## Report 3: Global compliancy across all nodes:

 ![](https://cloud.githubusercontent.com/assets/479121/22980371/3d8bd6ae-f367-11e6-8920-b215240569fe.png)


Reasoning:

Requirements:

* API: `/stats/summary/nodes`
* Ruby script:
```bash
$ bundle exec elastic_global_compliancy_nodes_severity.rb 'compliance-2017.03.19'
$ bundle exec elastic_global_compliancy_nodes_severity.rb 'compliance-20*'
*** Scans.report_ids response 200 OK, returned 2 reports in 0.011 seconds
*** Global compliancy nodes response 200 OK in 0.003 seconds
{"took"=>3,
 "aggregations"=>
  {"medium_risk"=>{"doc_count"=>0},
   "uncompliant_total"=>{"doc_count"=>2},
   "skipped_total"=>{"doc_count"=>0},
   "compliant_total"=>{"doc_count"=>0},
   "low_risk"=>{"doc_count"=>0},
   "high_risk"=>{"doc_count"=>2}}}
```

## Report 4: Top failed profiles

![](https://cloud.githubusercontent.com/assets/107378/23868310/e2843bf6-0816-11e7-82d8-a1eee904f322.png)

Reasoning:

Requirements:
* inspec only provides the profiles with the results of the controls. No aggregation of failed or passed controls. Data enrichment(i.e. compliance_summary.profiles) is required per scan so that we have access to an array of profiles used, along with failure totals per profile.
* `compliance_summary.profiles` array needs to be defined as `nested` in the ElasticSearch mapping!!!

* API: `/stats/failures?types=profiles`
* Ruby script:
```bash
$ bundle exec ./elastic_top_failed_profiles.rb 'compliance-20*'
*** Scans.report_ids response 200 OK, returned 21 reports in 0.049 seconds
*** Overall compliance response 200 OK in 0.042 seconds
[{"key"=>
   "linux-baseline|0704c70df5f251b31b4741e0310d8b69ccf7db05e8a97c3ca0a3422335bdff88",
  "failed_nodes"=>{"doc_count"=>10},
  "title"=>"DevSec Linux Security Baseline"},
 {"key"=>
   "cis-ubuntu14.04lts-level2|5051d03c2fdadd4cbc0140a3a193a6828434728ab19469b44ed5354a77e0234c",
  "failed_nodes"=>{"doc_count"=>5},
  "title"=>"CIS Ubuntu 14.04 LTS Server Benchmark Level 2"},
...
```

## Report 5: Top failed controls

![](https://cloud.githubusercontent.com/assets/107378/23868309/e27a2b8e-0816-11e7-94e9-08b55d8d1dbe.png)

Reasoning: Aggregate the compliance failures from the latest scans at the specified point in time across all nodes (or nodes that match filter(s)) and group/aggregate the failures by control

Top 20 failures needed for the bubble graphs. `doc_count` represents the number of nodes that have this control failed in the most recent scan.

Requirements:
* `profiles`(json) or `controls`(json-min) array in documents. Fields needed:
```
profiles.controls.results.status
profiles.controls.id
```
* `profiles`????(need to clarify here) and `profiles.controls` array need to be nested in the ElasticSearch mapping!!!

* API: `/stats/failures?types=controls`
* Ruby script:
```bash
$ bundle exec elastic_top_failed_controls.rb 'compliance-2020.03.01'
*** Scans.report_ids response 200 OK, returned 2 reports in 0.003 seconds
*** Top controls response 200 OK in 0.001 seconds
{"took"=>1,
 "timed_out"=>false,
 "_shards"=>{"total"=>5, "successful"=>5, "failed"=>0},
 "hits"=>{"total"=>2, "max_score"=>0.0, "hits"=>[]},
 "aggregations"=>
  {"profiles"=>
    {"doc_count"=>54,
     "inner"=>
      {"doc_count"=>27,
       "totals"=>
        {"doc_count_error_upper_bound"=>0,
         "sum_other_doc_count"=>7,
         "buckets"=>
          [{"key"=>"os-02", "doc_count"=>1},
           {"key"=>"os-05", "doc_count"=>1},
           {"key"=>"os-05b", "doc_count"=>1},
           {"key"=>"os-06", "doc_count"=>1},
           {"key"=>"os-08", "doc_count"=>1},
           {"key"=>"sysctl-02", "doc_count"=>1},
           {"key"=>"sysctl-05", "doc_count"=>1},
           {"key"=>"sysctl-06", "doc_count"=>1},
           {"key"=>"sysctl-07", "doc_count"=>1},
           {"key"=>"sysctl-08", "doc_count"=>1},
           {"key"=>"sysctl-09", "doc_count"=>1},
           {"key"=>"sysctl-10", "doc_count"=>1},
           {"key"=>"sysctl-14", "doc_count"=>1},
           {"key"=>"sysctl-15", "doc_count"=>1},
           {"key"=>"sysctl-16", "doc_count"=>1},
           {"key"=>"sysctl-17", "doc_count"=>1},
           {"key"=>"sysctl-18", "doc_count"=>1},
           {"key"=>"sysctl-20", "doc_count"=>1},
           {"key"=>"sysctl-21", "doc_count"=>1},
           {"key"=>"sysctl-22", "doc_count"=>1}]}}}}}
```


## Report 6: Profile list with aggregated compliance summaries

![](https://cloud.githubusercontent.com/assets/479121/23169336/5fb77f1a-f819-11e6-83f5-2a78b673f158.png)

Reasoning: A list of all the profiles with their status across everything+filters i.e. i get a: profile X at version Y is at 87% compliance and has 120 controls; we have 12 criticals, 3 major, 1 minor, 1 skipped, nn passed controls across all nodes in my filters
(use case: executive/auditor comes in and wants to know how we are doing on CIS in our infrastructure; I need a management level summary: "We have 12 more critical controls that are failing in our CIS baseline")

Aggregate all the profiles that's in use across all nodes (or nodes that match filter(s)), then aggregate the compliance results from the latest scans at the specified point in time across all nodes (or nodes that match filter(s))

Requirements:

* inspec only provides the profiles with the results of the controls. No aggregation of failed or passed controls. Data enrichment(i.e. compliance_summary.profiles) is required per scan so that we have access to an array of profiles used, along with failure totals per profile.
* `compliance_summary.profiles` array needs to be defined as `nested` in the ElasticSearch mapping!!!


* API: `/stats/profiles`
* Ruby script:
```bash
$ bundle exec elastic_profiles_list_agg_summaries.rb 'compliance-20*'
*** Scans.report_ids response 200 OK, returned 2 reports in 0.051 seconds
*** Profiles list aggregation response 200 OK in 0.004 seconds
{"took"=>4,
 "aggregations"=>
  {"profiles"=>
    {"totals"=>
      {"buckets"=>
        [{"key"=>
           "linux-baseline|b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
          "doc_count"=>2,
          "failures"=>{"value"=>45.0},
          "majors"=>{"value"=>0.0},
          "minors"=>{"value"=>0.0},
          "criticals"=>{"value"=>45.0},
          "passed"=>{"value"=>45.0},
          "skipped"=>{"value"=>0.0}},
         {"key"=>
           "apache-baseline|65707cb4299e5e821c687f6d5a704ffd3e21f6139a9ad0cc3b438c343b129d8c",
          "doc_count"=>1,
          "failures"=>{"value"=>0.0},
          "majors"=>{"value"=>0.0},
          "minors"=>{"value"=>0.0},
          "criticals"=>{"value"=>0.0},
          "passed"=>{"value"=>0.0},
          "skipped"=>{"value"=>14.0}}}]}}}
```

## Report 7: Node list with aggregated compliance summaries

![](https://cloud.githubusercontent.com/assets/479121/23169256/12266374-f819-11e6-9d39-1567ef3aebf7.png)

Reasoning: Aggregate all nodes (or nodes that match filter(s)), then aggregate the compliance results from the latest scans at the specified point in time

* API: `/nodes`
* Ruby script:
```bash
$ bundle exec elastic_node_list_agg_summaries.rb 'compliance-20*'
*** Scans.report_ids response 200 OK, returned 2 reports in 0.001 seconds
*** Node list aggregation response 200 OK in 0.002 seconds
{"took"=>2,
 "hits"=>
  {"hits"=>
    [{"_source"=>
       {"platform"=>
         {"name"=>"os",
          "os"=>
           {"name"=>"centos",
            "family"=>"redhat",
            "release"=>"5.11",
            "arch"=>"x86_64"}},
        "end_time"=>"2017-03-28T11:31:27+01:00",
        "controls"=>
         {"failed"=>{"total"=>23, "minor"=>0, "major"=>0, "critical"=>23}},
        "node_uuid"=>"4360a1c0-8744-4212-a385-56fcf6224ec4",
        "node_name"=>"turquoise-kingsford",
        "environment"=>"alpha"}},
     {"_source"=>
       {"platform"=>
         {"name"=>"os",
```

## Report 8: Control list within a profile with aggregated compliance summaries

![](https://cloud.githubusercontent.com/assets/479121/23169350/68902718-f819-11e6-9e5f-534ce4c4d6a5.png)

Reasoning: For each control in a single profile, aggregate the compliance results from the latest scans at the specified point in time across all nodes (or nodes that match filter(s))

Requirements:
* `profiles.controls` array needs to be defined as `nested` in the ElasticSearch mapping!!!

* API (top part):    `/stats/profiles/:id/summary`
* API (bottom part): `/stats/profiles/:id/controls`
* Ruby script:

```bash
$ bundle exec elastic_control_list_profile.rb 'compliance-20*'
*** Scans.report_ids response 200 OK, returned 1 reports in 0.002 seconds
*** Control list within a profile response 200 OK in 0.002 seconds
[{"key"=>"os-01",
  "doc_count"=>1,
  "passed"=>{"doc_count"=>1},
  "failed"=>{"doc_count"=>0},
  "skipped"=>{"doc_count"=>0},
  "impact"=>1.0,
  "title"=>"Trusted hosts login"},
 {"key"=>"os-02",
  "doc_count"=>1,
  "passed"=>{"doc_count"=>0},
  "failed"=>{"doc_count"=>1},
  "skipped"=>{"doc_count"=>0},
  "impact"=>1.0,
  "title"=>"Check owner and permissions for /etc/shadow"},
  ...
---------------------------------------------
Total failed: 23
Total passed: 22
Total skipped: 68
Version: 2.0.1
Profile: DevSec Linux Security Baseline
License: Apache 2 license
Supports: [{"os-family"=>"linux"}]
```


## Report 9: Global compliancy status across all controls

![](https://cloud.githubusercontent.com/assets/479121/23169011/2dd06256-f818-11e6-945c-2738faf1acdf.png)

Reasoning: Aggregate the compliance results from each control in use from the latest scans at the specified point in time across all nodes (or nodes that match filter(s))
* API: `/stats/summary/controls`
* Ruby script:
```bash
$ bundle exec elastic_global_compliancy_controls.rb 'compliance-20*'
*** Scans.report_ids response 200 OK, returned 2 reports in 0.002 seconds
*** Global compliance controls response 200 OK in 0.008 seconds
{"took"=>8,
 "aggregations"=>
  {"skipped_total"=>{"value"=>136.0},
   "passed_total"=>{"value"=>44.0},
   "failed_critical"=>{"value"=>46.0},
   "failed_minor"=>{"value"=>0.0},
   "failed_total"=>{"value"=>46.0},
   "failed_major"=>{"value"=>0.0}}}
```

## Report 10: Node complete status

**Hannah to provide image**

Reasoning: One specific node and its status in detail i.e. `my favorite.kitten.node.com has had 3 profiles run with 150 controls at 70% compliancy (+details) in total of which this is the list of controls that were tested with their metadata and Ok/NotOk/Skipped in a list`

Requirements:
* `profiles`(json) array in documents

Script has the `node_uuid` hardcoded inside:

* API: `/nodes/:nodeid`
* Ruby script:
```bash
$ bundle exec elastic_node_details.rb 'compliance-20*' 4360a1c0-8744-4212-a385-56fcf6224ec4
*** Node details response 200 OK in 0.001 seconds
{"took"=>1,
 "hits"=>
  {"hits"=>
    [{"_id"=>"70d095fd-2c5b-4470-aa55-72ec8d69344d",
      "_source"=>
       {"platform"=>
         {"name"=>"os",
          "os"=>
           {"name"=>"centos",
            "family"=>"redhat",
            "release"=>"5.11",
            "arch"=>"x86_64"}},
        "end_time"=>"2017-03-28T11:31:27+01:00",
        "controls"=>
         {"total"=>113,
          "passed"=>{"total"=>22},
          "skipped"=>{"total"=>68},
          "failed"=>{"total"=>23, "minor"=>0, "major"=>0, "critical"=>23}},
        "status"=>"failed",
        "profiles_sums"=>
         [{"profile"=>"linux-baseline-2.0.1",
           "controls"=>
            {"total"=>45,
             "passed"=>{"total"=>22},
             "skipped"=>{"total"=>0},
             "failed"=>{"total"=>23, "minor"=>0, "major"=>0, "critical"=>23}}},
...
-------------------------------------
{"hits"=>
  {"hits"=>
    [{"_source"=>
       {"version"=>"1.17.0",
        "other_checks"=>[],
        "statistics"=>{"duration"=>3.355939},
        "node_uuid"=>"4360a1c0-8744-4212-a385-56fcf6224ec4",
        "node_name"=>"turquoise-kingsford",
        "trigger"=>"Rick",
        "trigger_mode"=>"manual",
        "authz"=>"SOMETHING",
        "doc_version"=>"1",
        "environment"=>"alpha",
        "end_time"=>"2017-03-28T11:31:27+01:00",
        "profiles_min"=>
         [{"name"=>"linux-baseline",
           "version"=>"2.0.1",
           "namespace"=>"someone",
           "sha256"=>"TODO",
           "controls"=>
            [{"id"=>"os-01",
...
```

## Report 11: Trend graph with the number of uncompliant and compliant nodes over time

![](https://cloud.githubusercontent.com/assets/479121/23169514/05294406-f81a-11e6-8266-90aff80cb190.png)

Reasoning: Compliance officer wants to see the compliance state of all nodes (or nodes that match filter(s)) over time. For example, if 10 out of 10 nodes are failing a profile, the above graph will have a straight orange(uncompliant) line at 10(Y axis).

API: `/stats/trend/nodes`
Ruby script:
```bash
$ bundle exec elastic_trend_compliancy_nodes.rb 'compliance-20*'
*** Scans.report_ids response 200 OK, returned 3 reports across 13 ranges in 0.016 seconds
*** Global compliancy nodes response 200 OK in 0.008 seconds
[["2017-02-23",
  {"doc_count"=>0,
   "uncompliant"=>{"doc_count"=>0},
   "compliant"=>{"doc_count"=>0}}],
 ["2017-02-28",
  {"doc_count"=>0,
   "uncompliant"=>{"doc_count"=>0},
   "compliant"=>{"doc_count"=>0}}],
 ["2017-03-05",
  {"doc_count"=>1,
   "uncompliant"=>{"doc_count"=>1},
   "compliant"=>{"doc_count"=>0}}],
...
 ["2017-04-24",
  {"doc_count"=>0,
   "uncompliant"=>{"doc_count"=>0},
   "compliant"=>{"doc_count"=>0}}]]
```

## Report 12: Auto completion of environments

![](https://cloud.githubusercontent.com/assets/107378/24107055/ac0b45d6-0d81-11e7-9c5d-df614f9467fe.png)

Reasoning: When adding search filters, we need to provide suggestions based on user input for fields like environment, platform, node_name, etc:

* API:
```
/suggestions?type=environment&text=dev%20ze
/suggestions?type=platform&text=cent
/suggestions?type=profile&text=cent
/suggestions?type=node&text=bobby
!!! type=controls NOT IMPLEMENTED AT THE MOMENT because filtering on control name is tricky. We don't store control names in the `inspec_summary` document type !!!
```
* Ruby script:
```bash
$ bundle exec elastic_suggest_environment.rb 'compliance-20*' 'dev ze'
*** Environments suggestions response 200 OK in 0.007 seconds
{"took"=>7,
 "hits"=>{"total"=>3, "max_score"=>0.0, "hits"=>[]},
 "aggregations"=>
  {"group_by_name"=>
    {"doc_count_error_upper_bound"=>0,
     "sum_other_doc_count"=>0,
     "buckets"=>
      [{"key"=>"DevSec Prod Alpha",
        "doc_count"=>2,
        "remove_dups"=>
         {"hits"=>
           {"total"=>2,
            "max_score"=>1.5013397,
            "hits"=>
             [{"_index"=>"compliance-2017.04.04",
               "_type"=>"inspec_summary",
               "_id"=>"3ca95021-84c1-43a6-a2e7-be10edcb238d",
               "_score"=>1.5013397}]}}},
       {"key"=>"DevSec Prod Zeta",
        "doc_count"=>1,
        "remove_dups"=>
         {"hits"=>
           {"total"=>1,
            "max_score"=>3.9768348,
            "hits"=>
             [{"_index"=>"compliance-2017.03.06",
               "_type"=>"inspec_summary",
               "_id"=>"44024b50-2e0d-42fa-a57c-25e05e48a1b5",
               "_score"=>3.9768348}]}}}]}}}
```

* API: `/suggestions?type=profile&text=cent`
* Ruby script:
Suggest profile:
```bash
$ bundle exec elastic_suggest_profile.rb 'compliance-profiles' 'dev'
*** Environments suggestions response 200 OK in 0.014 seconds
{"took"=>14,
 "hits"=>
  {"hits"=>
    [{"_id"=>
       "3984753145f0db693e2c6fc79f764e9aff78d892a874391fc5f5cc18f4675b68",
      "_score"=>3.326571,
      "_source"=>{"title"=>"DevSec SSH Baseline", "version"=>"2.1.1"}},
     {"_id"=>
       "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
      "_score"=>3.0509436,
      "_source"=>
       {"title"=>"DevSec Linux Security Baseline", "version"=>"2.0.1"}},
     {"_id"=>
       "65707cb4299e5e821c687f6d5a704ffd3e21f6139a9ad0cc3b438c343b129d8c",
      "_score"=>2.3038783,
      "_source"=>{"title"=>"DevSec Apache Baseline", "version"=>"2.0.1"}}]}}
```

## Report 13: Auto completion on ALL THE THINGS

![](https://cloud.githubusercontent.com/assets/107378/24107055/ac0b45d6-0d81-11e7-9c5d-df614f9467fe.png)

Reasoning: When not selecting a filter(i.e. platform, environment, etc) in the search field, we should search for suggestions across multiple fields(i.e. platform, environment, node name, profile, etc)

Ruby script:
```bash
$ time ./elastic_suggest_all.rb '...' 'devsec'

```

## Report 14: Scan Results (from nodes & profile context)

From nodes context ( all profiles that were run on selected node_uuid, sorted by passed/failed ),<br>
and profiles context ( all nodes that ran selected profile, sorted by passed/failed ):<br>
<img src=https://cloud.githubusercontent.com/assets/10341541/25319789/923426b2-285e-11e7-8086-09c8cb95c1b0.png width="400" height="400">
<img src=https://cloud.githubusercontent.com/assets/10341541/25319799/a453ffa2-285e-11e7-9ce3-9988aa70a963.png width="400" height="400">

These three views are the same for both contexts:<br>
All controls that are part of the selected profile, test results for that control from that profile on that node, information about the control<br>
<img src=https://cloud.githubusercontent.com/assets/10341541/25319791/985e3b18-285e-11e7-8909-89cd07f1e97e.png width="300" height="300">
<img src=https://cloud.githubusercontent.com/assets/10341541/25319792/99da7fd8-285e-11e7-84c0-54666c00d952.png width="300" height="300">
<img src=https://cloud.githubusercontent.com/assets/10341541/25319794/9c969e50-285e-11e7-8522-afc784fd9ccf.png width="300" height="300">

* Ruby script:
```bash
$ bundle exec elastic_nodes_for_failed_control.rb 'compliance-20*'
*** Scans.report_ids response 200 OK, returned 2 reports in 0.005 seconds
*** Nodes for a failed control response 200 OK in 0.009 seconds
{"took"=>9,
 "hits"=>
  {"total"=>2,
   "hits"=>
    [{"_id"=>"3ca95021-84c1-43a6-a2e7-be10edcb238d",
      "_source"=>
       {"node_name"=>"teal-spohn",
        "node_uuid"=>"74a54a28-c628-4f82-86df-61c43866db6a"}},
     {"_id"=>"44024b50-2e0d-42fa-a57c-25e05e48a1b5",
      "_source"=>
       {"node_name"=>"red-brentwood",
        "node_uuid"=>"99516108-8126-420e-b03e-a90a52f25751"}}]}}
```

## Report 15: Trend graph with the number of passed, failed and skipped controls over time

![](https://cloud.githubusercontent.com/assets/107378/25188352/af2b5e0a-251d-11e7-8a79-1ffe1404fcdc.png)

Reasoning: Compliance officer wants to see the state of the controls over time. For example, if 10 nodes are running a profile containing just one control that fails on every node, the above graph will have a straight blue(passed) line at 1(Y axis).

* API: `/stats/trend/controls`
* Ruby script:
```bash
$ bundle exec elastic_trend_compliancy_controls.rb 'compliance-20*'
*** Scans.report_ids response 200 OK, returned 3 reports across 7 ranges in 0.016 seconds
*** Global compliancy controls response 200 OK in 0.089 seconds
[["2017-02-23",
  {"doc_count"=>0,
   "controls-agg"=>
    {"doc_count"=>0,
     "passed-agg"=>{"doc_count"=>0, "id-agg"=>{"value"=>0}},
     "skipped-agg"=>{"doc_count"=>0, "id-agg"=>{"value"=>0}},
     "failed-agg"=>{"doc_count"=>0, "id-agg"=>{"value"=>0}}}}],
...
 ["2017-04-04",
  {"doc_count"=>1,
   "controls-agg"=>
    {"doc_count"=>113,
     "passed-agg"=>{"doc_count"=>22, "id-agg"=>{"value"=>22}},
     "skipped-agg"=>{"doc_count"=>68, "id-agg"=>{"value"=>68}},
     "failed-agg"=>{"doc_count"=>23, "id-agg"=>{"value"=>23}}}}],
 ["2017-04-14",
  {"doc_count"=>0,
   "controls-agg"=>
    {"doc_count"=>0,
     "passed-agg"=>{"doc_count"=>0, "id-agg"=>{"value"=>0}},
     "skipped-agg"=>{"doc_count"=>0, "id-agg"=>{"value"=>0}},
     "failed-agg"=>{"doc_count"=>0, "id-agg"=>{"value"=>0}}}}],
```

## Report 16: Report metadata
* API: `/stats/summary`

* Works with all the typical filters, environment, node_id, profile_id, platform

![](https://cloud.githubusercontent.com/assets/107378/25238478/730464b2-25e5-11e7-8ce4-70cc48e5b6c6.png)

## TODOs:

* Profile id(sha256 content) is not reliable. To be provided by inspec
* add authz info to docs(e.g. user, org, team)?!?!?
* platform target via a more predictable path? `platform.os.name` is not generic enough for cloud targets, etc
* BUG: remove `controls` key from inspec json full
* inspec enhancement: overall status of a profile(avoid everyone needs to implement this)
* decide: version documents for migration purposes later?
* profiles namespacing? yes, how?
* tags? yes, how?
* add `add_time` in profiles elastic documents? Will allow us to do better suggestions since a profile can have multiple versions.


## Profile data needed:

* `compliance_summary.failed/total/skipped` enrichment needed for: `1`, `2`, `3`, `7`, `9`

* `compliance_summary.profiles` enrichment needed for: `4`, `6` or full `profiles`

* full `profiles` or partial:
```
profiles.controls.results.status
profiles.controls.id
```
needed for: `5`

* full `profiles` or partial:
```
profiles.controls.results.status
profiles.controls.id
profiles.controls.title
profiles.controls.impact
```
needed for: `8`

* full `profiles` report for `10`


## BUGs:
Because of [inspec #1580](https://github.com/chef/inspec/issues/1580), the generator was missing the `code` data from controls for full json profiles. By targeting the unpacked profiles, the full profile output grew by 40%:
```
462011  c6-full-without-code.json
650600  c6-full-with-code.json
```
