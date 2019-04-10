# Schema

```
CREATE TABLE comp_profiles(
  id        integer PRIMARY KEY,
  title     text,
  version   text
)

CREATE TABLE comp_controls(
  id         integer PRIMARY KEY,
  title      text,
  profile_id integer
)

CREATE TABLE comp_nodes(
  id          integer PRIMARY KEY,
  name        text,
  platform    text,
  environment text
)

CREATE TABLE comp_scans(
  id         integer PRIMARY KEY,
  node_id    integer,
  end_time   timestamp DEFAULT current_timestamp
)

CREATE TABLE comp_scan_results(
  id           integer PRIMARY KEY,
  scan_id      integer,
  node_id      integer,
  profile_id   integer,
  control_id   integer,
  status       text
)
```

# Queries

Query result examples are from [this small sample set](#data) below.

Query benchmark results are from a larger dataset created by the data generator with the following simulation settings:

```
days: 365
size: 100
max_scans: 24
sample_format: min
```

Scans were generated across 100 nodes for a year resulting in 1047965 scans being generated:

| "table_name"        | "row_estimate" | "total"  | "index"   | "table"      | 
|---------------------|----------------|----------|-----------|--------------| 
| "comp_profiles"     | 0              | "32 kB"  | "16 kB"   | "8192 bytes" | 
| "comp_controls"     | 972            | "584 kB" | "0 bytes" | "576 kB"     | 
| "comp_scans"        | 1.04796e+06    | "164 MB" | "81 MB"   | "83 MB"      | 
| "comp_nodes"        | 100            | "32 kB"  | "16 kB"   | "8192 bytes" | 
| "comp_scan_results" | 2.72848e+08    | "87 GB"  | "8208 MB" | "67 GB"      | 

The machine used to benchmark against was a `db.m4.large` AWS RDS instance (2 CPUs, 8GB RAM).

Results were collected by running this [simple PG benchmarking script](https://github.com/chef/chef-compliance/commit/eff117eddaf157123619e86c5e4ce75597ef7524). It runs each query multiple times with `EXPLAIN (ANALYZE, COSTS, VERBOSE, BUFFERS, FORMAT JSON)` prefixed to the query. It takes each query's `EXPLAIN` results and averages them together.

## Global node overview

#### Global node compliancy

![](https://cloud.githubusercontent.com/assets/479121/23168469/41805ed4-f816-11e6-93a4-3bf0906f34ac.png)

##### Query: 

```
WITH scan_counts_by_node AS (
  WITH scan_counts_by_node_id AS (
    SELECT
      node_id,
      count(status) FILTER (WHERE status = 'passed') AS passed,
      count(status) FILTER (WHERE status = 'failed') AS failed,
      count(impact) FILTER (WHERE status = 'failed' AND impact >= 0.7) AS high,
      count(impact) FILTER (WHERE status = 'failed' AND impact >= 0.4 AND impact < 0.7) AS medium,
      count(impact) FILTER (WHERE status = 'failed' AND impact >= 0 AND impact < 0.4) AS low
    FROM comp_scan_results
    INNER JOIN comp_controls
    ON comp_scan_results.control_id = comp_controls.id
    WHERE scan_id
    IN (
      SELECT DISTINCT ON (node_id) id
      FROM comp_scans
      WHERE end_time <= '2017-02-11 15:46:27'
      ORDER BY node_id, end_time DESC
    )
    GROUP BY node_id
  )
  SELECT passed, failed, high, medium, low
  FROM scan_counts_by_node_id
)
SELECT
  count(failed) FILTER (WHERE failed > 0) AS uncompliant,
  count(failed) FILTER (WHERE failed = 0) AS compliant,
  sum(high) AS high,
  sum(medium) AS medium,
  sum(low) AS low
FROM scan_counts_by_node
```

##### Result: 

| "uncompliant" | "compliant" | "high" | "medium" | "low" | 
|---------------|-------------|--------|----------|-------| 
| 3             | 1           | 3      | 1        | 3     | 

##### Benchmarks:

```
Avg planning time:  1.96
Avg execution time: 734.92
Avg total time:     736.88
```

#### Top failures by platform

![](https://cloud.githubusercontent.com/assets/479121/23168815/895fb488-f817-11e6-8f69-4b2931a2a4c4.png)

##### Query:

```
WITH scan_counts_by_platform AS (
  SELECT
    platform,
    count(status) FILTER (WHERE status = 'failed') AS failed
  FROM comp_scan_results
  INNER JOIN comp_nodes
  ON comp_scan_results.node_id = comp_nodes.id
  WHERE scan_id
  IN (
    SELECT DISTINCT ON (node_id) comp_scans.id
    FROM comp_scans
    WHERE end_time <= '2017-02-12 15:46:27'
    ORDER BY node_id, end_time DESC
  )
  GROUP BY platform
)
SELECT platform, failed
FROM scan_counts_by_platform
ORDER BY failed DESC
```

##### Result:

| "platform" | "failed" | 
|------------|----------| 
| "centos"   | 3        | 
| "ubuntu"   | 2        | 

##### Benchmarks:

```
Avg planning time:  1.05
Avg execution time: 800.25
Avg total time:     801.31
```

#### Top failures by environment

![](https://cloud.githubusercontent.com/assets/479121/23168832/959d3018-f817-11e6-891e-1cca386119c1.png)

##### Query:

```
WITH scan_counts_by_environment AS (
  SELECT
    environment,
    count(status) FILTER (WHERE status = 'failed') AS failed
  FROM comp_scan_results
  INNER JOIN comp_nodes
  ON comp_scan_results.node_id = comp_nodes.id
  WHERE scan_id
  IN (
    SELECT DISTINCT ON (node_id) comp_scans.id
    FROM comp_scans
    WHERE end_time <= '2017-02-12 15:46:27'
    ORDER BY node_id, end_time DESC
  )
  GROUP BY environment
)
SELECT environment, failed
FROM scan_counts_by_environment
ORDER BY failed DESC
```

##### Result:

| "environment" | "failed" | 
|---------------|----------| 
| "production"  | 5        | 
| "staging"     | 0        | 

##### Benchmarks:

```
Avg planning time:  1.01
Avg execution time: 705.32
Avg total time:     706.34
```

#### Control status

![](https://cloud.githubusercontent.com/assets/479121/23169011/2dd06256-f818-11e6-945c-2738faf1acdf.png)

##### Query:

```
WITH scan_counts_by_control AS (
  WITH scan_counts_by_control_id AS (
    SELECT
      control_id,
      count(status) FILTER (WHERE status = 'passed') AS passed,
      count(status) FILTER (WHERE status = 'failed') AS failed,
      count(impact) FILTER (WHERE status = 'failed' AND impact >= 0.7) AS high,
      count(impact) FILTER (WHERE status = 'failed' AND impact >= 0.4 AND impact < 0.7) AS medium,
      count(impact) FILTER (WHERE status = 'failed' AND impact >= 0 AND impact < 0.4) AS low
    FROM comp_scan_results
    INNER JOIN comp_controls
    ON comp_scan_results.control_id = comp_controls.id
    WHERE scan_id
    IN (
      SELECT DISTINCT ON (node_id) id
      FROM comp_scans
      WHERE end_time <= '2017-02-11 15:46:27'
      ORDER BY node_id, end_time DESC
    )
    GROUP BY control_id
  )
  SELECT passed, failed, high, medium, low
  FROM scan_counts_by_control_id
)
SELECT
  count(failed) FILTER (WHERE failed > 0) AS uncompliant,
  count(failed) FILTER (WHERE failed = 0) AS compliant,
  sum(high) AS high,
  sum(medium) AS medium,
  sum(low) AS low
FROM scan_counts_by_control
```

Result:

| "uncompliant" | "compliant" | "high" | "medium" | "low" | 
|---------------|-------------|--------|----------|-------| 
| 4             | 1           | 3      | 1        | 3     | 

##### Benchmarks:

```
Avg planning time:  1.95
Avg execution time: 650.58
Avg total time:     652.52
```

#### Top failed profiles

![](https://cloud.githubusercontent.com/assets/479121/23169131/8d28ebba-f818-11e6-875b-045e9a305d6b.png)

##### Query:

```
WITH scan_counts_by_profile_id AS (
  SELECT
    profile_id,
    count(status) FILTER (WHERE status = 'failed') AS failed
  FROM comp_scan_results
  WHERE scan_id
  IN (
    SELECT DISTINCT ON (node_id) id
    FROM comp_scans
    WHERE end_time <= '2017-02-07 15:46:27'
    ORDER BY node_id, end_time DESC
  )
  GROUP BY profile_id
)
SELECT id, title, failed
FROM scan_counts_by_profile_id
INNER JOIN comp_profiles
ON scan_counts_by_profile_id.profile_id = comp_profiles.id
ORDER BY failed DESC
```

Result:

| "id" | "name"          | "failed" | 
|------|-----------------|----------| 
| 1    | "centos-level1" | 5        | 
| 2    | "centos-level2" | 5        | 
| 5    | "linux-basic1"  | 1        | 

##### Benchmarks:

```
Avg planning time:  1.01
Avg execution time: 685.59
Avg total time:     686.61
```

#### Top failed controls

![](https://cloud.githubusercontent.com/assets/479121/23169175/c076467a-f818-11e6-8b3d-d6ef90c1be8d.png)

##### Query:

```
WITH scan_counts_by_control AS (
  SELECT
    control_id,
    count(status) FILTER (WHERE status = 'passed') AS passed,
    count(status) FILTER (WHERE status = 'failed') AS failed
  FROM comp_scan_results
  WHERE scan_id
  IN (
    SELECT DISTINCT ON (node_id) id
    FROM comp_scans
    WHERE end_time <= '2017-02-11 15:46:27'
    ORDER BY node_id, end_time DESC
  )
  GROUP BY control_id
)
SELECT id, name, passed, failed
FROM scan_counts_by_control
INNER JOIN comp_controls
ON scan_counts_by_control.control_id = comp_controls.id
ORDER BY failed DESC
```

Result:

| "id" | "name"        | "passed" | "failed" | 
|------|---------------|----------|----------| 
| 13   | "Control 5.4" | 0        | 3        | 
| 12   | "Control 5.3" | 2        | 2        | 
| 14   | "Control 6.1" | 1        | 1        | 
| 11   | "Control 5.2" | 3        | 1        | 
| 10   | "Control 5.1" | 1        | 0        | 

##### Benchmarks:

```
Avg planning time:  1.07
Avg execution time: 761.26
Avg total time:     762.33
```

## Node list view

#### List of nodes

![](https://cloud.githubusercontent.com/assets/479121/23169256/12266374-f819-11e6-9d39-1567ef3aebf7.png)

##### Query:

```
WITH scan_counts_by_node AS (
  SELECT
    node_id,
    count(status) FILTER (WHERE status = 'passed') AS passed,
    count(status) FILTER (WHERE status = 'failed') AS failed
  FROM comp_scan_results
  WHERE scan_id
  IN (
    SELECT DISTINCT ON (node_id) id
    FROM comp_scans
    WHERE end_time <= '2017-02-12 15:46:27'
    ORDER BY node_id, end_time DESC
  )
  GROUP BY node_id
)
SELECT id, name, passed, failed
FROM scan_counts_by_node
INNER JOIN comp_nodes
ON scan_counts_by_node.node_id = comp_nodes.id
ORDER BY name
```

##### Result:

| "id" | "name"     | "passed" | "failed" | 
|------|------------|----------|----------| 
| 3    | "centos-1" | 1        | 3        | 
| 4    | "centos-2" | 2        | 0        | 
| 5    | "centos-3" | 4        | 0        | 
| 1    | "ubuntu-1" | 2        | 2        | 

##### Benchmarks:

```
Avg planning time:  1.0
Avg execution time: 816.68
Avg total time:     817.67
```

## Profile list view

#### List of profiles

![](https://cloud.githubusercontent.com/assets/479121/23169336/5fb77f1a-f819-11e6-83f5-2a78b673f158.png)

##### Query:

```
WITH scan_counts_by_profile AS (
  SELECT
    profile_id,
    count(status) FILTER (WHERE status = 'passed') AS passed,
    count(status) FILTER (WHERE status = 'failed') AS failed
  FROM comp_scan_results
  WHERE scan_id
  IN (
    SELECT DISTINCT ON (node_id) id
    FROM comp_scans
    WHERE end_time <= '2017-02-03 15:46:27'
    ORDER BY node_id, end_time DESC
  )
  GROUP BY profile_id
)
SELECT id, name, passed, failed
FROM scan_counts_by_profile
INNER JOIN comp_profiles
ON scan_counts_by_profile.profile_id = comp_profiles.id
ORDER BY name
```

##### Result:

| "id" | "name"          | "passed" | "failed" | 
|------|-----------------|----------|----------| 
| 1    | "centos-level1" | 2        | 5        | 
| 2    | "centos-level2" | 0        | 5        | 
| 5    | "linux-basic1"  | 2        | 1        | 

##### Benchmarks:

```
Avg planning time:  1.01
Avg execution time: 454.59
Avg total time:     455.6
```

## Profile detail View

### List of controls within profile

![](https://cloud.githubusercontent.com/assets/479121/23169350/68902718-f819-11e6-9e5f-534ce4c4d6a5.png)

##### Query:

```
WITH scan_counts_by_control AS (
  SELECT
    control_id,
    count(status) FILTER (WHERE status = 'passed') AS passed,
    count(status) FILTER (WHERE status = 'failed') AS failed
  FROM comp_scan_results
  WHERE scan_id
  IN (
    SELECT DISTINCT ON (node_id) id
    FROM comp_scans
    WHERE end_time <= '2017-02-11 15:46:27'
    ORDER BY node_id, end_time DESC
  )
  AND profile_id = 5
  GROUP BY control_id
)
SELECT id, name, passed, failed
FROM scan_counts_by_control
INNER JOIN comp_controls
ON scan_counts_by_control.control_id = comp_controls.id
ORDER BY name
```

##### Result:

| "id" | "name"        | "passed" | "failed" | 
|------|---------------|----------|----------| 
| 10   | "Control 5.1" | 1        | 0        | 
| 11   | "Control 5.2" | 3        | 1        | 
| 12   | "Control 5.3" | 2        | 2        | 
| 13   | "Control 5.4" | 0        | 1        | 

##### Benchmarks:

```
Avg planning time:  1.08
Avg execution time: 694.66
Avg total time:     695.73
```

# Data

### comp_scans

| "id" | "node_id" | "end_time"            | 
|------|-----------|-----------------------| 
| 1    | 1         | "2017-02-01 15:46:27" | 
| 2    | 1         | "2017-02-09 15:46:27" | 
| 3    | 3         | "2017-02-09 15:46:27" | 
| 4    | 4         | "2017-02-11 15:46:27" | 
| 5    | 5         | "2017-02-11 15:46:27" | 
| 6    | 5         | "2017-02-12 15:46:27" | 

### comp_scan_results

| "id" | "scan_id" | "node_id" | "profile_id" | "control_id" | "status" | 
|------|-----------|-----------|--------------|--------------|----------| 
| 1    | 1         | 1         | 1            | 1            | "failed" | 
| 2    | 1         | 1         | 1            | 2            | "failed" | 
| 3    | 1         | 1         | 1            | 2            | "failed" | 
| 4    | 1         | 1         | 1            | 3            | "passed" | 
| 5    | 1         | 1         | 1            | 4            | "passed" | 
| 6    | 1         | 1         | 1            | 5            | "failed" | 
| 7    | 1         | 1         | 1            | 5            | "failed" | 
| 8    | 1         | 1         | 2            | 6            | "failed" | 
| 9    | 1         | 1         | 2            | 7            | "failed" | 
| 10   | 1         | 1         | 2            | 7            | "failed" | 
| 11   | 1         | 1         | 2            | 8            | "failed" | 
| 12   | 1         | 1         | 2            | 9            | "failed" | 
| 13   | 1         | 1         | 5            | 10           | "failed" | 
| 14   | 1         | 1         | 5            | 11           | "passed" | 
| 15   | 1         | 1         | 5            | 12           | "passed" | 
| 16   | 2         | 1         | 5            | 10           | "passed" | 
| 17   | 2         | 1         | 5            | 11           | "passed" | 
| 18   | 2         | 1         | 5            | 12           | "failed" | 
| 19   | 2         | 1         | 5            | 13           | "failed" | 
| 20   | 3         | 3         | 5            | 11           | "failed" | 
| 21   | 3         | 3         | 5            | 12           | "failed" | 
| 22   | 3         | 3         | 5            | 13           | "failed" | 
| 23   | 3         | 3         | 6            | 14           | "passed" | 
| 24   | 4         | 4         | 5            | 11           | "passed" | 
| 25   | 4         | 4         | 5            | 12           | "passed" | 
| 26   | 5         | 5         | 5            | 11           | "passed" | 
| 27   | 5         | 5         | 5            | 12           | "passed" | 
| 28   | 5         | 5         | 5            | 13           | "failed" | 
| 29   | 5         | 5         | 6            | 14           | "failed" | 
| 30   | 6         | 5         | 5            | 11           | "passed" | 
| 31   | 6         | 5         | 5            | 12           | "passed" | 
| 32   | 6         | 5         | 6            | 14           | "passed" | 
| 33   | 6         | 5         | 6            | 15           | "passed" | 

### comp_profiles

| "id" | "name"          | 
|------|-----------------| 
| 1    | "centos-level1" | 
| 2    | "centos-level2" | 
| 3    | "ubuntu-level1" | 
| 4    | "ubuntu-level2" | 
| 5    | "linux-basic1"  | 
| 6    | "linux-basic2"  | 

### comp_controls

| "id" | "name"        | "profile_id" | "impact" | 
|------|---------------|--------------|----------| 
| 1    | "Control 1.1" | 1            | 1        | 
| 2    | "Control 1.2" | 1            | 0.7      | 
| 3    | "Control 1.3" | 1            | 0.7      | 
| 4    | "Control 1.4" | 1            | 0.4      | 
| 5    | "Control 1.5" | 1            | 1        | 
| 6    | "Control 2.1" | 2            | 1        | 
| 7    | "Control 2.2" | 2            | 1        | 
| 8    | "Control 2.3" | 2            | 0        | 
| 9    | "Control 2.4" | 2            | 0.4      | 
| 10   | "Control 5.1" | 5            | 0.7      | 
| 11   | "Control 5.2" | 5            | 0.5      | 
| 12   | "Control 5.3" | 5            | 1        | 
| 13   | "Control 5.4" | 5            | 0        | 
| 14   | "Control 6.1" | 6            | 1        | 
| 15   | "Control 6.2" | 6            | 0.4      | 

### comp_nodes

| "id" | "name"     | "platform" | "environment" | 
|------|------------|------------|---------------| 
| 1    | "ubuntu-1" | "ubuntu"   | "production"  | 
| 2    | "ubuntu-2" | "ubuntu"   | "production"  | 
| 3    | "centos-1" | "centos"   | "production"  | 
| 4    | "centos-2" | "centos"   | "production"  | 
| 5    | "centos-3" | "centos"   | "staging"     | 
| 6    | "centos-4" | "centos"   | "staging"     | 
