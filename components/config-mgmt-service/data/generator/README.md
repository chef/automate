# Automate Data Generator

## Bulk data generator

Run `make generate` to generate 100 nodes.

This generates 100 nodes and related converges in a semi-random fashion in the current time window for API and UI dev work. It creates bulk (but totally fake) data and injects it directly into
Elasticsearch, completely bypassing the data collector API. When run with no
special environment variables, this will point to a default Elasticsearch
endpoint on the local machine (localhost port 9200 - available with the
local-dev docker environment).

## Limitations

The current version generates only one node plus a range of converge history
for that node. It currently inserts barebones incomplete data (small subsets of
each document's fields), so some Visibility features don’t work at the moment.

In particular, converges only contain very-high-level info like times and node
name/ip/etc. There’s no cookbooks, recipes, resources, etc. included yet.

TBD improvements:
-   [ ] enable converge-trend-graph by adding `@timestamp` fields
    (OR change the trend graph to use `end_time` instead of `@timestamp`)
-   [ ] generate more than one node (should be easy, just wrap the current main
  in a loop)
-   [ ] generate data for multiple chef-server orgs (N nodes per org, or random
  sample?)
-   [ ] generate data for multiple chef-servers
-   [ ] generate data for chef-solo converges (i.e. a special case of
  chef-server/org)
-   [ ] chef-server object updates (e.g. cookbook uploads, roles)
