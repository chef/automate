+++
title = "Chef Automate HA with Minimum of 5 nodes"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Chef Automate HA with Minimum nodes"
    parent = "automate/deploy_high_availability/"
    identifier = "automate/deploy_high_availability/"
    weight = 230
+++

This is an usecase where we can achieve chef-automate Ha with 5 nodes 

So among 5 nodes, 3 will be backend nodes and 2 will be frontend nodes. In 3 backend nodes, we will be having PostgreSQL and Elasticsearch on all of them and both frontends will have automate and chef-server services on them.

So to achieve that we need to put instance count as 2 for chef-server and automate, put same ip address for both, and in backend components put instance count as 3 for PostgreSQL and Elasticsearch and put same IP address for both

file:///home/ad.msystechnologies.com/faizan.fulara/Pictures/155130240-1562d89d-c5a9-46e5-b044-82b9ec10a180.png

Considerations to be taken:

1. Backends that is postgresql and elasticsearch will have same 3 IP's
2. Frontends that is automate and chef-server should have same 2 IPs

Deployment should be same go same as on prem config 