## Context
Do Performance testing and benchmarking to capture the API response times for 
different number of sources in automate-ui

## Compliance node count API URL
API endpoint - "/api/v0/compliance/reporting/stats/nodes/count"

## Adding nodes to populate compliance reporting
From hab studio:

1. Command to generate 50k, 100k and 200k nodes as follows:-  
    chef_load_compliance_scans -D2 -N50000 -M1 -T100000  
    chef_load_compliance_scans -D2 -N100000 -M1 -T200000  
    chef_load_compliance_scans -D2 -N200000 -M1 -T400000  


2. Open postgres DB and truncate table in hab studio

    a) chef-automate dev psql postgres  
    b) \c chef_compliance_service  
    c) select * from telemetry;  
    b) truncate table telemetry;  


3. Hit the node count API and get the node count details.  


4. The following details are captured through node count API

   | Node Count | API response time |
   | ---------- | ----------------- |
   | 66472      | 44ms              |
   | 79703      | 76ms              |
   | 119699     | 127ms             |
   | 225292     | 99ms              |



## Application services count API URL
API endpoint - "api/v0/applications/telemetry/services/count"

## Adding application service data
From hab studio:

1. Command to add the sample application service data:-  
    applications_populate_database

NOTE: to add the 10k, 20k, 50k entries we need to do manually changes in .studio/applications-service file.

2. Hit the application serivces count API and get the count details.  


3. The following details are captured through node count API

   | Service Count | API response time |
   | ------------- | ----------------- |
   | 27347         | 74ms              |
   | 55278         | 136ms             |


## chef client-run node count usage API URL
API endpoint - "api/v0/cfgmgmt/telemetry/nodes/count"

## Adding client-run node data
From hab studio:

1. Command to add the client-run data:-  
    chef_load_nodes 10000
    chef_load_nodes 20000
    chef_load_nodes 50000

2. Hit the client-run nodes count API and get the node count details.  


3. The following details are captured through node count API

   | Node Count | API response time |
   | ---------- | ----------------- |
   | 33393      | 36ms              |
