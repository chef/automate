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


