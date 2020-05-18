# ADDING DATA TO YOUR DEV ENV

## Adding data to event feed

from hab studio:

`chef_load_actions`

## Adding nodes to populate client runs

from hab studio:

option one: `chef_load_nodes` 

option two: 
to load infra nodes with failed runs:
```
for _ in {1..500} ; do   m=$((RANDOM % 4));   n=$((RANDOM % 25));   generate_chef_run_failure_example |     jq --arg msg "Error $n occurred" --arg type "Chef::ExampleError$m" '.error.message = $msg | .error.class = $type' |     send_chef_data_raw https://A2-URL $TOKEN; done
```
_note: play with the args to generate more varied data_

## Adding services to apps page

from hab studio:

`applications_populate_database` 

## Adding desktops data to desktops page

from hab studio:

`load_desktop_reports` 

## Adding nodes to populate compliance reporting

from hab studio:

option one: `chef_load_compliance_scans -N100 -D4 -M3 -T1000` 
100 nodes, scanning for the past 4 days, max of 3 node scans per day, no more than 1000 total scans. modify the numbers on each of those options to meet your needs.

option two: `chef_load_compliance_nodes` runs the above command with the following easy default options: `-N50 -D10 -M1 -T500`

option three: `load_compliance_reports` will send the reports located in `components/compliance-service/test-data/audit-reports` to your dev enironment. these reports are populated with fields such as chef_server, organization, policy_name. these reports are especially useful when you need to test searchbar functionality for multiple search fields or resource scoped access ingest match rules.

## Adding nodes to populate the nodes and scan jobs pages

from hab studio:

run `load_scan_jobs`

## Adding profiles

this should be done by downloading profiles from the "Available" tab under the Compliance->Profiles

## Adding cloud integrations

AWS:
 - use the `okta_aws` functionality to get your temporary aws credentials (see details here)[https://chefio.atlassian.net/wiki/spaces/ENG/pages/94259373/SOP-001+-+Gaining+access+to+okta+AWS+accounts+via+the+AWS+API#SOP-001-GainingaccesstooktaAWSaccountsviatheAWSAPI-Step1:Installokta_aws]

 - get your credentials: `cat ~/.aws/credentials`

 - use the api to add integrations using these credentials:
 ```
 curl -s --insecure -H "api-token: $token_value" https://a2-dev.test/api/v0/nodemanagers -d '{
    "name": "my aws api integration with session token",
    "type": "aws-api",
    "instance_credentials": [],
    "credential_data": [
            {"key": "AWS_ACCESS_KEY_ID", "value": "your-aws-access-key" },
            {"key": "AWS_SECRET_ACCESS_KEY", "value": "your-aws-secret-key" },
            {"key": "AWS_SESSION_TOKEN", "value": "your-aws-token" }
   ]
}'
```
_Note: to add an ec2 integration for aws, change type from "aws-api" to "aws-ec2"_

If testing in an instance running in AWS, you can use the "read creds from cloud env" option to make things easier.

## Adding data to infra views

from hab studio:

ensure you have `jq` installed

option one: `infra_service_load_sample_data`
By default, 50 records of the infra servers and orgs will be added with the prefix `chef-server` and `chef-org` respectively.

option two: `infra_service_load_sample_data -N 100 -S infra-server -O infra-org`
100 records of the infra servers and orgs will be added, modify the numbers on each of those options to meet your needs.

option three: look through the pinned items in slack channel #automate-infra-views and follow the instructions there (works against running chef server)

----------------------------------------------------------------------------------
# DELETING DATA

to delete the postgres data:

`A2_URL='https://a2-dev.test' A2_TOKEN='token_val' components/compliance-service/scripts/delete-pg-data.sh`


to delete the elasticsearch data:

`curl -s http://localhost:10141/_cat/indices/comp*?h=i | while read compliance_index; do curl -X DELETE http://localhost:10141/$compliance_index; done`


----------------------------------------------------------------------------------
# ADDING DATA TO NON-DEV-ENV AUTOMATE 
we don't have hab studio when we're trying to load data into a built instance of automate, so we have to do things a bit differently.

## Adding data to event feed
TBD

## Adding services to apps page
TBD

## Adding nodes to populate client runs
from your hab studio:
```
for _ in {1..500} ; do   m=$((RANDOM % 4));   n=$((RANDOM % 25));   generate_chef_run_failure_example |     jq --arg msg "Error $n occurred" --arg type "Chef::ExampleError$m" '.error.message = $msg | .error.class = $type' |     send_chef_data_raw https://A2-URL $TOKEN; done
```
_note: play with the args to generate more varied data_

## Adding nodes to populate compliance reporting
`components/compliance-service/test_data/audit_reports/send_to_data_collector.sh https://a2-url token_value`

## Profiles, cloud integrations
same instructions as dev env

## Scan jobs
create a credential with your chef AD username and your key (~/.ssh/id_rsa)
create a node with fqdn `localhost`, attach the credential (https://a2-url/compliance/scan-jobs/nodes/add)
create a scan job using that node and profiles installed

## Adding data to infra views
use the credentials pinned in the #automate-infra-views slack channel to add a chef server and then org via the ui, at a2-url/infrastructure/chef-servers
