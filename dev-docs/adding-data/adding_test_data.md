# ADDING DATA TO YOUR DEV ENV

## Adding data to event feed

from hab studio:

`chef_load_actions`

## Adding nodes to populate client runs

from hab studio:

there are two options...

1. `chef_load_nodes` 

1. to load infra nodes with failed runs:
    ```shell
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

there are three options...

1. `chef_load_compliance_scans -N100 -D4 -M3 -T1000`

    100 nodes, scanning for the past 4 days, max of 3 node scans per day, no more than 1000 total scans. modify the numbers on each of those options to meet your needs.
1. `chef_load_compliance_nodes`

    runs the above command with the following easy default options: `-N50 -D10 -M1 -T500`
1. `load_compliance_reports`

    will send the reports located in `components/compliance-service/test-data/audit-reports` to your dev environment. these reports are populated with fields such as chef_server, organization, policy_name. these reports are especially useful when you need to test search bar functionality for multiple search fields or resource scoped access ingest match rules.

## Adding nodes to populate the nodes and scan jobs pages

from hab studio:

run `load_scan_jobs`

## Adding profiles

this should be done by downloading profiles from the "Available" tab under the Compliance->Profiles

## Adding cloud integrations

AWS:
 - use the `okta_aws` functionality to get your temporary [aws credentials](https://chefio.atlassian.net/wiki/spaces/ENG/pages/94259373/SOP-001+-+Gaining+access+to+okta+AWS+accounts+via+the+AWS+API#SOP-001-GainingaccesstooktaAWSaccountsviatheAWSAPI-Step1:Installokta_aws)

 - get your credentials: `cat ~/.aws/credentials`

 - use the api to add integrations using these credentials:
 ```shell
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

there are four options...
1. `infra_service_load_sample_data`
    
    By default, 50 records of the infra servers and orgs will be added with the prefix `chef-server` and `chef-org` respectively.
    <sub>_*Only for servers & organizations testing does not get cookbooks, roles, data bags, environments etc data._<sub>
1. `infra_service_load_sample_data -N 100 -S infra-server -O infra-org`
    
    100 records of the infra servers and orgs will be added, modify the numbers on each of those options to meet your needs.
    <sub>_*Only for servers & organizations testing does not get cookbooks, roles, data bags, environments etc data._<sub>
1. [internal steps](https://chefio.slack.com/archives/CPYNX33LJ/p1583485764104300)
    it will add the chef infra server detail running on AWS ec2 instance and will allow you to get data like cookbooks, roles, data bags, environments etc.

1. `infra_service_load_chef_repo`

    This will spin up the local chef infra server and it will add the data resides at [chef-repo](./infra/chef-repo) folder using `knife upload` method.

----------------------------------------------------------------------------------

## Adding a policyfile node
1. Ensure Chef Infra Server is running. If it is not start it with `start_chef_server`
1. Then run `converge_policyfile_chef_client`

# DELETING DATA

to delete the postgres scanjobs data:

`A2_URL='https://a2-dev.test' A2_TOKEN='token_val' components/compliance-service/scripts/delete-pg-data.sh`


to delete the elasticsearch data:

`curl -s http://localhost:10141/_cat/indices/comp*?h=i | while read compliance_index; do curl -X DELETE http://localhost:10141/$compliance_index; done`


----------------------------------------------------------------------------------
# ADDING DATA TO NON-DEV-ENV AUTOMATE 
we don't have hab studio when we're trying to load data into a built instance of automate, so we have to do things a bit differently.

### Modifying the hab studio functions to talk to another automate url
modify `components/ingest-service/chef-load-data/chef-load.toml` to point to the desired automate instance and include an ingest token:
```
data_collector_url = "https://a2-url/data-collector/v0"
data_collector_token = "token"
```
modify `.studio/chef-load`'s prepare_chef_load function to be only:
```
function prepare_chef_load() {
  cp components/ingest-service/chef-load-data/chef-load.toml /tmp/chef-load.toml
  install_if_missing core/busybox-static netstat;
  install_if_missing chef/chef-load chef-load;
}
```
`source .studio/chef-load`

## Adding data to event feed
follow the instructions for "Modifying the hab studio functions to talk to another automate url" then: 
```
chef_load_actions
```

Troubleshooting If your event feed guitar string graph does not show up locally in the UI run:
```
go_update_component config-mgmt-service
```

## Adding services to apps page
TBD

## Adding nodes to populate client runs
from your hab studio:
```shell
for _ in {1..500} ; do   m=$((RANDOM % 4));   n=$((RANDOM % 25));   generate_chef_run_failure_example |     jq --arg msg "Error $n occurred" --arg type "Chef::ExampleError$m" '.error.message = $msg | .error.class = $type' |     send_chef_data_raw https://A2-URL $TOKEN; done
```
_note: play with the args to generate more varied data_

or 
follow the instructions for "Modifying the hab studio functions to talk to another automate url"
`chef_load_nodes`

## Adding nodes to populate compliance reporting
`components/compliance-service/test_data/audit_reports/send_to_data_collector.sh https://a2-url token_value`

or 
follow the instructions for "Modifying the hab studio functions to talk to another automate url"
`load_compliance_reports`

## Profiles, cloud integrations
same instructions as dev env

## Scan jobs
create a credential with your chef AD username and your key (~/.ssh/id_rsa)
create a node with fqdn `localhost`, attach the credential (https://a2-url/compliance/scan-jobs/nodes/add)
create a scan job using that node and profiles installed
