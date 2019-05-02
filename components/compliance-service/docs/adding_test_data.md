# Adding nodes to populate the compliance reporting page

from hab studio: 

`chef_load_compliance_scans -N20` 

# Adding nodes to populate the nodes and scan jobs pages

ensure you have `jq` installed
the token is retrieved by running `get_admin_token` from within the studio
if you've never run the get_secrets script, or haven't in a while, run `CHEF_USERNAME=username scripts/get_secrets` from the studio

_note: this should be run on your local system, not from within the studio_
`source dev/secrets-env.sh`
`A2_URL='https://a2-dev.test' A2_TOKEN='token_val' components/compliance-service/scripts/create-pg-data.sh`

# Adding profiles

this should be done by downloading profiles from the "Available" tab under the "Asset Store"

# Adding cloud integrations

AWS:
 - use the `okta_aws` functionality to get your temporary aws credentials (see details here)[https://chefio.atlassian.net/wiki/spaces/ENG/pages/94259373/SOP-001+-+Gaining+access+to+okta+AWS+accounts+via+the+AWS+API#SOP-001-GainingaccesstooktaAWSaccountsviatheAWSAPI-Step1:Installokta_aws]

 - use the api to add integrations using these credentials:
 ```
 curl -s --insecure -H "api-token: $token_value" https://a2-dev.test/api/v0/nodemanagers -d '{
    "name": "my aws api integration with session token",
    "type": "aws-api",
    "instance_credentials": [],
    "credential_data": [
            {"key": "AWS_ACCESS_KEY_ID", "value": "value" },
            {"key": "AWS_SECRET_ACCESS_KEY", "value": "value" },
            {"key": "AWS_SESSION_TOKEN", "value": "value" }
   ]
}'
```
_Note: to add an ec2 integration for aws, change type from "aws-api" to "aws-ec2"_

Azure:
- turn on the integration by typing 'beta' into your browser and enabling it
- search for 'Compliance Azure Creds' in LastPass
- use these credentials to create your Azure integrations

GCP:
- turn on the integration by typing 'beta' into your browser and enabling it
- search for 'GCP creds (compliance)' in LastPass
- use these credentials to create your GCP integration

# Deleting data

to delete the postgres data:

`A2_URL='https://a2-dev.test' A2_TOKEN='token_val' components/compliance-service/scripts/delete-pg-data.sh`


to delete the elasticsearch data:

`curl -s http://localhost:10141/_cat/indices/comp*?h=i | while read compliance_index; do curl -X DELETE http://localhost:10141/$compliance_index; done`