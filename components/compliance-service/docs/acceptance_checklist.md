 # acceptance checklist
from the context of one of the acceptance instances, logged in as any user:

 ## test credentials (-AUTOMATED IN CYPRESS TESTS-)
 - create password credential (username and password available in
   vault under secret/a2/testing/target_username and secret/a2/testing/target_password)

 ## test manual node creation, detect job (-AUTOMATED IN CYPRESS TESTS-)
 - create, edit manual node (hostname available in vault under secret/a2/testing/target_hostname)
   attach the above credential)

 ## test asset store
 - view and delete install linux-baseline, windows-baseline, aws-cis profiles
 - ensure the event feed has received the deleted profile events
 - install linux-baseline or ssh-baseline, windows-baseline, and aws-cis profiles
 - install profile with dependencies: see `api/tests/mario-0.1.0.tar.gz` or make your own
 - ensure the event feed has received the profile creation events

 ## test nodemanager creation
 - create cloud integration for AWS-API (use read creds from env option or enter in your own aws creds)
 - ensure nodes were added to the system and are reachable

 ## test scan jobs
 - create job for manual node manager
 - create job for aws-api mgr (recommend just one region scan)
 - create a job with a recurrence schedule (-AUTOMATED IN CYPRESS TESTS-)
 - ensure the event feed has received the job creation events
 - select sort and filter buttons on jobs and nodes list under scan jobs tab

 ## test reporting
 - select sort and filter buttons on compliance reporting pages
 - compliance reporting: remember to check node details and profile details page, open control results screens (scan results), see source code of controls, just select all the buttons and ensure they do the right thing
 - download a report without filters applied
 - download a report with filters applied
 - play with the search bar

 ## test it worked
 - check on job with recurrence, ensure jobs were executed, have reports
 - delete credential, node, nodemanager, job

 ## test inspec compliance cli with automate
 - login with automate url and token `inspec compliance login <automate_url> --insecure --user=admin --token=<token_val>`
 - list profiles: `inspec compliance profiles`
 - upload profile to automate: `inspec compliance upload`

 ## test audit cookbook with automate
 - take a look at the readme in components/compliance-service/smokin, and use that kitchen setup to test audit cookbook against automate
 `COLLECTOR_URL='<automate_url>/data-collector/v0' COLLECTOR_TOKEN='<token_val>' kitchen converge`
 - ensure the linux baseline profile is installed, and change the profiles part of the `.kitchen.yml` to fetch from compliance instead of url. Run the command again, ensure things work.


--------------------------------------
# NOTES:

### deleting compliance elasticsearch data:
```
curl -s http://localhost:10141/_cat/indices/comp*?h=i | while read compliance_index; do curl -X DELETE http://localhost:10141/$compliance_index; done

chef-automate restart-services
```

### deleting postgres data:
The A2 token is available in vault under 'secret/a2/testing/a2_token'

```
A2_URL='https://HOST' A2_TOKEN='TOKEN' components/compliance-service/scripts/delete-pg-data.sh
```
