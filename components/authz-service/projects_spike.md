# Project Mapping Spike

We've added two GRPC endpoints to use against the ingest POC for project mappings.
This is storing the values within OPA and should be pretty similar performance wise
to the final result if we go the OPA route. We have not tested on large project data
sets yet which is probably worth doing soon.

Be sure to rebuild the authz-service before dialing.

```
rebuild components/authz-service
```

## Endpoints

### ListProjectRules

ListProjectRules requires no input values and returns a map of projectID -> list of rules for each project.

```
# grpcurl --insecure -cert /hab/svc/authz-service/config/service.crt -key /hab/svc/authz-service/config/service.key localhost:10130 chef.automate.domain.authz.v2.Projects.ListProjectRules
{
  "projectRules": {
    "project1": {
      "rules": [
        {
          "type": "ChefServers",
          "values": [
            "chef-server-1",
            "chef-server-2",
            "chef-server-3"
          ]
        }
      ]
    },
    "project2": {
...
```

### GetRulesForProject

GetRulesForProject accepts a projectID and returns a list of the rules for the specified project.

```
# grpcurl --insecure -cert /hab/svc/authz-service/config/service.crt -key /hab/svc/authz-service/config/service.key  -d '{"project_id":"project2"}' localhost:10130 chef.automate.domain.authz.v2.Projects.GetRulesForProject
{
  "rulesForProject": {
    "rules": [
      {
        "type": "ChefOrgs",
        "values": [
          "Org1",
          "Org2"
        ]
      },
      {
        "type": "ChefServers",
        "values": [
          "chef-server-3",
          "chef-server-4",
          "chef-server-5"
        ]
      }
    ]
  }
}
```

## Canned Data

We are populating OPA with canned data. This can be seen and modified in
`components/authz-service/server/v2/policy.go` in the `getRuleMap` function.

Feel free to modify the content of this to fit your needs (as long as they
conform to the rule as we've defined it, if that doesn't fit your needs
please chat with auth team).

After updating the rules, a simple `rebuild components/authz-service` will
re-populate the OPA store with your modified data.
