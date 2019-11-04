+++
title = "Monitoring Chef Automate"
description = "Instructions for monitoring AUtomate"
date = 2019-10-28T14:44:28-07:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "get_started"
    weight = 50
+++

## Checking the status endpoint
The authenticated endpoint `/status` provides status for the overall Chef
Automate installation as well as its component services. When all Chef Automate component
services are up, `/status` returns a response code of 200; otherwise, it returns 500.

To set up a token that can be used with your monitoring system:

1. Generate a token:
```bash
chef-automate iam token create --id <token-id> <token-name>
```

2. Create a policy that allows the token you created to access the `/status`
   endpoint. If you are using IAM v1, the command is:
```bash
curl -k -H "api-token: <admin-token>" -H "Content-Type: application/json" -d '{ "action": "read", "resource": "service_info:status", "subjects": [ "token:<token-id>" ] }' https://automate.example.com/api/v0/auth/policies?pretty
```
If you are using IAM v2, the command is
```bash
curl -k -H "api-token: <admin-token>" -d '{ "name": "Monitoring", "id": "monitoring", "members": [ "token:<token-id>" ], "statements": [ { "effect": "ALLOW", "actions": [ "system:status:get" ], "projects": [ "*" ] } ] }' -X POST https://automate.example.com/apis/iam/v2beta/policies?pretty
```

3. Test that your token and policy give you access to the `/status` endpoint by
   running
```bash
curl -k -H "api-token: <token-id>" https://automate.example.com/api/v0/status?pretty
```
The output will look like this:
```json
    {
      "ok": true,
      "services": [
        {
          "name": "deployment-service",
          "status": "OK"
        },
        {
          "name": "backup-gateway",
          "status": "OK"
        },
        {
          "name": "automate-postgresql",
          "status": "OK"
        },
        ...
      ]
    }
```

