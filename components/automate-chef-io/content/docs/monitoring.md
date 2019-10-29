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
Automate installation as well as its component services. To set up a token that can be
used with your monitoring system:

1. Generate a token by running
```bash
chef-automate iam token create --id <token-id> <token-name>
```

1. Create a policy that allows the token you created to access the `/status`
   endpoint:
```bash
curl -k -H "api-token: <admin-token>" -H "Content-Type: application/json" -d '{ "action": "read", "resource": "service_info:status", "subjects": [ "token:<token-id>" ] }' https://automate.example.com/api/v0/auth/policies?pretty
```

1. Test that your token and policy give you access to the `/deployment/status` endpoint by
   running
```bash
curl -k -H "api-token: <token-id>" https://automate.example.com/api/v0/status?pretty
```
The output will look like this:
```
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

