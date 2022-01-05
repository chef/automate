+++
title = "Monitoring Chef Automate"

date = 2019-10-28T14:44:28-07:00
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Monitoring"
    parent = "automate/configuring_automate"
    identifier = "automate/configuring_automate/monitoring.md Monitoring Chef Automate"
    weight = 50
+++

Use the authenticated https endpoint `/status` to monitor your Chef Automate installation.

## Checking the Status Endpoint

The authenticated endpoint `/status` provides status for the Chef Automate installation as well as for its component services.
When all Chef Automate component services are up, `/status` returns a response code of 200.
Otherwise, `/status` returns 500.

The status of a service can be `OK`, `UNKNOWN`, or `CRITICAL`, and is shown in the JSON output:

   ```json
       {
         "ok": false,
         "service_status": [
          {
          "service": "deployment-service",
          "status": "OK"
        },
        {
          "service": "config-mgmt-service",
          "status": "UNKNOWN"
        },
        {
          "service": "ingest-service",
          "status": "CRITICAL"
        },
         ]
       }
   ```

To use `/status`, set up an authentication token for use with your monitoring system by following the steps below:

1. Generate a token:

    ```bash
    chef-automate iam token create --id <token-id> <token-name>
    ```

2. Create a policy that allows your created token to access the `/status` endpoint.

    ```bash
    curl -k -H "api-token: <admin-token>" -d '{ "name": "Monitoring", "id": "monitoring", "members": [ "token:<token-id>" ], "statements": [ { "effect": "ALLOW", "actions": [ "system:status:get" ], "projects": [ "*" ] } ] }' -X POST https://automate.example.com/apis/iam/v2/policies?pretty
    ```

3. Test that your token and policy give you access to the `/status` endpoint by running the following command:

    ```bash
    curl -k -H "api-token: <token-id>" https://automate.example.com/api/v0/status?pretty
    ```

   The output appears in the following JSON format:

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
         ]
       }
   ```

4. After establishing your authentication token and confirming access, connect to the `/status` endpoint.
