Use this test-kitchen setup to run the audit cookbook in a VM and send the report to Automate

## Dependencies:

 * Vagrant
 * VirtualBox
 * Install necessary ruby gems:

    ```bash
    bundle
    ```

## Converge a VM with the audit cookbook and send the report to Automate
```bash
# Create & Converge the VM with chef-client and audit cookbook as per the details in .kitchen.yml
# We specify the Automate data collector and token as ENV variables
COLLECTOR_URL='https://A2-HOST/data-collector/v0/' COLLECTOR_TOKEN='TOKEN' kitchen converge

# Destroy the VM
kitchen destroy
```

## Generating a token

* The value for COLLECTOR_TOKEN can be obtained from Automate's hab studio via command:
  ```bash
  get_admin_token
  ```

* Or from the Automate UI:
  1. Navigate to `Settings` > `API Tokens` > `Create Token`
  1. Fill in the form. Remember or copy the ID.
  1. Navigate to `Settings` > `Policies` > `Ingest` > `Members`
    (or `Administrator` to grant admin permissions)
  1. Click `Add Members`
  1. In the page modal that opens, click `Add Member Expression`.
    Select the Type `Token` and enter the ID of your new token.
  1. Return to `Settings` > `API Tokens`
  1. Click the three dot menu on the right side of your token's row and click `Copy Token`.
