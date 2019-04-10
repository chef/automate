Use this test-kitchen setup to run the audit cookbook in a VM and send the report to Automate

## Dependencies:

 * Vagrant
 * VirtualBox
 * test-kitchen

Berkshelf gem for pulling the cookbooks defined in `Berksfile`
```bash
gem install berkshelf
```

## Converge a VM with the audit cookbook and send the report to Automate
```bash
# Create & Converge the VM with chef-client and audit cookbook as per the details in .kitchen.yml
# We specify the Automate data collector and token as ENV variables
COLLECTOR_URL='https://A2-HOST/data-collector/v0/' COLLECTOR_TOKEN='TOKEN' kitchen converge

# Destroy the VM
kitchen destroy
```

The value for COLLECTOR_TOKEN can be obtained from Automate's hab studio via command:
```bash
get_admin_token
```
or from the Automate UI: `Admin` > `API Tokens` > `Add API Token` > `Generate API Token` > Copy the value
