# Developing with A2 and Workflow

This describes how to stand up Workflow in A2, and what you need to build
to update Workflow in A2.

## Starting Workflow in A2 Mode

- Run `start_all_services` in your dev environment.
- Run `start_workflow_server`

The following A2 components may need to be rebuilt depending on the scope of your work.

- `components/automate-cli`
- `components/automate-load-balancer`
- `components/automate-deployment`
- `components/automate-workflow-ctl`
- `components/automate-workflow-server`
- `components/automate-workflow-web`
- `components/automate-workflow-nginx`

## Creating an Enterprise

We currently use the `workflow-ctl` command from the `automate-workflow-server`
habitat package to create enterprises and do other interactions with the Workflow
server. You will need to binlink the package with: `install_workflow_ctl` to ensure
the latest dev build is linked.

 - From within the dev studio, create an enterprise for workflow: `create_workflow_enterprise`

## Logging Into Workflow
  - Login to Automate 2.0
  - Select _Client Runs_ from the top navbar.
  - Select _Workflow_ from the left navbar. If _Workflow_ is not appearing in the left navbar then this usually means the service is not running or failed to start.

## Configuring Runners
A Chef Server is required to configure runners.

1. Run `start_chef_server`
1. Create a chef-server organization if one does not already exist. `chef-server-ctl org-create workflow workflow`
1. Add a new `delivery` user to the chef-server with
    chef-server-ctl user-create delivery delivery user dev@chef.io delivery > /hab/svc/automate-workflow-server/var/etc/delivery.pem
	chmod 600 /hab/svc/automate-workflow-server/var/etc/delivery.pem
1. Grant the `delivery` user admin access to the `workflow` org with: `chef-server-ctl org-user-add workflow delivery --admin`
1. Configure the values in the `workflow.toml` example below:

TODO: Automate the above

### workflow.toml
```toml
# Enable and set these values if you are configuring runners.
[workflow.v1.sys.chef_server]
url = "https://chef-server.fdqn/organizations/{my-chef-server-organization}"
web_ui_url = "https://chef-server.fqdn/"
chef_user = "delivery"
```
Run `chef-automate config patch workflow.toml` to apply the configuration.

## Installing Runners

1. Setup a runner host (Ubuntu assumed), and make sure that
   1. a2-dev.test is in /etc/hosts
   2. An accessible SSH key is in authorized_keys of the ubuntu user
   3. The ubuntu user has passwordless sudo

2. Install the runner. Note the full path for the ssh key is needed.
   workflow-ctl install-runner IP_OF_RUNNER ubuntu -i $(pwd)/USER_SSH_KEY

3. In the webui, go to "Manage Runners" tab, and run test to verify things are ok.

NOTE:
If you see something like:
    ERROR: Network Error: getaddrinfo: Name or service not known
check that /etc/hosts has entries for a2-dev.test on both runner and a2 server.

## Building Workflow UI

1. From the top level of the A2 repo, run `hab studio enter` to enter
   the habitat studio.

1. Set the origin for building the nginx package with `export
   vendor_origin=$HAB_ORIGIN` -- this will ensure we build against the
   packages built with your origin.

1. Run `rebuild automate-workflow-web` to build your UI changes.

1. Run `rebuild components/automate-workflow-nginx` to build and install the new UI for Automate 2.

## Building Workflow Server
### Step-by-Step

1. From the top level of the A2 repo, run `hab studio enter` to enter
   the habitat studio.

1. Set the origin for building the nginx package with `export
   vendor_origin=$HAB_ORIGIN` -- this will ensure we build against the
   packages built with your origin.

1. Run `rebuild components/automate-workflow-ctl` to build your UI changes.

1. Run `rebuild components/automate-workflow-server` to build the new
   Workflow server for Automate 2.

1. Update the binlinks to ensure they point to the new build of the
   package: `install_workflow_ctl`

---
![Package Dependencies](images/Workflow_in_Automate-2.svg)
