# About:

This is a quick way to spin the A2 habitat based development environment in AWS.
Once the requirements are fulfilled, it takes around 5 minutes to have your automate branch running in ec2.


# Requirements:

## Install the AWS vagrant plugin
```
vagrant plugin install vagrant-aws
```

## Import the vagrant AWS dummy box
```
vagrant box add aws https://github.com/mitchellh/vagrant-aws/raw/master/dummy.box
```

## Variables

### Required ENV variables

1. `GITHUB_TOKEN` env variable with private repo read access to `chef` org repos

2. `AWS_ACCESS_KEY_ID` & `AWS_SECRET_ACCESS_KEY` of `Chef Engineering AWS` account.
You can create them by reachable via Okta

3. `AWS_SSH_KEY_NAME` needs to be set to the name of your SSH key added to the [EC2 Key Pairs](https://us-east-2.console.aws.amazon.com/ec2/v2/home?region=us-east-2#KeyPairs:sort=keyName). This is not used outside of this Vagrantfile so is often passed as an env parameter (see below).

### Optional ENV variables

1. `STOP_HOURS` (default: 48) can be used to specify after how many hours the server to self shutdown (stop). Used to prevent unused ec2 instances from running long periods of time.

2. `AWS_EC2_IP` can be used to specify an elastic IP that you prefer to be assigned to the box instead of an ephemeral public IP.

3. `VERSION` in format 'channel[/build]' (default: 'dev/latest') can be used to specify which versions to use when pulling the Automate components. Valid examples: 'dev/20190328132226', 'current', 'acceptance'. The latest build will be pulled from channel if the build timestamp is not specified.

Pick previous build timestamps from the three supported channels from here:
 * [dev versions](https://packages.chef.io/manifests/dev/automate/versions.json)
 * [acceptance versions](https://packages.chef.io/manifests/acceptance/automate/versions.json)
 * [current versions](https://packages.chef.io/manifests/current/automate/versions.json)

## Load SSH private key identity

Add to the ssh agent the private key that is allowed to clone the `automate` github repo:
```
ssh-add
```

# Launch the A2 instance

The Vagrantfile creates the instance in the `us-east-2`(Ohio) region.

Destroy any previous instance and start it back up.
```
vagrant destroy -f
AWS_SSH_KEY_NAME='apop' VERSION='dev' vagrant up
vagrant ssh
vagrant destroy -f
```

or how I like to run it:
```
vagrant destroy -f; AWS_SSH_KEY_NAME='apop' VERSION='dev' vagrant up; vagrant ssh
```

This will create the EC2 instance and connect to it via SSH.
It will automatically checkout your automate branch, install and setup habitat with `ubuntu` HAB_ORIGIN.
It will enter the hab studio and provide these two final steps:
```
>>> TWO MORE STEPS NEEDED TO RUN A2 <<<
1. Run this here: start_all_services
2. Run the A2 UI: http://ec2-11-22-33-44.us-east-2.compute.amazonaws.com
```
Your A2 UI URL will be different ^

Additionally, you can build/rebuild automate components, for example:
```
rebuild components/ingest-service
hab svc status
chef-automate status
```

See the services logs, from studio shell:
```
sl
```

or from your favourite Linux shell:
```
tail -f /hab/studios/home--*/hab/sup/default/sup.log
```

## Upgrading to another Automate build

Let's say you specified `acceptance` channel for the automate VERSION and you want to upgrade to a new version to test data migration. Run this command in the hab studio to overwrite the original manifest and trigger an upgrade to the latest build from the dev channel:

```
wget -O /src/results/build.json "https://packages.chef.io/manifests/dev/automate/latest.json"
```
If you want a specific build, replace `latest` in the URL above with the build timestamp(Ex: 20190329033403). Find all `dev` build timestamps in `versions.json`

Wait 30 seconds or so for the auto-upgrade to kick in. You can verify the progress of the upgrade with:
```
chef-automate upgrade status
```

## SSH into the server

Can be done via `vagrant ssh` from the `ec2` directory of the automate repository. This will use the `ubuntu` user which will enter `hab studio` automatically.

To bypass the hab studio, you can login as user `root` using the private key associated with the public key specified via ENV variable `AWS_SSH_KEY_NAME`.

## Accessing the UI

* After loading successfully, the URL of the created EC2 instance is output in the terminal (see above).
* The default username and password are admin/chefautomate currently and can be found in the [UI development documentation](https://github.com/chef/automate/blob/master/dev-docs/ui-development.md).

## Accessing hab services from your local machine

Let's say we want to use Postman locally to query the ElasticSearch hab service running on our remote ec2 instance.

Start an SSH tunnel like this, where TCP port 9222 on your laptop is proxied to the remote ec2 instance as port 10141. The `ec2-11-22...amazonaws.com` hostname will be different for you.
```
ssh -L 9222:127.0.0.1:10141 ubuntu@ec2-11-22-33-44.us-east-2.compute.amazonaws.com /bin/bash
```

To test that out, leave the above ssh connection running and run this command in a new terminal or hit the URL in Postman:
```
curl "http://127.0.0.1:9222/_cat/indices?v&pretty&s=index"
```

# Termination

In order to avoid waste, instances launched with this Vagrantfile will automatically stop after running for more than 2 days. If you want to increase or decrease this number based on your circumstance, you can set the STOP_HOURS variable same way AWS_SSH_KEY_NAME is set above. Default value for STOP_HOURS is 48.

A stopped instance can be started back up or terminated from AWS EC2 instances page.
...
