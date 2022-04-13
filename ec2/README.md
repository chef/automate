# About:

This is a quick way to spin the A2 habitat based development environment in AWS.
Once the requirements are fulfilled, it takes around 5 minutes to have your a2 branch running in ec2.


# Requirements:

## Install Vagrant
If you are on a mac, you can use homebrew:
```
brew install vagrant
```

## Install the most recent AWS vagrant plugin
```
vagrant plugin install vagrant-aws
```

## Import the vagrant AWS dummy box
```
vagrant box add aws https://github.com/mitchellh/vagrant-aws/raw/master/dummy.box
```

~~## Install okta_aws in order to login to AWS via Okta~~

~~https://github.com/chef/okta_aws~~

Note: Access to Chef's AWS accounts via Okta is now deprecated. Instead use Progress Azure AD and SAML.

## Install awscli and saml2aws to login to AWS
If you are on a mac, you can use homebrew:
```
brew install awscli
brew install saml2aws
```

## Configure saml2aws
Run saml2aws configure with the following options:
```
saml2aws configure \
     --idp-provider='AzureAD' \
     --mfa='Auto' \
     --url='https://account.activedirectory.windowsazure.com' \
     --username='USERNAME@progress.com' \
     --app-id='60978246-ce55-4e43-bdd1-f08b130d52bd' \
     --skip-prompt
```

## Use saml2aws to obtain temporary credentials
Login to AWS using saml2aws with chef-engineering as profile name:
```
$ saml2aws login --profile chef-engineering

Using IdP Account default to access AzureAD https://account.activedirectory.windowsazure.com
To use saved password just hit enter.
? Username USERNAME@progress.com
? Password ***************

Authenticating as USERNAME@progress.com ...
? Enter verification code 620226
? Please choose the role Account: chef-engineering (112758395563) / ProgressAzureAD_Administrators
Selected role: arn:aws:iam::112758395563:role/ProgressAzureAD_Administrators
Requesting AWS credentials using SAML assertion.
Logged in as: arn:aws:sts::112758395563:assumed-role/ProgressAzureAD_Administrators/USERNAME@progress.com

Your new access key pair has been stored in the AWS configuration.
Note that it will expire at 2022-03-21 13:23:00 +0530 IST
To use this credential, call the AWS CLI with the --profile option (e.g. aws --profile chef-engineering ec2 describe-instances).
```
Note: Credential is valid for only 1 hour. To refresh the token, you have to relogin via `saml2aws`. You can check credential in `~/.aws/credentials` file.

## Variables

### Required ENV variables

1. `GITHUB_TOKEN` env variable with private repo read access to `chef` org repos

2. `AWS_SSH_KEY_NAME` needs to be set to the name of your SSH key added to the [EC2 Key Pairs](https://us-east-2.console.aws.amazon.com/ec2/v2/home?region=us-east-2#KeyPairs:sort=keyName). This is not used outside of this Vagrantfile so is often passed as an env parameter (see below).
3. `AWS_SSH_KEY_PATH` needs to be set to the path to your SSH key added to the `EC2 Key Pairs`.

### Optional ENV variables

1. `STOP_HOURS` (default: 48) can be used to specify after how many hours the server to self shutdown (stop). Used to prevent unused ec2 instances from running long periods of time.

2. `AWS_ACCESS_KEY_ID` & `AWS_SECRET_ACCESS_KEY`. If not defined, authentication will fallback to `~/.aws/credentials`, created or auto-generated via `okta_aws`.

3. `AWS_PROFILE` can be used to specify the profile to be used from `~/.aws/credentials`. If the `okta_aws` command is installed, it will be invoked with `AWS_PROFILE`.

4. `AWS_EC2_IP` can be used to specify an elastic IP that you prefer to be assigned to the box instead of an ephemeral public IP.

5. `VERSION` in format 'channel[/build]' (default: 'dev/latest') can be used to specify which versions to use when pulling the Automate components. Valid examples: 'dev/20190328132226', 'current', 'acceptance'. The latest build will be pulled from channel if the build timestamp is not specified.

6. `EBS_SNAPSHOT` can be used to specify a volume snapshot to restore for the ElasticSearch data directory. Beneficial to load large data sets previously generated for performance or data migration testing. Here are a few public EBS snapshots that can be used:
 * snap-09008b989ac450bf1 : 75GB volume empty, partitioned and ext4 formatted
 * snap-01c2a3639d3146721 : 75GB volume with comp-2 indices for 50k nodes scanned on 2019.03.29
 * snap-08726019113625ec7 : 75GB volume with comp-2 indices for 50k nodes scanned on 2019.03.29, 2019.04.08, 2019.04.09

Pick previous build timestamps from the three supported channels from here:
 * [dev versions](https://packages.chef.io/manifests/dev/automate/versions.json)
 * [acceptance versions](https://packages.chef.io/manifests/acceptance/automate/versions.json)
 * [current versions](https://packages.chef.io/manifests/current/automate/versions.json)

## Load SSH private key identity

Add to the ssh agent the private key that is allowed to clone the `a2` github repo:
```
ssh-add -K PATH_TO_PRIVATE_KEY
```

## Add vaild Automate JWT license
Provide a valid Automate JWT license as content in ENV variable `AUTOMATE_LICENSE` or save it in this file: `../dev/license.jwt`.

# Launch the A2 instance

The Vagrantfile creates the instance in the `us-east-2`(Ohio) region.

Destroy any previous instance and start it back up.
```
vagrant destroy -f
AWS_SSH_KEY_NAME='apop' AWS_PROFILE='chef-engineering' VERSION='dev' vagrant up
vagrant ssh
vagrant destroy -f
```

or how I like to run it:
```
vagrant destroy -f; AWS_SSH_KEY_NAME='apop' AWS_PROFILE='chef-engineering' VERSION='dev' vagrant up; vagrant ssh
```

This will create the EC2 instance and connect to it via SSH.
It will automatically checkout your a2 branch, install and setup habitat with `ubuntu` HAB_ORIGIN.
It will enter the hab studio and provide these two final steps:
```
>>> TWO MORE STEPS NEEDED TO RUN A2 <<<
1. Run this here: start_all_services
2. Run the A2 UI: http://ec2-11-22-33-44.us-east-2.compute.amazonaws.com
```
Your A2 UI URL will be different ^

Additionally, you can build/rebuild a2 components, for example:
```
rebuild components/ingest-service
hab svc status
chef-automate status
```

See the services logs, from studio shell:
```
sl
```

or from your Unix shell:
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

Can be done via `vagrant ssh` from the `ec2` directory of the a2 repository. This will use the `ubuntu` user which will enter `hab studio` automatically.

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
