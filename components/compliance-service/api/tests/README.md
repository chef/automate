# About:

This directory contains Ruby based integration tests for the compliance-service.

There's a Vagrantfile in this directory that can setup via ssh an Ubuntu host to run these integration tests.

# Use:

## Install the `vagrant-managed-servers` plugin

```
vagrant plugin install vagrant-managed-servers
```

## Example usage:

```
export SSH_HOST='192.168.56.90'
export SSH_USER='vagrant'
export SSH_KEY_PATH='~/.ssh/id_rsa'
vagrant up
vagrant provision
# Follow the instructions provided at the end of the provision step. For example:
vagrant ssh
cd automate/components/compliance-service
TEST="00*" make run-test
```
