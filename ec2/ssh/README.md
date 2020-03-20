# About:

Similar to the ec2 Vagrantfile setup but without ec2, just ssh

# Requirements:

## Install the most recent AWS vagrant plugin
```
vagrant plugin install vagrant-managed-servers
```

# Example usage:

```
export SSH_HOST='192.168.56.90'
export SSH_USER='vagrant'
export SSH_KEY_PATH='/Users/apop/.ssh/id_rsa'
export VERSION='dev'
vagrant destroy -f; vagrant up; vagrant provision; vagrant ssh
```
