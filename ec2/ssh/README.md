# About:

Similar to the ec2 Vagrantfile setup but without ec2, just ssh

# Requirements:

## Install the most recent AWS vagrant plugin
```
vagrant plugin install vagrant-managed-servers
```

# Run

``
vagrant destroy -f; SSH_HOST='192.168.56.90' SSH_USER='vagrant' SSH_KEY_PATH='/Users/apop/.ssh/id_rsa' VERSION='dev' vagrant up; vagrant ssh
``
