# About:

Similar to the ec2 Vagrantfile setup but without ec2, just ssh

# Requirements:

## Install the most recent AWS vagrant plugin
```
vagrant plugin install vagrant-managed-servers
```

## Import the vagrant AWS dummy box
```
vagrant box add dummy https://github.com/tknerr/vagrant-managed-servers/raw/master/dummy.box --provider=managed
```
