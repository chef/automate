#! /bin/sh

# Exit on any error.
set -e

# Check what our platform is
platform=`uname`

mkdir -p /var/opt/delivery/workspace

if [[ "$platform" == "Darwin" ]]; then
  #set root group to admin on osx
  rootgroup="admin"

  # create dbuild user, find a unique ID for that user, find their primary group ID and set their group to that
  dseditgroup -q -o create dbuild || true
  dscl . -create /Users/dbuild || true
  dscl . -create /Users/dbuild NFSHomeDirectory /var/opt/delivery/workspace || true
  dseditgroup -q -o edit -a dbuild -t user dbuild || true
  dscl . -create /Users/dbuild UserShell /bin/bash || true
  dscl . -create /Users/dbuild dbuild "dbuild" || true
  dscl . -create /Users/dbuild UniqueID $(expr 1 + $(dscl . list /Users UniqueID | sort -nk2 | awk '{print $2}' | tail -1)) || true
  dscl . -create /Users/dbuild PrimaryGroupID `dscl . list /Groups PrimaryGroupID | grep dbuild | awk '{print $2}'` || true

else
  # on linux the group root exists
  rootgroup="root"

  # By default centos/RHEL machines don't have sbin on the path, which
  # is where useradd is
  PATH=$PATH:/sbin:/usr/sbin groupadd dbuild || true
  PATH=$PATH:/sbin:/usr/sbin useradd -d /var/opt/delivery/workspace -g dbuild dbuild || true

fi

mkdir -p /var/opt/delivery/workspace/.chef
mkdir -p /var/opt/delivery/workspace/bin
mkdir -p /var/opt/delivery/workspace/lib
mkdir -p /var/opt/delivery/workspace/etc
chmod -R 0755 /var/opt/delivery

cp ./builder_key /var/opt/delivery/workspace/etc/builder_key
cp ./builder_key /var/opt/delivery/workspace/.chef/builder_key
chmod 0600 /var/opt/delivery/workspace/etc/builder_key
chmod 0600 /var/opt/delivery/workspace/.chef/builder_key
chown root:$rootgroup /var/opt/delivery/workspace/etc/builder_key
chown root:$rootgroup /var/opt/delivery/workspace/.chef/builder_key

if [ -f "./custom_certificate_chain.crt" ]
   then
       cp ./custom_certificate_chain.crt /var/opt/delivery/workspace/etc/custom_certificate_chain.crt
       cp ./custom_certificate_chain.crt /var/opt/delivery/workspace/.chef/custom_certificate_chain.crt
       chmod 0600 /var/opt/delivery/workspace/etc/custom_certificate_chain.crt
       chmod 0600 /var/opt/delivery/workspace/.chef/custom_certificate_chain.crt
       chown root:$rootgroup /var/opt/delivery/workspace/etc/custom_certificate_chain.crt
       chown root:$rootgroup /var/opt/delivery/workspace/.chef/custom_certificate_chain.crt
fi

cp ./delivery.pem /var/opt/delivery/workspace/etc/delivery.pem
cp ./delivery.pem /var/opt/delivery/workspace/.chef/delivery.pem
chmod 0600 /var/opt/delivery/workspace/etc/delivery.pem
chmod 0600 /var/opt/delivery/workspace/.chef/delivery.pem
chown root:$rootgroup /var/opt/delivery/workspace/etc/delivery.pem
chown root:$rootgroup /var/opt/delivery/workspace/.chef/delivery.pem

cp ./config.rb /var/opt/delivery/workspace/etc/delivery.rb
cp ./config.rb /var/opt/delivery/workspace/.chef/knife.rb
chmod 0644 /var/opt/delivery/workspace/etc/delivery.rb
chmod 0644 /var/opt/delivery/workspace/.chef/knife.rb
chown dbuild:dbuild /var/opt/delivery/workspace/etc/delivery.rb
chown dbuild:dbuild /var/opt/delivery/workspace/.chef/knife.rb

cp ./git-ssh-wrapper /var/opt/delivery/workspace/bin/git_ssh
chmod 0755 /var/opt/delivery/workspace/bin/git_ssh

cat /etc/chef/trusted_certs/*.crt >> /opt/chefdk/embedded/ssl/certs/cacert.pem

cp delivery-cmd /var/opt/delivery/workspace/bin
chown -R dbuild:dbuild /var/opt/delivery/workspace
chmod 750 /var/opt/delivery/workspace/bin/delivery-cmd
chown root:$rootgroup /var/opt/delivery/workspace/bin/delivery-cmd

# Ensure the dbuild user has access to required chef bits
# so that we can run push-client as dbuild.
chown root:dbuild /etc/chef/client.pem
if [ $EUID == 0 ]; then
  chmod 0755 /etc/chef/trusted_certs
  chmod 0644 /etc/chef/client.rb
  chmod 0644 /etc/chef/trusted_certs/*
  chmod 0640 /etc/chef/client.pem
else
  sudo chmod 0755 /etc/chef/trusted_certs
  sudo chmod 0644 /etc/chef/client.rb
  sudo chmod 0644 /etc/chef/trusted_certs/*
  sudo chmod 0640 /etc/chef/client.pem
fi
