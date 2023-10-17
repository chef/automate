include disable-common.inc      # dangerous directories like ~/.ssh and ~/.gnupg
#include disable-devel.inc      # development tools such as gcc and gdb
#include disable-exec.inc       # non-executable directories such as /var, /tmp, and /home
#include disable-interpreters.inc       # perl, python, lua etc.
include disable-programs.inc    # user configuration for programs such as firefox, vlc etc.
#include disable-shell.inc      # sh, bash, zsh etc.
#include disable-xdg.inc        # standard user directories: Documents, Pictures, Videos, Music

### Home Directory Whitelisting ###
### If something goes wrong, this section is the first one to comment out.
### Instead, you'll have to relay on the basic blacklisting above.
#private 
blacklist /hab/cache
blacklist /hab/etc
blacklist /hab/svc
blacklist /hab/launcher
blacklist /hab/user
blacklist /hab/studios
blacklist /hab/sup
blacklist /hab/pkgs/chef/applications-service
blacklist /hab/pkgs/chef/automate-dex
blacklist /hab/pkgs/chef/automate-opensearch
blacklist /hab/pkgs/chef/backup-gateway
blacklist /hab/pkgs/chef/deployment-service
blacklist /hab/pkgs/chef/infra-proxy-service
blacklist /hab/pkgs/chef/local-user-service
blacklist /hab/pkgs/chef/report-manager-service
blacklist /hab/pkgs/chef/authn-service
blacklist /hab/pkgs/chef/automate-es-gateway
blacklist /hab/pkgs/chef/automate-pg-gateway
blacklist /hab/pkgs/chef/cereal-service
blacklist /hab/pkgs/chef/es-sidecar-service
blacklist /hab/pkgs/chef/ingest-service
blacklist /hab/pkgs/chef/mlsa
blacklist /hab/pkgs/chef/secrets-service
blacklist /hab/pkgs/chef/authz-service
blacklist /hab/pkgs/chef/automate-gateway
blacklist /hab/pkgs/chef/automate-platform-tools
blacklist /hab/pkgs/chef/compliance-service
blacklist /hab/pkgs/chef/event-feed-service
blacklist /hab/pkgs/chef/nodemanager-service
blacklist /hab/pkgs/chef/session-service
blacklist /hab/pkgs/chef/automate-cli
blacklist /hab/pkgs/chef/automate-load-balancer
blacklist /hab/pkgs/chef/automate-postgresql
blacklist /hab/pkgs/chef/config-mgmt-service
blacklist /hab/pkgs/chef/event-gateway
blacklist /hab/pkgs/chef/license-audit
blacklist /hab/pkgs/chef/notifications-service
blacklist /hab/pkgs/chef/teams-service
blacklist /hab/pkgs/chef/automate-compliance-profiles
blacklist /hab/pkgs/chef/automate-openjdk
blacklist /hab/pkgs/chef/automate-ui
blacklist /hab/pkgs/chef/data-feed-service
blacklist /hab/pkgs/chef/event-service
blacklist /hab/pkgs/chef/license-control-service
blacklist /hab/pkgs/chef/pg-sidecar-service
blacklist /hab/pkgs/chef/user-settings-service
read-only /hab/pkgs/chef/inspec
blacklist /hab/pkgs/yjain
 
### Filesystem Whitelisting ###
include whitelist-run-common.inc
include whitelist-runuser-common.inc
include whitelist-usr-share-common.inc
include whitelist-var-common.inc

#apparmor       # if you have AppArmor running, try this one!
caps.drop all
ipc-namespace
netfilter
#no3d   # disable 3D acceleration
#nodvd  # disable DVD and CD devices
#nogroups       # disable supplementary user groups
#noinput        # disable input devices
nonewprivs
noroot
#notv   # disable DVB TV devices
#nou2f  # disable U2F devices
#novideo        # disable video capture devices
net none
#seccomp !chroot        # allowing chroot, just in case this is an Electron app
#shell none
#tracelog       # send blacklist violations to syslog

#disable-mnt    # no access to /mnt, /media, /run/mount and /run/media
#private-bin dash, hab
#private-cache  # run with an
#read-only /hab