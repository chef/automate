#include disable-common.inc      # dangerous directories like ~/.ssh and ~/.gnupg
#include disable-devel.inc      # development tools such as gcc and gdb
#include disable-exec.inc       # non-executable directories such as /var, /tmp, and /home
#include disable-interpreters.inc       # perl, python, lua etc.
#include disable-programs.inc    # user configuration for programs such as firefox, vlc etc.
#include disable-shell.inc      # sh, bash, zsh etc.
#include disable-xdg.inc        # standard user directories: Documents, Pictures, Videos, Music

### Home Directory Whitelisting ###
### If something goes wrong, this section is the first one to comment out.
### Instead, you'll have to relay on the basic blacklisting above.
#private 
#whitelist /hab/pkgs/chef/inspec/4.56.22/20220517052126/bin/inspec
#blacklist /hab
 
#read-only /hab/pkgs/chef/inspec/4.56.22/20220517052126/bin/inspec
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
#private-bin dash,hab,inspec
#private-cache  # run with an
read-only /hab
