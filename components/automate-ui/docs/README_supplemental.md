This page provides supplementary information for Automate-UI.
See the main [README](README.md) for the larger picture.

# Sections
* [Config Management Service Notes](#config-mgmt-service-notes)
* [Compliance Service Notes](#compliance-service-notes)

# Config Management Service Notes

Always refer to the [Config Management Service repo](https://github.com/chef/automate/tree/master/components/config-mgmt-service)
for the most up-to-date information that may supercede anything here at any time :-)

## Introduction

Once you pull down the config-mgmt service repository and setup its prerequisites as described
in the **README.md** for the service, navigate to its directory.
If you have `direnv` wired up correctly you will see several environment variables
beginning with **HAB** being exported:

```
$ cd config-mgmt-service
direnv: loading .envrc
direnv: export +HAB_DOCKER_OPTS +HAB_ORIGIN +HAB_UPDATE_STRATEGY_FREQUENCY_MS
```

The most common reason that the env variables fail to appear is that
the `direnv` setup (`eval "$(direnv hook bash)"`) really needs to be the _last_ item
in your **.bashrc** or **.bash_profile**.

The sequence of steps to stand up the config-mgmt service--as revealed by the `dev_help` command
in **habitat studio**--is just:
```
# setup_dev_environment
# build
# start
```

Note that you will still have no data in the service yet;
how to do that is described near the end of this page.

## The Build Step

A successful `build` should conclude with something like this:
```
. . .
» Signing config-mgmt-service_blake2bsums
☛ Signing config-mgmt-service_blake2bsums with MyUserName-20170905215937 to create /hab/pkgs/MyUserName/config-mgmt-service/0.1.0/20170906171418/FILES
★ Signed artifact /hab/pkgs/MyUserName/config-mgmt-service/0.1.0/20170906171418/FILES.
   config-mgmt-service: Generating package artifact
. . .
★ Signed artifact /hab/cache/artifacts/MyUserName-config-mgmt-service-0.1.0-20170906171418-x86_64-linux.hart.
'/hab/cache/artifacts/MyUserName-config-mgmt-service-0.1.0-20170906171418-x86_64-linux.hart' -> '/src/results/MyUserName-config-mgmt-service-0.1.0-20170906171418-x86_64-linux.hart'
   config-mgmt-service: hab-plan-build cleanup
   config-mgmt-service:
   config-mgmt-service: Source Path: /src
   config-mgmt-service: Installed Path: /hab/pkgs/MyUserName/config-mgmt-service/0.1.0/20170906172232
   config-mgmt-service: Artifact: /src/results/MyUserName-config-mgmt-service-0.1.0-20170906172232-x86_64-linux.hart
   config-mgmt-service: Build Report: /src/results/last_build.env
   config-mgmt-service: SHA256 Checksum: d3136a649d9c91cd78c55fd879fbdf0d0ae006dc11294cf0cf4fc80f394efb98
   config-mgmt-service: Blake2b Checksum: 5a425312ec68413ab9af1ec51c92992f2e3b117b4a59495178a37a9850f62be3
   config-mgmt-service:
   config-mgmt-service: I love it when a plan.sh comes together.
   config-mgmt-service:
   config-mgmt-service: Build time: 0m57s
[default:/src:0]#
```

Besides the above log output you can confirm that the build was successful
by invoking the actual service binary directly.
From the log trail above, find the line beginning with **Signed artifact** and ending with **FILES**.
Take that path and replace the leaf **FILES** with **bin**.
Navigate to that directory, where there should be a `config-mgmt-service` executable and just run it.
This should result in a brief **man page** for the service.
So _/hab/pkgs/MyUserName/config-mgmt-service/0.1.0/20170906171418/**FILES**_
becomes _/hab/pkgs/MyUserName/config-mgmt-service/0.1.0/20170906171418/**bin**_ and then:
```
[default:/src:0]# cd /hab/pkgs/MyUserName/config-mgmt-service/0.1.0/20170906171418/bin
[default:/src:0]# ./config-mgmt-service

NAME:
   config-mgmt-service - config-mgmt API Service

USAGE:
   config-mgmt-service [global options] command [command options] [arguments...]

VERSION:
   0.1.0

COMMANDS:
     start    Starts the config-mgmt API service
     help, h  Shows a list of commands or help for one command

GLOBAL OPTIONS:
   --help, -h     show help
   --version, -v  print the version

COPYRIGHT:
   Copyright 2017, Chef Software Inc.
```
## Starting the config-mgmt service

What you should see when you start the config-mgmt service is something like this:
```
[default:/src:0]# start
hab-sup(MN): Supervisor starting MyUserName/config-mgmt-service. See the Supervisor output for more details.
 => Waiting to load svc MyUserName/config-mgmt-service
 => Waiting to load svc MyUserName/config-mgmt-service
. . .
» Applying configuration for config-mgmt-service.default incarnation 1504718624
Ω Creating service configuration
✓ Verified this configuration is valid TOML
↑ Applying to peer 127.0.0.1:9638
★ Applied configuration
 => Waiting for port 1234 to be listening
 => Waiting for port 1234 to be listening
[default:/src:0]#
```

To troubleshoot starting the service, it may be helpful to monitor the supervisor log
(`sup-log` or `sl` for short) while the service attempts to start up.
It is cleaner to do this in a separate window, of course,
but that means you would be outside **habitat studio**.
`sup-log` just runs `tail` on the supervisor's log file and this can easily be done
externally with just one extra step.
First, identify the name of your docker instance (right-most column below).
Note that this name is a random string, and is regenerated each time you start the config-mgmt service.

```
$ docker ps
CONTAINER ID  IMAGE                     COMMAND                 CREATED         STATUS      PORTS                      NAMES
b1fd6b4d036d  habitat.../studio:0.30.2  "/bin/hab studio e..."  10 minutes ago  Up 10 mins  0.0.0.0:1234->1234/tcp...  determined_ritchie
```

Supply the above name to this next command to see the supervisor log outside of habitat studio:
```
$ docker exec -it determined_ritchie tail -f /hab/studios/src/hab/sup/default/sup.log

→ Using core/libsodium/1.0.11/20170514010744
→ Using core/linux-headers/4.3/20170513200956
→ Using core/openssl/1.0.2j/20170513215106
→ Using core/xz/5.2.2/20170513214327
→ Using core/zlib/1.2.8/20170513201911
✓ Installed core/hab-launcher/4571/20170727193541
★ Install of core/hab-launcher/4571/20170727193541 complete with 1 new packages installed.
hab-sup(MR): Supervisor Member-ID 6b15ce72b3664e8f9b8d13174b83e704
hab-sup(MR): Starting gossip-listener on 0.0.0.0:9638
hab-sup(MR): Starting http-gateway on 0.0.0.0:9631
hab-sup(PK): Confirmed latest version of core/elasticsearch is core/elasticsearch/5.5.0/20170726170852
hab-sup(MR): Starting core/elasticsearch
hab-sup(PK): Confirmed latest version of core/elasticsearch is core/elasticsearch/5.5.0/20170726170852
elasticsearch.default(SR): Hooks recompiled
. . .
hab-sup(MR): Starting MyUserName/config-mgmt-service
config-mgmt-service.default(SR): Hooks recompiled
. . .

```
Should you want to restart the config-mgmt service just stop and restart it like so:
```
[default:/src:0]# hab svc stop $HAB_ORIGIN/$pkg_name
[default:/src:0]# start
```

(Rumor has it that there will soon just be a simple `stop` command available.)

If the service is not starting up properly,
also check the running processes inside **habitat studio**.
You should see something similar to this:
```
[default:/src:0]# ps

PID   USER  TIME COMMAND
    1 root  0:00 {hab-studio} /bin/sh /hab/pkgs/core/hab-studio/0.30.2/20170822233239/bin/hab-studio enter
  132 root  0:00 /hab/pkgs/core/bash/4.3.42/20170513213519/bin/bash --login +h
 3766 root  0:00 /hab/pkgs/core/bash/4.3.42/20170513213519/bin/bash --login +h
 3784 root  0:01 /hab/pkgs/core/hab-launcher/4571/20170727193541/bin/hab-launch start core/elasticsearch
 3787 root  0:11 /hab/pkgs/core/hab-sup/0.30.2/20170822230509/bin/hab-sup start core/elasticsearch
 3855 hab   0:52 /hab/pkgs/core/jre8/8u131/20170622181030/bin/java -Xms1g -Xmx1g -XX:+UseConcMarkSweepGC
 5144 hab   0:00 config-mgmt-service start --port 1234 --host 0.0.0.0 --backend elasticsearch --elasticsearch
 5294 root  0:00 ps
```
If not, you could try rerunning the setup command (`setup_dev_environment`) then invoking `start`.
(You do not need to rerun the `build` step if you have not left **habitat studio**.)

Once the config-mgmt service has successfully started you should be able to ping it:

```
$ curl http://localhost:1234/version
{"SHA":"9703cd1","built":"2017-09-06_17:22:49","name":"config-mgmt-service","version":"0.1.0"}
```

## Generating Sample Nodes

There will, however, be no nodes available with that minimum setup.
Add this next command to generate 100 nodes with a random distribution of statuses:
```
[default:/src:0]# generate_elasticsearch_data
. . .
wrote node name Qui-quis-illum with id 97b5f6bb-cf7d-4f18-966d-1f70c94acd7e
wrote node name Consectetur-nisi-possimus with id 76b2c016-e19e-4bd1-83d5-e0b636c4c864
wrote node name facere-saepe-esse with id 9d3c5159-7d57-486a-a248-8823474b9edc
wrote node name Est-possimus-qui with id 49871fc7-b520-415e-a87b-d107640ba934
wrote node name totam-nobis-at with id 6bdddcce-0086-4b43-b2dc-a6379b0cdb3f
wrote node name accusamus-quo-et with id 0f56463b-48fc-43c9-912f-55cba36d6103
. . .
```

(Note that this command should be run after the build but it could be before or after
you start the service.)

You can then query further:

```
$ curl http://localhost:1234/stats/node_counts
{"success":35,"failure":32,"missing":33,"total":100}
```
