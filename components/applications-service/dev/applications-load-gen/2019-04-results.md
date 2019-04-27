EAS/Applications Performance Test April 2019
============================================

Results
-------

One modification was made to the application service ingestion code to allow for multi-threaded processing of incoming messages. A few modifications were necessary to enable the load generator to successfully create thousands of connections to the event-gateway's NATS instance. With these modifications in place, the system was capable of handling approximately 650 messages per second at a steady state. Higher loads of up to 800 messages per second were applied; in these cases the messages were accepted and buffered by the nats-streaming system.

Discussion
----------

* A message rate of 600 messages per second corresponds to 18,000 services with a 30s healthcheck interval.
* To reserve some headroom, we recommend no more than 15,000 services if using an equivalent configuration.
* Actual performance may depend greatly on disk IO performance, so could be lower on systems with less performant hardware.
* Engineers at Chef Software have observed significant operational issues when buffers grow large; as a result of this work, the maximum number of outstanding messages has been set to 100,000. This provides the following behaviors:
  * At a processing rate of 500 messages/s, the backlog can be cleared in less than 5 minutes if load is removed at the front-end.
  * If the system has 100 messages/s of headroom, the backlog can be cleared in less than 25 minutes.


System Configuration
--------------------

Testing was performed in AWS EC2, Oregon region.

Existing documentation for Chef Automate v1 recommends a system with 4(v)CPUs and 16GiB of RAM as a minimum for a production system. Some performance testing work I had previously done suggested 264GiB as a reasonable size for the root EBS volume. The Instance was configured as follows:

* OS: Amazon Linux 2
* Instance Type: m5.xlarge
  * vCPU: 4
  * RAM: 16 GiB
* Root EBS Volume:
  * 264 GiB
  * 792 IOPS (default for a volume this size)
* Only port 22 was exposed via security group; other ports accessed via tunneling

OS Setup
--------

Package installation:

```
yum update -y
yum install curl tmux git htop -y
```

Update sysctl settings for Chef Automate:

```
vim /etc/sysctl.conf
## add this:
# vm.max_map_count=262144
# vm.dirty_expire_centisecs=20000
## Then run this:
sysctl -f /etc/sysctl.conf
```

Automate was installed according to the instructions, with the following modifications:
* `channel`: `"dev"` -- to get the latest builds
* `es heapsize`: `"8g"` -- EAS doesn't use Es, but this would be typical for this system configuration
* Set `override-origin` and `hartifacts` dir -- to side-load dev packages with fixes

After initial deployment of Chef Automate, the following additional configuration was done:
* apply the license key used by Chef developers
* `chef-automate applications enable` -- Enable applications features on the backend.
* `chef-automate dev enable-prometheus` -- enables metrics collection from Automate

With this configuration, it's useful to give the following options to ssh:
* `-L 10109:localhost:10109` -- access Automate's prometheus via http://localhost:10109
* `-L 443:localhost:443` -- access Automate's UI. This requires `ssh` to be root on the client side, so you may need to pass `-i /path/to/my/id_rsa` as well.

ULIMIT: Even when running as root, the system limits the number of open files allowed by default. In order to create the number of client connections required to simulate larger loads, it is necessary to increase this limit:
`ulimit -n 102400`.

Load Test Configuration
-----------------------

Configuration was based on the `noms.toml` configuration from the `dev/applications-load-gen` directory. This configuration creates two supervisor configurations with three services each. The total count of simulated supervisors was set to different values throughout the test to explore the system's capability to handle various amounts of load. One particularly valuable observation was to compare the message rate as measured by the load generator to that reported by the applications service. This helped to detect when nats-streaming began to buffer messages.

