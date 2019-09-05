FROM solita/ubuntu-systemd:16.04
MAINTAINER Chef Software <ryan@chef.io>

ENV DEBIAN_FRONTEND noninteractive

RUN sed -i 's/archive.ubuntu.com/us-west-2.ec2.archive.ubuntu.com/' /etc/apt/sources.list

# These lines are copy-pasta from the devchef/chefdk Dockerfile. We can't use
# that image b/c we need systemd in order to install/upgrade to a2
RUN apt-get update && apt-get install -y wget curl ssh build-essential && \
    curl -L https://omnitruck.chef.io/install.sh | bash -s -- -c stable -P chefdk -v 3.4.38 && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ENV PATH=/opt/chefdk/bin:/opt/chefdk/embedded/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

ENV API_FQDN a1-migration.test

ARG HAB_ORIGIN
ENV HAB_ORIGIN ${HAB_ORIGIN}

ARG SERVER_CHANNEL
ARG SERVER_VERSION
ENV SERVER_CHANNEL ${SERVER_CHANNEL:-stable}
ENV SERVER_VERSION ${SERVER_VERSION:-latest}

ARG AUTOMATE_CHANNEL
ARG AUTOMATE_VERSION
ENV AUTOMATE_CHANNEL ${AUTOMATE_CHANNEL:-stable}
ENV AUTOMATE_VERSION ${AUTOMATE_VERSION:-latest}

VOLUME /a1-migration

RUN apt-get update && /usr/bin/apt-get install lsb-release net-tools cron hostname less -y

COPY setup.rb install.rb run.sh delivery.license linux-patch-baseline-0.3.0.tar.gz a1-es2-sample-data-2018-05-09-21-34.tgz /a1-migration/

# This installs the automate and chef-server packages, but doesn't run
# reconfigure. We can only reconfigure a fully running system because
# reconfigure will try to start services, but we cannot start services if
# systemd isn't running and `sh` is pid 1 during `docker build`
RUN chef-apply /a1-migration/install.rb

EXPOSE 443

CMD ["bash", "-c", "echo 'login and run /a1-migration/run.sh to continue'; exec /sbin/init --show-status=on --log-target=journal 3>&1"]
