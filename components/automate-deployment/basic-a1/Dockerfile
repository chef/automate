FROM solita/ubuntu-systemd:16.04
MAINTAINER Chef Software <ryan@chef.io>

ENV DEBIAN_FRONTEND noninteractive

# These lines are copy-pasta from the devchef/chefdk Dockerfile. We can't use
# that image b/c we need systemd in order to install/upgrade to a2
RUN apt-get update && apt-get install -y wget curl ssh build-essential && \
    curl -L https://omnitruck.chef.io/install.sh | bash -s -- -c stable -P chefdk -v 3.4.38 && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ENV PATH=/opt/chefdk/bin:/opt/chefdk/embedded/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

ENV API_FQDN basic-a1.test

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

VOLUME /basic-a1

RUN apt-get update && /usr/bin/apt-get install lsb-release net-tools cron hostname less -y

COPY setup.rb install.rb run.sh delivery.license linux-patch-baseline-0.3.0.tar.gz extra.rb /basic-a1/

# This installs the automate package, but doesn't run
# reconfigure. We can only reconfigure a fully running system because
# reconfigure will try to start services, but we cannot start services if
# systemd isn't running and `sh` is pid 1 during `docker build`
RUN chef-apply /basic-a1/install.rb

EXPOSE 443

CMD ["bash", "-c", "echo 'login and run /basic-a1/run.sh to continue'; exec /sbin/init --show-status=on --log-target=journal 3>&1"]
