FROM solita/ubuntu-systemd:16.04
MAINTAINER Chef Software <ryan@chef.io>

ENV DEBIAN_FRONTEND noninteractive

# These lines are copy-pasta from the devchef/chefdk Dockerfile. We can't use
# that image b/c we need systemd in order to install/upgrade to a2
RUN apt-get update && apt-get install -y wget curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ARG HAB_ORIGIN
ENV HAB_ORIGIN ${HAB_ORIGIN}

EXPOSE 443
