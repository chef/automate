FROM ubuntu:18.04
MAINTAINER Chef Software <dan@chef.io>

ENV DEBIAN_FRONTEND noninteractive

# These lines are copy-pasta from the devchef/chefdk Dockerfile. We can't use
# that image b/c we need systemd in order to install/upgrade to a2
RUN apt-get update && apt-get install -y wget curl ssh build-essential
RUN apt-get update && /usr/bin/apt-get install lsb-release net-tools cron hostname less -y

RUN useradd -U -M hab

ENV HAB_LICENSE=accept

RUN curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh | bash -s -- -c unstable && \
    hab pkg install core/hab-sup -c unstable && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ENV PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

#CMD ["bash", "-c", "exec /bin/hab sup run"]
CMD ["/bin/bash"]
