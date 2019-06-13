FROM ubuntu:18.04
MAINTAINER Chef Software <docker@chef.io>

ENV DEBIAN_FRONTEND noninteractive

# Not all of these are strictly needed to install and run hab, but they should
# make it a little easier to debug things if you find a problem
RUN apt-get update && apt-get install -y wget curl ssh build-essential
RUN apt-get update && /usr/bin/apt-get install lsb-release net-tools hostname less -y

RUN useradd -U -M hab

ENV HAB_LICENSE=accept

RUN curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh | bash && \
    hab pkg install core/hab-sup/0.83.0-dev -c unstable && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ENV PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

CMD ["/bin/bash"]
