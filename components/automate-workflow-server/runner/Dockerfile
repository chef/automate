FROM devchef/chefdk:latest
MAINTAINER Chef Software, Inc. <docker@chef.io>

# based on
# - https://github.com/chef/chef-compliance/blob/master/core/test/containers/ubuntu/Dockerfile

# openssh daemon + sudo
RUN apt-get update && apt-get install curl git openssh-server sudo -y
RUN mkdir /var/run/sshd
EXPOSE 22
CMD ["/usr/sbin/sshd", "-D"]

# used for registering with workflow
ADD server/runner/register.sh /usr/app/register.sh
RUN chmod +x /usr/app/register.sh

# what install-build-node executes on a builder (create builder user, commands, workspace)
ADD automate-ctl/lib/installer/install-build-node.sh install-build-node.sh
ADD server/etc/builder_key builder_key
ADD server/runner/client.rb /etc/chef/client.rb

# install-build-node.sh and what it expects
RUN mkdir -p /etc/chef/trusted_certs
RUN touch delivery.pem config.rb /etc/chef/trusted_certs/none.crt /etc/chef/client.pem /etc/chef/client.rb
ADD automate-ctl/lib/installer/git-ssh-wrapper git-ssh-wrapper
ADD automate-ctl/lib/installer/delivery-cmd delivery-cmd
RUN bash install-build-node.sh

# cleanup
RUN rm builder_key delivery.pem config.rb git-ssh-wrapper delivery-cmd

# job_runner ssh login user
ADD automate-ctl/lib/installer/install-job-runner.rb install-job-runner.rb
RUN chef-apply install-job-runner.rb
