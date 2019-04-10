# build it with 'docker build -t cargostack/postgre .'
FROM        ubuntu:14.04.2
MAINTAINER  V

RUN mkdir -p /root/.ssh && chown root:root /root/.ssh && chmod 755 /root/.ssh
RUN echo 'ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEA6NF8iallvQVp22WDkTkyrtvp9eWW6A8YVr+kz4TjGYe7gHzIw+niNltGEFHzD8+v1I2YJ6oXevct1YeS0o9HZyN1Q9qgCgzUFtdOKLv6IedplqoPkcmF0aYet2PkEDo3MlTBckFXPITAMzF8dJSIFo9D8HfdOV0IAdx4O7PtixWKn5y2hMNG0zQPyUecp4pzC6kivAIhyfHilFR61RGL+GPXQ2MWZWFYbAGjyiYJnAmCP3NOTd0jMZEnDkbUvxhMmBYSdETk1rRgm+R4LOzFUGaHqHDLKLX+FIPKcF96hrucXzcWyLbIbEgE98OHlnVYCzRdK8jlqm8tehUc9c9WhQ== vagrant insecure public key' > /root/.ssh/authorized_keys
RUN chmod 0644 /root/.ssh/authorized_keys && chown root:root /root/.ssh/authorized_keys

RUN apt-get update && apt-get install -y openssh-server
RUN mkdir /var/run/sshd

EXPOSE 22
CMD ["/usr/sbin/sshd", "-D"]
