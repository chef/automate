# docker-based workflow runner setup

This is what is currently used for setting up a runner in the docker-based
development environment. It's supposed to be

a) a job runner accessible via SSH, and
b) close enough to dazzle-installed build nodes to use delivery-cmd.

The `make register_runner` target of the top-level Makefile will take care of
creating a runner in the workflow_api, and install the public key of the key
pair generated in the process in this container's job_runner user's
~/.ssh/authorized_keys.

## Chef client config

The `client.rb` in this repository goofily points at the runner's private key,
which is only to be used for accessing the git repository using the git_ssh
wrapper. However, the requests' signatures are never verified, so there's little
harm in this hack.

## Merely copied files

`delivery-cmd` and `git-ssh-wrapper` are copied as-is from the `omnibus/`
top-level directory, otherwise the docker build context would have to be the
entire repository. If you try to copy from `../../omnibus` then you get the
Docker error `Forbidden path outside the build context`.
