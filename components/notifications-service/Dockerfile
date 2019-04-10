FROM devchef/elixir:latest
MAINTAINER Chef Software, Inc. <docker@chef.io>

# Install node and npm
RUN apt-get update
RUN apt-get install -y node && apt-get install -y npm
RUN apt-get install -y inotify-tools

WORKDIR /usr/src/app
