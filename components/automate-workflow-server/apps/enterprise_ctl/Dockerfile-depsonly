FROM devchef/erlang-18:latest
MAINTAINER Chef Software, Inc <docker@chef.io>

COPY rebar.config rebar.config.script rebar Makefile concrete.mk /usr/src/app/
WORKDIR /usr/src/app/
RUN make .concrete/DEV_MODE compile
