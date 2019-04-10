FROM devchef/node-5.6:test
MAINTAINER Chef Software, Inc <docker@chef.io>

COPY Makefile package.json .babelrc tsconfig.json /usr/src/app/web/
WORKDIR /usr/src/app/web
RUN make install
