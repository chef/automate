FROM devchef/node-5.6:latest
MAINTAINER Chef Software, Inc <docker@chef.io>

COPY Makefile package.json .babelrc tsconfig.json /usr/src/app/web/
COPY config /usr/src/app/web/config
COPY src /usr/src/app/web/src
COPY dist /usr/src/app/web/dist
WORKDIR /usr/src/app/web
RUN make install

CMD [ "make", "build" ]
