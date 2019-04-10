# Delivery Web

The awesome web interface for CHEF Delivery.

## Install

To develop on the Delivery UI you'll want to 
[set up your workstation](https://github.com/chef/delivery/README.md) 
to get a local Delivery cluster up and running and have all the tools you need to develop on 
Chef Delivery.

## Develop for Workflow UI

By default, when you run `make` the Docker Compose suite will build the Workflow UI
once. The files (html, css, etc) are synced to the local `web/dist` directory
where they are synced into the Nginx container and served from there. Once the
build is complete, that container will exit cleanly. If you want to rebuild the
the UI, you can simply run `docker-compose restart workflow_ui`.

If you are doing active development on the Workflow UI and want to see your
changes occur in real time, you'll want to use the local watchers that will
continuously update your code locally, which will get synced into the nginx
container automatically.

To install the needed UI dependencies ensure you're in the `web/` directory (this one) and 
run the below command:

    make install

You need to install npm and nvm only if you are doing active local development. 
To install Node.js. We recommend [installing NVM](https://github.com/creationix/nvm#install-script),
then using that to install whatever version of Node is currently in use by the
project. This updates every so often, so the best way to find this out is to
look at the version of node-js container we use in the docker-compose.yml file.

Once NVM is installed, change to the `web` directory and run `nvm install`. This
will install all the dependencies required to build and run the Workflow UI.
There are a lot of dependencies to download, so make sure you're on a strong
internet connection.

Still in the `web` directory, run `make startdev`. That will install all
dev-related Node.js dependencies, run a build, and start the CSS and JS watchers,
which will compile to the `web/dist` directory on change. Your changes should be
reflected in the Workflow UI within a few seconds of saving.

In the `infra` directory:

    # Auto-sync built assets with local delivery server
    make auto_sync_delivery

Navigate to the UI in your favorite browser: `http://192.168.33.66`.

## Test

Currently, there are two types of tests for the Delivery UI:

### Unit 

Tests which load individual components into the browser and test their behavior in isolation. Unit specs are defined within [Jasmine](http://jasmine.github.io/2.3/introduction.html) and executed by [Karma](http://karma-runner.github.io).

### E2E 

Tests which load the full application into the browser within a mocked server environment and test the application behavior from the perspective of a user. E2E specs are defined within [Jasmine](http://jasmine.github.io/2.3/introduction.html) and executed by [Protractor](http://www.protractortest.org).

Ensure you have needed dependencies:

    make install

To run both unit and e2e tests:

    make test

To run unit tests:

    make unit

To run unit tests continuously during development:

    make unit-debug

To run e2e tests:

    make e2e

To run e2e tests in debug mode during development:

    make e2e-debug

See [testing docs](https://github.com/chef/delivery/blob/master/web/doc/testing.md) for more information on using tests during development and 
how to debug tests while writing them.

#### Browserstack

To run tests in Browserstack, you'll need to store the username and access key for your account in environment variables before running the tests.

To run both unit and e2e tests:

    BROWSERSTACK_USER=johndoe BROWSERSTACK_KEY=8f8ajj22af88 make test-stack

To run unit tests:

    BROWSERSTACK_USER=johndoe BROWSERSTACK_KEY=8f8ajj22af88 make unit-stack

To run e2e tests:

    BROWSERSTACK_USER=johndoe BROWSERSTACK_KEY=8f8ajj22af88 make e2e-stack

## Other Tools

[raml2html](https://www.npmjs.com/package/raml2html)

    npm install -g raml2html
