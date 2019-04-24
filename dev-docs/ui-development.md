# Automate UI Development Process

## Develop Automate UI Locally against the Deployinate Dev Env

For more details on the development environment, read [A2 development environment](./DEV_ENVIRONMENT.md).

If this is your first time operating the Automate UI, first follow the [Automate UI setup instructions](../components/automate-ui/README.md)
followed by the [Chef UI Library setup instructions](../components/chef-ui-library/README.md) to ensure you can run both services locally.

1. Navigate to the `a2` project directory in your terminal.

1. Choose whether to use the Docker- or Vagrant-based development environment.

    1. To use the Docker-based environment:

        1. Make sure you're running a recent release of
           [Docker for Mac](https://store.docker.com/editions/community/docker-ce-desktop-mac) and that the Docker
           daemon is running.

        1. Configure Docker to use at least **4 CPUs and 8GB RAM**. (If you don't, some services may
           fail to start.) You can do this in Docker &gt; Preferences &gt; Advanced.

        1. Make sure you've made the [appropriate hosts-file entry](./DEV_ENVIRONMENT.md#docker-setup).

        1. Run `hab studio enter`.

    1. To use the Vagrant-based environment (presumably with VirtualBox; others may work, but are untested):

        1. Make sure you've made the [appropriate hosts-file entry](./DEV_ENVIRONMENT.md#vagrant-setup).

        1. Run `vagrant up && vagrant ssh -- -R 4200:localhost:4200`.

1. Build the `automate-ui-devproxy` component (`build components/automate-ui-devproxy`). This is a
   hab package with the same bind interface and package name as `automate-ui`. If you build it
   before deploying, `start_all_services` will pick it up instead of the real `automate-ui` package.
   It simply proxies back to your mac to port `4200`.

1. Start all the services `start_all_services`.

1. Start the automate-ui in the background `start_automate_ui_background`. Run `ui_logs` to see how
   the UI builds.

1. Optional step: Run `start_chef_ui_library` if you want to start and watch for modifications to the
   chef-ui-library. Open your browser and navigate to the URL `http://localhost:3334`. Run `ui_logs`
   to see how the chef-ui-library rebuilds.

1. Optional step: Run the necessary commands to bring up data in your environment,
   for example `chef_load_actions` or `chef_load_nodes` to load events in the event feed. 
   **Troubleshooting** If your event feed guitar string graph does not show up locally in the UI
   run `go_update_component config-mgmt-service`

**IMPORTANT:** _If you don't want to run the UI inside the studio, you can follow these steps
instead of using the `start_automate_ui_background` helper method._

1. Open a new tab/window in the terminal and run `cd components/chef-ui-library`. Make sure
   you are on the right version of npm/node (you can find the correct version of node in the `.nvmrc`
   file). Run `npm install`. If you were previously on the wrong version of npm/node or you built
   the UI via habitat, you'll need to `rm -rf node_modules/ && npm install`.

1. Run `npm start` to compile the latest version of the chef-ui-library components. You can view
   the library in its entirety at <http://localhost:3333>. If you are developing in FireFox,
   instead run `npm run dev --es5`.

1. Open a new tab/window in the terminal and run `cd ../components/automate-ui`. Make sure
   you are on the right version of npm/node (you can find the correct version of node in the `.nvmrc`
   file). Run `npm install`. If you were previously on the wrong version of npm/node or you built
   the UI via habitat, you'll need to `rm -rf node_modules/ && npm install`.

1. Run `npm run serve:hab` to start up the webserver.

1. Open <https://a2-dev.test/> in your browser. Log in with an email and password of `admin`
   and `chefautomate`, respectively. If you have issues with login, please clear all site data (local
   and session storage and cookies) for `https://a2-dev.test` and re-navigate back to the root of
   <https://a2-dev.test/>.

### The Development Cycle: Making Changes to the UI Code

* Changes in a file in `automate-ui` will show up in the browser as soon as you save a file.
* Changes to the `chef-ui-library` will require you to run `npm install` in `automate-ui` in order to pick up the changes.

## Re-enable Habitat-based UI Build

If you wish to go back to using the default habitat build from the `dev` channel,
do the following from the root of the A2 repo:

* `rm -rf results/<your_hab_namespace>-automate-ui*`. It will automagically pick up the latest `dev` Automate UI package!

## Test-driven Development Tools

Visual Studio Code (VS code) with [wallaby.js](https://wallabyjs.com/) is a useful TDD tool when
working on the UI. wallaby.js allows angular unit tests to be ran and results shown while editing
files. This provides a large speedup in writing unit tests with extra code coverage information added.

### Install wallaby.js

The only thing needed to install wallaby.js is adding the VS code extension. The extension is
'Wallaby.js' by 'WallabyJs.wallaby-vscode'.

### Individual Unit Tests and Code Coverage

To start the unit test analysis, press Cmd + shift + = and then select 'Start' in the menu. This may
take a few minutes to load. To check if the load is complete, open <http://localhost:51245/>. To see
your unit test results, open any *.spec.ts file in VS code (example
'event-feed-guitar-strings.component.spec'.) On the file you should see green, red, or
pink squares in the gutter of the file. These squares are the statuses of each individual expect
test. Green is for success, red fail, and pink the path to the failure. To see the code coverage
go to a *.ts file (example ‘ event-feed-guitar-strings.component.ts’). In the gutters of the file
there are green and gray squares. Here the green means the code on that line has coverage and gray
no coverage. If there is a failed test, pink squares will show the path of the failure.

### Overall Unit Tests and Code Coverage

To see the overall view of all the unit tests and code coverage of the project go
to <http://localhost:51245>. It will take a minute to load for the first time. This shows all the
test statuses and the overall code coverage.
