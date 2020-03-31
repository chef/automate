# Cypress Testing

Cypress is a tool for complete end-to-end testing.

Cypress is ideal for testing any interactions with the Chef Automate
UI. This includes anything from logging in to filling out a form to
create an Automate resource.

We are using Cypress to add some automation to our acceptance testing
process. On each deploy to dev/acceptance, Cypress runs the tests in
`/integration` against each dev/acceptance environment and records the
results in our [Automate Acceptance Testing Cypress
dashboard](https://dashboard.cypress.io/#/projects/yvg8zo/runs).

## Running Cypress Locally

First install cypress if you haven't before.

```bash
cd automate/e2e
npm install
```

There are two different helpers you can run,
depending on if you want to run the tests marked as flaky:

```
npm run cypress:local
npm run cypress:local:flaky
```

These commands are simply setting the correct environment variables to run cypress locally.
See below for more details.

### Environment Variables

Next set your environment variables. See "Working with Secrets" in
dev-docs/DEV_ENVIRONMENT.md for details on setting up vault if you
don't already have `../dev/secrets-env.sh`:

```bash
source ../dev/secrets-env.sh
```

If your secrets become out of date, you'll likely need to re-fetch
`secrets-env.sh` and re-source. You might get a strange error like
this if your secrets are out of date:

```bash
InvalidCharacterError: Failed to execute 'atob' on 'Window': The string to be decoded is not correctly encoded.
```

Next, specify the Automate instance you wish to target. This could be
dev or acceptance; typically start with your local box with this:

```bash
export CYPRESS_BASE_URL=https://a2-dev.test
```

You must also set whether or not to run the flaky tests and optionally the admin token:

```bash
export CYPRESS_RUN_FLAKY=yes || no
export CYPRESS_ADMIN_TOKEN=<admin token value> # optional
```

The admin token does **not** need to be set beforehand. Cypress runs [the support file](https://docs.cypress.io/guides/core-concepts/writing-and-organizing-tests.html#Support-file) `index.ts` before all tests.
If `CYPRESS_ADMIN_TOKEN` hasn't been set, this block of code will take care of generating a new admin token and setting its value to the environment variable.

### Opening Cypress

If you are developing Cypress tests, you'll want to open the Cypress
app so you can watch the browser as the tests run. The Cypress tool is
great for debugging. Make sure you're still in the `e2e` directory of
Automate:

```bash
npm run cypress:open
```

If you'd like to only get test results, you can run the tests in the
terminal with Cypress in the background.

```bash
npm run cypress:run
```

You can test against dev/acceptance environments as well. Just be
sure to leave off the trailing `/`.

## Writing Cypress Tests

This [Cypress guide](https://docs.cypress.io/guides/getting-started/writing-your-first-test.html#Add-a-test-file)
provides an introduction on writing your first Cypress test.

Automate's Cypress tests are divided into two main categories: UI and API tests,
under the `ui` and `api` directories respectively.

API tests communicate only with the backend using `cy.request`. The `api/` sub-directories
correspond to the sub-directories under `automate-gateway/api`. When adding new API tests,
you may need to add the appropriate sub-directory if it doesn't exist yet.

UI tests are end-to-end tests that interact with the UI backed by a running Automade backend.
The `ui/` sub-directories correspond to the top navbar links, Event Feed, Infrastructure, Compliance,
and Settings. When adding new UI tests, you may need to add the appropriate sub-directory if it doesn't exist yet.

The last sub-directory is `common`, for testing UI interactions, such as logging in,
that don't fall into the above categories.

### Running Flaky Tests

There are some known flaky tests we are trying to debug. You must set whether or not you wish for them to run:

```bash
CYPRESS_RUN_FLAKY=yes npm run cypress:run
```

The tests are marked as flaky by the `itFlaky` descriptor instead of the `it` descriptor.

## Running Cypress pipeline tests

As part of our [deploy
pipeline](https://github.com/chef/automate/blob/master/.expeditor/deploy.pipeline.yml),
we run Cypress tests every time we deploy a new build to `dev` or
`acceptance`. You can see latest builds on [this buildkite
dashboard](https://buildkite.com/chef/chef-automate-master-deploy-dev/builds).

Your Cypress tests may be passing locally, but it's a good idea to
check their stability when pointed at `dev` and `acceptance`
environments.

To do so, follow these steps.

1. Log on to a docker container (using the same image buildkite uses),
   using your local Automate as the working directory.

   ```bash
   docker run -it --rm --volume $(PWD):/workdir --workdir /workdir chefes/releng-base bash
   ```

1. Set your environment variables. See "Working with Secrets" in
   dev-docs/DEV_ENVIRONMENT.md for details on setting up vault if you
   don't already have `../dev/secrets-env.sh`:

   ```bash
   source ../dev/secrets-env.sh
   export CHANNEL=acceptance # or dev
   ```

1. Run `.expeditor/buildkite/cypress.sh` to start running the tests
   against all the selected `CHANNEL` environments.

## Cypress Selector Scope

In general, we follow the [cypress best practices](https://docs.cypress.io/guides/references/best-practices.html#Selecting-Elements)
around selecting elements, mainly using `data-cy` to select elements specifically.
However, be careful when using `data-cy` in a shared stencil or angular component.
If you use it in a shared component that shows up more than once on the page,
unexpected results might occur.

## Helpful Links

[Best Practices Talk by Founder Brian
Mann](https://www.youtube.com/watch?v=5XQOK0v_YRE)

[Documentation](https://docs.cypress.io/guides/overview/why-cypress.html#In-a-nutshell)
