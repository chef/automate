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

Next set your environment variables. See "Working with Secrets" in
dev-docs/DEV_ENVIRONMENT.md for details on setting up vault if you
don't already have `../dev/secrets-env.sh`:

```bash
source ../dev/secrets-env.sh
```

Next, specify the Automate instance you wish to target. This could be
dev or acceptance; typically start with your local box with this:

```bash
export CYPRESS_BASE_URL=https://a2-dev.test
```

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

### IAM Version

Some tests will conditionally run in CI depending on what `CYPRESS_IAM_VERSION` is set to.
To target those specific sets of tests set that environment variable when starting cypress:

```bash
CYPRESS_IAM_VERSION="v2.1" npm run cypress:run
```

You might see failures if your dev environment doesn't match.

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

## Helpful Links

[Best Practices Talk by Founder Brian
Mann](https://www.youtube.com/watch?v=5XQOK0v_YRE)

[Documentation](https://docs.cypress.io/guides/overview/why-cypress.html#In-a-nutshell)
