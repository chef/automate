# Chef UI Library

User Interface (UI) Design patterns are recurring solutions that solve common design problems. The Chef UI Library aims to have the standard patterns used in the Chef UI available for use within Engineering so we can spend more time providing delight for our customers in other areas.

Stencil is a simple compiler for generating Web Components. Web Components is an emerging standard that is designed to make it easy to create reusable UI patterns using APIs that are native to the browser.
One clear advantage of this is that the components we make here have no ties to a single library or frontend framework.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Installing

A step by step series of examples that tell you have to get a development env running

### Running just the Stencil Library

To ensure you're using the correct version of node, install [NVM](https://github.com/creationix/nvm)
and run `nvm use` from `/chef-ui-library`. It will direct you to install the correct version if necessary.

Navigate to `/chef-ui-library` and run `npm install`

```bash
npm install
```

then run `npm start` to run a live-reload server at `http://localhost:3333`

```bash
npm start
```

## Developing against Automate UI and the Hab Studio

Please see the extensive "getting started" docs for developing against the
[habitat dev env](../../dev-docs/ui-development.md) alongside a local instance of Automate UI.

### Re-Generating Documentation

Documentation that describes the atoms and molecules present within the UI library is generated from javadoc style comments using TypeDoc. Technically TypeDoc generates some json metadata that is then read by an internal Stencil component to display said atoms and molecules in the development environment.
That metadata is automatically generated when `npm start` is run. Atoms and molecules are automatically updated when their code is updated, but the documentation generation can be expensive and is therefore done on demand.
If you update any internal docs then you need to either restart the development server or run `npm run docs` to regenerate the necessary metadata.

### Running and Developing patterns within Automate UI

* Section coming soon

### The Sandbox

You will note the sandbox.html file in the src directory. This was added to help make prototyping easier. When developing your new atom, molecule or template use the sandbox as a place to test your work. DO NOT commit any of your scratch work to the repo.
To make it easier not to forget that; run `git update-index --assume-unchanged src/sandbox.html` from the root of the library.
This command tells git to ignore any changes to the file. To undo the command and make git aware of changes to the sandbox; run `git update-index --no-assume-unchanged src/sandbox.html`.

The sandbox can be viewed at [http://localhost:3333/sandbox.html](http://localhost:3333/sandbox.html)

## Running the tests

### Lint Tests

* Sass: `npm run lint.sass`
* Typescript: `npm run lint.ts`

### Unit Tests

Stencil comes with **Jest** built-in - read more about it at [Testing Section of the Stencil Site](https://stenciljs.com/docs/getting-started).

* Run once: `npm test`
* Re-run every time you save a file: `npm run test.watch`

## Built With

* [StencilJS](https://stenciljs.com/docs/intro) - component compiler
* [Jest and the Stencil](https://stenciljs.com/docs/testing) - unit testing framework
* [NPM](https://www.npmjs.com/) - package manager
