# Automate 2 UI

This page is an overview of all things Automate 2 (A2) UI and it's development.

### Table of Contents
[What It Is](#what_it_is)

[How to Develop in automate-ui](#dev)

[Directory structure](#structure)

[Important files](#important)

## <a id="what_it_is"></a>What It Is
The Automate-UI repo is where all the front end code for Automate 2.0 and above lives. It does not include any workflow code, or anything that was EOL'd in `Automate 1.X.`
At the switch to 2.0, the Angular-CLI tool was added to the code base to simplify the development experience. 

## <a id="dev"></a>How to Develop in automate-ui
The README gives comprehensive directions on how to start the dev server and use the Angular-CLI tool.

## <a id="structure"></a>Directory structure
As part of the move, we have flattened out most of the directory structure to remove nested components.

Nested components created a more complex, less declarative developer experience where it was unclear where components lived and how to interact with them; it also led to code duplication. 

Currently, the structure is (aspirationally) as follows:

### View containers:
```
src/app/pages/$VIEW-NAME/
```
notes: currently, the entirety of compliance is nested under here. this is because the components and routing were tightly coupled in ways that would have been difficult to untangle in our Angular-CLI migration timeframe. We are planning on moving forward to flatten this part of the application apart, and would request that folks try to stick to the structure being detailed here rather than putting more code/components inside the `+compliance/` directory.

### Components of those views:
```
src/app/components/$COMPONENT-NAME/
```
### services:
```
src/app/services/$SERVICE-NAME/
```
### testing helpers:
```
src/app/testing/$TESTING-FILE.ts
```
### types: custom types
```
src/app/types/$NAMESPACE-TYPES.types.ts
```
notes: currently a lot of our types live in one file just called `types.ts` – that is not ideal, and we need to break that apart

### data: data processing
```
src/app/data/$DATA-TO-BE-PROCESSED.ts
```
notes: this directory is for functions related to processing/transforming data coming in from http requests before they are inserted into the state.

### directives: components without a view piece
```
src/app/directives/$DIRECTIVE-NAME/
```

### helpers:
```
src/app/helpers/$HELPER-NAME/
```
### pipes: pipes are used for filtering data in the markup.
```
src/app/pipes/$PIPE-NAME.ts
```
### requests: all api request functions should ultimately go through functions in this directory
```
src/app/requests/$DESCRIPTIVE-REQUEST-NAME.ts
```

## <a id="important"></a>Important files
```
./app-routing.module.ts
```
this is our routing module. all new routes should be added here.
*note:* currently, there is a compliance routing module. we are hoping to remove that to eliminate some obfuscation of code in the coming weeks.
```
./app.module.ts
```
this is where all the components, pages, services, pipes, and modules are made available to the application.
In this file, the @NgModule decorator (on top of the AppModule class) takes in metadata consisting of those elements.

The metadata consists of:

*imports:* the declarations of external libraries and other local modules (in our case, only the ComplianceModule is a "local" import)

*declarations:* makes directives (components, pipes) available to all the other directives in the application.

*providers:* services and other values needed for dependency injection (DI).

For more info, see the official Angular docs.
