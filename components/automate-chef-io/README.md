# Automate 2.X - Website

A new static website for Automate is based on [Hugo](https://gohugo.io), the Go-powered static site generator.

## Getting Started

### One Time Install

1. Disable smart quotes. OS X tries to use smartquotes by default when editing Hugo docs. You want smartquotes disabled regardless if you are writing code. Here's how [you can disable it](http://www.iclarified.com/38772/how-to-disable-curly-quotes-in-mac-os-x-mavericks).

1. [Install Hugo](https://gohugo.io/getting-started/installing/). If you're using homebrew:

    ```shell
    brew install hugo
    ```

1. [Install go-swagger](https://goswagger.io/install.html). The currently supported version is 0.20.1. If you're using homebrew:

    ```shell
    brew tap go-swagger/go-swagger && brew install go-swagger
    ```

    If everything is up to date you should see output like this when running `swagger version`:

    ```shell
    $ swagger version
    version: v0.20.1
    commit: 9004771f77bedabb792a48dc846ba7fc800398a0
    ```

1. [Install jq](https://stedolan.github.io/jq/)

    ```shell
    brew install jq
    ```

### Development

1. From the `components/automate-chef-io` directory, run the following command to start the development server.

    ```shell
    $ make serve
    ```

    When this is running, any changes you make to content in the `components/automate-chef-io` directory will be automatically compiled and updated the browser.

    To view the docs in your browser, navigate to:

    ```shell
    http://localhost:1313/docs/
    ```

## Creating and Changing Docs

The site is built with [Hugo](https://gohugo.io/), a Go-based static site generator, and uses [Stencil](https://stenciljs.com/) for web components and [Sass](http://sass-lang.com/) for CSS. You'll probably want to familiarize yourself with the Hugo documentation, which covers templating, layouts, functions, etc., but there are helpers to assist you with doing some common things, like creating a new blog post:

```shell
hugo new docs/my-sweet-new-page.md
```

Your new page will be created as a draft with enough [frontmatter](https://gohugo.io/content-management/front-matter/) to get you going. All content is authored in [Markdown](https://en.wikipedia.org/wiki/Markdown).

## Helpers

make sync
: Pull down the current chef hugo theme

make serve
: Start the live-reload development environment

> If you get a `git clone` related failure when running these commands, you may not have your HTTPS GitHub credentials setup. These commands will clone down the hugo theme via HTTPS. [Read these docs](https://help.github.com/articles/which-remote-url-should-i-use/#cloning-with-https-urls-recommended) to learn why we use HTTPS over SSH, and how to setup your GitHub credentials.

## Markdown Content

Please try to keep all your documentation in the `docs` directory. Any new documents should automatically show up in the sidebar. If you need to control the ordering of the sidebar, you can add `weight` to the frontmatter of the documents.

To add a new Markdown file, simply run the following command from the `components/automate-chef-io` directory:

```shell
hugo new docs/<filename>.md
```

This will create the file and fill in the necessary frontmatter automatically.

## Data Content

Hugo allows us to nest our data directory structure as much as necessary. You can add as many folders as necessary under `components/automate-chef-io/data/docs`.

```
.
├── data
│   ├── docs
│   │   ├── cli
|   │   │   ├── command_one.yml
|   │   │   └── command_two.yml
```

## API Documentation

To view the API documentation locally, 1. From the `components/automate-chef-io` directory, run the following command to start the development server.

    ```shell
    $ make serve
    ```

    To view the API docs in your browser, navigate to:

    ```shell
    http://localhost:1313/docs/api
    ```

## Developing API Docs

We use the [ReDoc](https://github.com/Redocly/redoc) in conjunction with the the Hab studio for Automate, [grpc-gateway protoc-gen-swagger](https://github.com/grpc-ecosystem/grpc-gateway/tree/master/protoc-gen-swagger) and the [OpenAPI 2.0 specification](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md) to automatically generate API documentation from comments on automate protoc files.

## Writing API Docs

To develop API documentation, compile go components, sync the swagger, files and serve the development environment locally, run the following from the top level `automate` directory.

To see your changes, you will need to re-run this command to re-build the documentation.

    ```shell
    hab studio run "source .studiorc && compile_all_protobuf_components" && pushd components/automate-chef-io/ && make sync_swagger_files generate_swagger && make serve || popd
    ```

    View the API docs in your browser:

    ```shell
    http://localhost:1313/docs/api
    ```

The proto files for APIs are located at:

```shell
automate/components/automate-gateway/api/
```

You can find the examples from these instructions at:

```shell
automate/components/automate-gateway/api/compliance/reporting/reporting.proto
```

Adding comments to a proto file uses Go inline and block comment syntax:

```go
/* This is a block comment */
// This is an inline comment
```

Block comments are usually placed before the start of an RPC service definition. Block comments for API documentation are sometimes called "leading comments".

Inline are usually placed within the message formats. Inline comments for API documentation are sometimes called "following comments".


    * Installation of necessary things
    * Commands to run
    * TODO: is there a "getting started with automate" I can link to for this?
    * Don't fix conflicts in generated files, regenerate

* Docs Styleguide

    * Service title and description
    * Field description
    * Mention object reference gotcha
    * Grammar choices? ie, 1st vs 3rd person, etc
    * Tabs preferred over spaces

* Extra things

* References / relevant links

    * https://github.com/OAI/OpenAPI-Specification/blob/3.0.0/versions/2.0.md#schemaObject
    * https://github.com/grpc-ecosystem/grpc-gateway/blob/master/examples/proto/examplepb/a_bit_of_everything.proto
    * https://github.com/Redocly/redoc/blob/master/docs/redoc-vendor-extensions.md#x-displayname
    * https://swagger.io/docs/specification/2-0/basic-structure/
