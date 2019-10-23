# Automate 2.X - Website

A new static website for Automate is based on [Hugo](https://gohugo.io), the Go-powered static site generator.

## Getting Started

### One Time Install

1. Disable smart quotes. OS X tries to use smartquotes by default when editing Hugo docs. You want smartquotes disabled regardless if you are writing code. Here's how [you can disable it](http://www.iclarified.com/38772/how-to-disable-curly-quotes-in-mac-os-x-mavericks).

1. [Install Hugo](https://gohugo.io/getting-started/installing/). If you're using homebrew:

    ```
    brew install hugo
    ```

1. [Install go-swagger](https://goswagger.io/install.html). The currently supported version is 0.20.1. If you're using homebrew:

    ```
    brew tap go-swagger/go-swagger
    brew install go-swagger
    ```

    If everything is up to date you should see output like this when running `swagger version`:

    ```
    $ swagger version
    version: v0.21.0
    commit: 7c0fc3ee340f9d99d85573bc54a57e303a639692
    ```

1. [Install jq](https://stedolan.github.io/jq/download/). If you're using homebrew:

    ```
    brew install jq
    ```

### Development

1. From the `components/automate-chef-io` directory, run the following command to start the development server.

    ```
    $ make serve
    ```

    When this is running, any changes you make to content in the `components/automate-chef-io` directory will be automatically compiled and updated the browser.

## Creating Blog Posts and Making Docs Changes

The site is built with [Hugo](https://gohugo.io/), a Go-based static site generator, and uses [Stencil](https://stenciljs.com/) for web components and [Sass](http://sass-lang.com/) for CSS. You'll probably want to familiarize yourself with the Hugo documentation, which covers templating, layouts, functions, etc., but there are helpers to assist you with doing some common things, like creating a new blog post:

```shell
hugo new blog/my-sweet-new-post.md
```

Your new post will be created as a draft with enough [frontmatter](https://gohugo.io/content-management/front-matter/) to get you going. All content is authored in [Markdown](https://en.wikipedia.org/wiki/Markdown).

## Helpers

make sync
: Pull down the current chef hugo theme

make serve
: Start the live-reload development environment

> If you get a `git clone` related failure when running these commands, you may not have your HTTPS GitHub credentials setup. These commands will clone down the hugo theme via HTTPS. [Read these docs](https://help.github.com/articles/which-remote-url-should-i-use/#cloning-with-https-urls-recommended) to learn why we use HTTPS over SSH, and how to setup your GitHub credentials.

## Markdown Content

Please try to keep all your documentation in the `docs` directory. Any new documents should automatically show up in the sidebar. If you need to control the ordering of the sidebar, you can add `weight` to the frontmatter of the documents.

To add a new Markdown file, simply run the following command from the `components/automate-chef-io` directory:

```
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

## API Development

To view the API documentation locally, enter the `/components/automate-chef-io` directory (where this README.md is located) and run:

`make serve`

In your browser, navigate to:

`http://localhost:1313/docs/api/`

To develop API documentation

1. Run `make serve` from the automate-chef-io directory. When left running this command watches for changes to files it knows about and will rebuild the hugo site automatically.
1. Open a second terminal window for the next commands
1. Run compile_all_protobuf_components in a hab studio
1. Run make sync_swagger_files generate_swaggerfrom the automate-chef-io directory
1. visit http://localhost:1313/docs/api/

Or to compile everything in one copy-pastable command:

`hab studio run "source .studiorc && compile_all_protobuf_components" && pushd components/automate-chef-io/ && make sync_swagger_files generate_swagger && make serve || popd`

* Building docs
    * Installation of necessary things
    * Commands to run
    * TODO: is there a "getting started with automate" I can link to for this?
    * Don't fix conflicts in generated files, regenerate
* Docs Styleguide
    * Service title and description
    * Field description
    * Mention object reference gotcha
    * Grammar choices? ie, 1st vs 3rd person, etc
    * Tabs prefered over spaces
* Extra things?
* References / relevant links
    * https://github.com/OAI/OpenAPI-Specification/blob/3.0.0/versions/2.0.md#schemaObject
    * https://github.com/grpc-ecosystem/grpc-gateway/blob/master/examples/proto/examplepb/a_bit_of_everything.proto
    * https://github.com/Redocly/redoc/blob/master/docs/redoc-vendor-extensions.md#x-displayname
    * https://swagger.io/docs/specification/2-0/basic-structure/
