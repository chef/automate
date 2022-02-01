# Chef Automate Documentation

The Chef Automate Documentation is deployed on https://docs.chef.io/automate/ using Hugo and Netlify.

## The Fastest Way to Contribute

There are two steps to updating the Chef Automate documentation:

1. Update the documentation in the `chef/automate` repository.
1. Update the Chef Automate repository module in `chef/chef-web-docs`.

### Update Content in `chef/automate`

The fastest way to change the documentation is to edit a page on the
GitHub website using the GitHub UI.

To perform edits using the GitHub UI, click on the `[edit on GitHub]` link at
the top of the page that you want to edit. The link takes you to that topic's GitHub
page. In GitHub, click on the pencil icon and make your changes. You can preview
how they'll look right on the page ("Preview Changes" tab).

We also require contributors to include their [DCO signoff](https://github.com/chef/chef/blob/master/CONTRIBUTING.md#developer-certification-of-origin-dco)
in the comment section of every pull request, except for obvious fixes. You can
add your DCO signoff to the comments by including `Signed-off-by:`, followed by
your name and email address, like this:

`Signed-off-by: Julia Child <juliachild@chef.io>`

See our [blog post](https://blog.chef.io/introducing-developer-certificate-of-origin/)
for more information about the DCO and why we require it.

After you've added your DCO signoff, add a comment about your proposed change,
then click on the "Propose file change" button at the bottom of the page and
confirm your pull request. The CI system will do some checks and add a comment
to your PR with the results.

The Chef documentation team can normally merge pull requests within seven days.
We'll fix build errors before we merge, so you don't have to
worry about passing all the CI checks, but it might add an extra
few days. The important part is submitting your change.

### Update the Chef Automate Repository Module In `chef/chef-web-docs`

We use [Hugo modules](https://gohugo.io/hugo-modules/) to build Chef's documentation from multiple repositories. The documentation from those repositories are [vendored](https://gohugo.io/hugo-modules/use-modules/#vendor-your-modules) in chef-web-docs.

When Chef Automate is promoted to stable, Expeditor will submit a pull request to `chef/chef-web-docs` to update the Automate documentation on.docs.chef.io.

To update the Hugo module for documentation in `chef/automate`:

1. Make sure your documentation changes are merged into master in `chef/automate`.
1. Wait for Expeditor to submit a PR to `chef/chef-web-docs` after Chef Automate is promoted to stable.

If you need to manually update the Chef Automate documentation, contact your
friendly local Docs Team who will update the Automate Hugo module for you.

## Local Docs Development Environment

For API Docs Development, see the [API_README.md](https://github.com/chef/automate/blob/main/components/docs-chef-io/API_README.md)
We use [Hugo](https://gohugo.io/), [Go](https://golang.org/), [NPM](https://www.npmjs.com/).
You will need Hugo 0.92 or higher installed and running to build and view the documentation.

To install Hugo, NPM, and Go on Windows and macOS:

- On macOS run: `brew install hugo node go jq`
- On Windows run: `choco install hugo nodejs golang jq`

To install Hugo on Linux, run:

- `apt install -y build-essential`
- `sudo apt-get install jq`
- `snap install node --classic --channel=12`
- `snap install hugo --channel=extended`

1. [Install cspell](https://github.com/streetsidesoftware/cspell/tree/master/packages/cspell)

    To be able to run the optional `make spellcheck` task you'll need to install `cspell`:

    ```shell
    npm install -g cspell
    ```

## Preview Chef Automate Documentation

There are two ways to preview the documentation in `chef/automate`:

- Submit a PR
- `make serve`

### Submit a PR

When you submit a PR to `chef/automate`, Netlify will build the documentation
and add a notification to the GitHub pull request page. You can review your
documentation changes as they would appear on docs.chef.io.

### make serve

Running `make serve` will clone a copy of `chef/chef-web-docs` into `components/docs-chef-io`.
That copy will be configured to build the Automate documentation from `components/docs-chef-io`
and live reload if any changes are made while the Hugo server is running.

- Run `make serve`
- go to http://localhost:1313

## Clean Your Local Environment

If you have a local copy of chef-web-docs cloned into `components/docs-chef-io`,
running `make clean_all` will delete the SASS files, node modules, and fonts in
`components/docs-chef-io/chef-web-docs/themes/docs-new` used to
build the docs site in the cloned copy of chef-web-docs. Hugo will reinstall these the next time you run `make serve`.

## Creating New Pages

Please contact the documentation team if you think your project requires new pages. Chef Automate documentation files belong in the `components/docs-chef-io/content/automate` directory. To add a new Markdown file, run the following command from `components/docs-chef-io`:

```
hugo new content/automate/<filename>.md
```

This command creates a draft page with enough front matter to start writing.

Hugo uses [Goldmark](https://github.com/yuin/goldmark) which is a superset of Markdown that includes GitHub styled tables, task lists, and definition lists.

See our [Style Guide](https://docs.chef.io/style_guide/) for more information about formatting documentation in Markdown.

## Data Content

Hugo allows us to nest our data directory structure as much as necessary. You can add as many folders as necessary under `components/docs-chef-io/data/automate`.

```output
.
├── data
│   ├── automate
│   │   ├── cli_chef_automate
|   │   │   ├── command_one.yml
|   │   │   └── command_two.yml
```

### chef-automate CLI Page

There are two shortcodes that are used to generate the page content of the chef-automate CLI Page, `automate_cli_commands` and `automate_cli_status_errors`.
The shortcodes are in the "docs-new" theme on `chef/chef-web-docs` and both have a `data_path` parameter which is the path to the data file or files that are used by the shortcodes to generate that page's content.

In addition, the chef-automate CLI page uses a custom layout to generate
that page's table of contents. The name of layout is specified in the chef-automate CLI page's frontmatter and the layout is stored in `layouts/partials`.

## Documentation Feedback

We love getting feedback, questions, or comments.

### Email

Send an email to Chef-Docs@progress.com for documentation bugs, ideas, thoughts, and suggestions. This email address is not a support email address. If you need support, contact [Chef Support](https://www.chef.io/support/).

### GitHub issues

Submit an issue to the [Automate repo](https://github.com/chef/automate/issues)
for "important" documentation bugs that may need visibility among a larger group, especially in situations where a doc bug may also surface a product bug.

Submit an issue to [chef-web-docs](https://github.com/chef/chef-web-docs/issues) for doc feature requests and minor documentation issues.
