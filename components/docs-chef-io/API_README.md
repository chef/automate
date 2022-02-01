## Purpose

Our goal is to provide clear, correct, and useful documentation for Chef Automate users.

## Setup

We use [Hugo](https://gohugo.io/), [Go](https://golang.org/), [NPM](https://www.npmjs.com/),
[go-swagger](https://goswagger.io/install.html), and [jq](https://stedolan.github.io/jq/).
You will need Hugo 0.92 or higher installed and running to build and view our documentation properly.

To install Hugo, NPM, Swagger, jq, and Go on Windows and macOS:

- On macOS run: `brew tap go-swagger/go-swagger && brew install go-swagger hugo node go jq`
- On Windows run: `choco install hugo nodejs golang jq`
  - See the Go-Swagger [docs to install go-swagger](https://goswagger.io/install.html)

To install Hugo on Linux, run:

- `apt install -y build-essential`
- `sudo apt-get install jq`
- `snap install node --classic --channel=12`
- `snap install hugo --channel=extended`
- See the Go-Swagger [docs](https://goswagger.io/install.html) to install go-swagger

1. [Install cspell](https://github.com/streetsidesoftware/cspell/tree/master/packages/cspell)

    To be able to run the optional `make spellcheck` task you'll need to install `cspell`:

    ```shell
    npm install -g cspell
    ```

## Build and View API Documentation

Rebuilding the `.proto` files is required for:

- Documenting new or updated services, messages, or fields.
- Documentation changes to existing Chef Automate `.proto` files.

Rebuilding the `.proto` files is *not* required for:

- Viewing the current API documentation locally
- Code changes in the `.proto` files that do not include documentation changes.*However*, new or updated services, messages, or fields for endpoints *require* documentation.

## View Published Documentation

To view the API current documentation locally, navigate to the `components/docs-chef-io` directory, run the following command to start the development server:

```shell
make serve
```

To view the API docs in your browser, navigate to:

```shell
http://localhost:1313/automate/api
```

## View API Docs During Development

Chef Automate generates API documentation from proto files comments using

- [grpc-gateway protoc-gen-swagger](https://github.com/grpc-ecosystem/grpc-gateway/tree/master/protoc-gen-swagger) as the protoc compiler
- [OpenAPI 2.0 specification](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md) as the API schema
- [ReDoc](https://github.com/Redocly/redoc) as the front-end display

### Build the API Docs

1. Edit the .proto files.
2. Recompile the .proto files as described in the [Chef Automate development guide](https://github.com/chef/automate/blob/master/dev-docs/DEV_ENVIRONMENT.md). This step automatically generates the `Authorization Action:` section. See [PR #2982](https://github.com/chef/automate/pull/2982) for information and caveats.
3. From the top of the `components/docs-chef-io` directory, run `make sync_swagger_files` to sync the .swagger files generated with those in the docs component.
4. Run the `make serve` and view the documentation at:

    ```shell
    http://localhost:1313/automate/api
    ```

5. Before opening a PR, run `make spellcheck` the top of the `components/docs-chef-io` directory, and correct your typos.

### All-in-one Build Command

You can build the API docs by running the following all-in-one command from the top-level `automate` directory:

`hab studio run "source .studiorc && compile_all_protobuf_components" && pushd components/docs-chef-io/ && make sync_swagger_files generate_swagger && make serve || popd`

Review the API docs in your browser:

`http://localhost:1313/docs/api`

### Notes on building the docs

- Don't try to resolve merge conflicts in generated files. Fix the problems in the `.proto` file or other human-curated files.
- The all-in-one build command changes directories and you will need to change back to the root Chef Automate directory.
- The all-in-one build command sometimes fails and leaves things in an inconsistent state.
- The all-in-one build command is safe to run repeatedly while developing and editing `.proto` files or similar,

## Tags and Tag Groups

- Endpoints need to appear in the rendered documentation as human-readable groupings. We use group and endpoint tags to create an organized and meaningful presentation for the Chef Automate API.
- Endpoints must have a tag that is a member of a tag group in order to appear in the rendered documentation. Endpoints without tags that are members of a tag group will not show up.
- An endpoint's default tag is the parent service where it was defined.
- Every service must have a unique tag name. For services without unique names, follow the explicit tagging process. Without unique service tag names, all endpoints for all of these services appear together in the rendered documentation.

### Automatic tagging

Set up automatic tagging in the `components/docs-chef-io/data/automate/api-static/03-tags.swagger.json` file.

- Add the service's name to the `tags` list in a `tagGroup` or create a new `tagGroup` with the service's name in the `tags` list.
- Make the service name human-friendly by adding an entry to the `tags` in with a custom `x-displayName`.

### Explicit tagging

- Each endpoint needs a tag specified in its definition, by adding a block like this to the endpoint definition in the proto file:

    ```go
    option (grpc.gateway.protoc_gen_swagger.options.openapiv2_operation) = {
      tags: "My Custom Tag"
    };
    ```

- All tags for an individual service should be the same, unless multiple groupings are desired in which case multiple independent tags can be used.
- After this, the process described for automatic tagging can be used, substituting the custom tag defined in place of the service's name.

### Notes on Tags

- Proto files can use tabs and spaces. We have a defacto convention for using tabs since our protos are primarily used with Go code.
- Endpoints will not show up in the documenentation unless one or more of their tags are assigned to a tag group, but they will still appear in the Swagger file accessible through the docs in the browser.
- multiline fields are not allowed in proto files. This the main reason we settled on using the comment style of endpoint documentation.

## API Style Guide

Add comments to protos file with standard Go block and inline comment syntax.

Block comments are usually placed before the start of a gRPC service definition. Block comments for API documentation are sometimes called "leading comments".

Inline are usually placed within the message formats. Inline comments for API documentation are sometimes called "following comments".

```go
/* This is a block comment.

Block comments can span multiple lines.

Block comments support Markdown */

// This is an inline comment.
```

Block comments support multiple lines:

```go
  /*
  Fetch a node

  Fetch a specific node by id.
  Does not support filtering, pagination or sorting.
  */
  rpc ListReportIds(Query) returns (ReportIds) {
```

Block comments supports common markdown elements, such as:

```go
  /*
  Code blocks support `highlighting`

  It also supports

  Using code blocks, if you need them.

  */
```

Block comments support markdown tables:

```go
  /*
  List reports

  Makes a list of reports. Adding a filter makes a list of all node reports that meet the filter criteria. Supports pagination, filtering, and sorting.

  | Sort parameter | Sort value |
  | --- | --- |
  | latest_report.controls.failed.critical | controls_sums.failed.critical |
  | latest_report.controls.failed.total | controls_sums.failed.total |
  | latest_report.end_time (default) | end_time |
  | latest_report.status | status |
  | node_name | node_name.lower |
  */
  rpc ListReports(Query) returns (Reports) {
```

### Exclude an endpoint

You can exclude an endpoint from the swagger generation by importing this in your proto file:

```go
import "protoc-gen-swagger/options/annotations.proto";
```

And adding this annotation to the rpc in question:

```go
option (grpc.gateway.protoc_gen_swagger.options.openapiv2_operation) = {
  tags: "hidden";
};
```

### Service Documentation

Not in iteration #1.

### Resource Documentation

Given a GRPC endpoint defined like this in the proto files:

```go
>.rpc ListReports(Query) returns (Reports) {
>.>.option (google.api.http) = {
>.>.>.post: "/api/v0/compliance/reporting/reports"
>.>.>.body: "*"
>.>.};
>.};
```

- For the first iteration of the Chef Automate API docs, we need summaries and descriptions for all user-facing end points. This can be done two ways, in the comment style (preferred) or the attribute style.

- Comment style of endpoint documentation supports multiline fields. Attribute style does not.

- Descriptions support GitHub Flavored Markdown, which means that you can use lists, tables, code-blocks and more.

### Comment Style Descriptions

```go
  /*
  List reports
  This is the description.
  The description can span multiple lines and can contain `GitHub Flavored Markdown`
  */
  rpc ListReports(Query) returns (Reports) {
    option (google.api.http) = {
      post: "/api/v0/compliance/reporting/reports"
      body: "*"
    };
    option (grpc.gateway.protoc_gen_swagger.options.openapiv2_operation) = {
      tags: "Reporting"
    };
  };
```

### Attribute Style Descriptions

```go
rpc ListReports(Query) returns (Reports) {
    option (google.api.http) = {
      post: "/api/v0/compliance/reporting/reports"
      body: "*"
    };
    option (grpc.gateway.protoc_gen_swagger.options.openapiv2_operation) = {
      tags: "Reporting"
      summary: "List reports"
      description: "This is the description.\nThe description can span multiple lines and can contain GitHub Flavored Markdown."
    };
  };
```

Resource documentation provides RPC-level description. It is sometimes also called "method description" because it documents what you can do with a specific API HTTP Method.

Use block/leading comments for RPC-level/method documentation.

Compare the leading comments to the content of the API documentation for the `ReadNode(ID)` resource.

```go
  /*
  Fetch a node #(summary)

  Fetch a specific node by id. #(description)
  Does not support filtering, pagination or sorting.
  */
  rpc ReadNode(Id) returns (Node) {
```

The rendered documentation:

![View of rendered API docs](./static/automate/images/api-resource-definition.png)

Note that once you recompile the proto files as described earlier, the actual source comment above will be updated to include an authorization action, for any relevant methods.

```go
  /*
  Fetch a node #(summary)

  Fetch a specific node by id. #(description)
  Does not support filtering, pagination or sorting.

  Authorization Action:


  compliance:reportNodes:get


  */
  rpc ReadNode(Id) returns (Node) {
```

### Message Level Descriptions

Document fields and parameters directly above the relevant code line, using `//` inline comments

### Message Documentation

Given messages defined like this in the proto files:

```go
message Dog {
  // The dog's name.
  string name = 1;
  // Intentionally blank.
  Food favorite_food = 2;
  // Types of food the dog is normally fed.
  repeated Food typical_foods = 3;
}
// A type of food eaten by a Dog
message Food {
  // The food's name.
  string name = 1;
  // Serving size in ounces.
  int32 serving_size =3;
}
```

- Use a period (`.`) at the end of a comment to set it as the `description` for a message field. Omit the period to set a comment as the `title` for a message field. Our convention is to set comments as descriptions in message fields by closing them with periods.
- Swagger relies on inheritance to document nested references. A parent message referenced in a child message inherits its description from the parent, not where it is referenced by the child (see the example of `favorite_food` above). The Chef API style uses `// Intentionally blank.` in the child message.
This convention makes the purpose of the comment clear, makes undocumented message code visible, and makes bad blank message comment lines stand out in the API documentation.

## Non-Proto File Sources for API Documentation

We use non-proto file sources to document API content in cases where proto files run up against the limitations of tool we use for the Swagger conversion and also in cases where expressing the content in proto files is cumbersome.
We support these cases by adding static Swagger JSON or YAML files that combine with the Swagger generated from the proto files.

### When to use Non-Proto Files

- Endpoints that do not automatically generate Swagger files during proto compilation. An example of this is the ReportingService's Export endpoint, which derives its endpoint from outside of the proto files, and is excluded in the proto to Swagger conversion process. See: `components/docs-chef-io/data/automate/api-static/01-reporting-export.swagger.json`.
- Documentation data that extends the protobuf or Swagger specs, for example the ReDoc `X-tagGroup` that we use for tag groups and tag display names. See: `components/docs-chef-io/data/automate/api-static/03-tags.swagger.json`.
- Metadata that is specific to the API documentation. For example, the support page URL, the logo image, etc. See: `components/docs-chef-io/data/automate/api-static/00-meta.swagger.json`.
- Large sections of prose or formatted text. With a few exceptions, proto files do not support multiline strings, so it is simpler to write and edit long content outside of the proto files. See: `components/docs-chef-io/data/automate/api-static/02-meta-description.yaml`.

### Creating Non-Proto File Documentation

- Create a new JSON or YAML file in the `components/docs-chef-io/data/automate/api-static/` directory and its contents will be added to the final Swagger JSON  file used to render the documentation.
- Files in this directory are sorted numerically so that the order in which things are applied is deterministic and predictable.
- The Swagger files are combined in an additive way. The static user-edited Swagger files are applied first, follwed by all of the Swagger files generated from compiling the proto files.
- The Chef API style prefers JSON files in nearly all cases, but uses YAML files to supply long text sections in the documentation.

## API File Index

- **API Proto Files**: automate/components/automate-gateway/api/
- **Example API Proto File**: automate/components/automate-gateway/api/compliance/reporting/
- **Meta Swagger**: automate/components/docs-chef-io/data/automate/api-static/01-meta-swagger.json
- **Reporting Export**: automate/components/docs-chef-io/data/automate/api-static/01-reporting-export.swagger.json
- **Node Export**: automate/components/docs-chef-io/data/automate/api-static/05-node-export.swagger.json
- **Description**: automate/components/docs-chef-io/data/automate/api-static/02-meta-description.yaml
- **Tags**: automate/components/docs-chef-io/data/automate/api-static/03-tags.swagger.json
- **Compiled Swagger**: automate/components/docs-chef-io/static/automate/api-docs/all-apis.swagger.json

## External references

- [OpenAPI 2.0 (aka swagger) spec](https://github.com/OAI/OpenAPI-Specification/blob/3.0.0/versions/2.0.md)
- [An example proto file in the protoc-gen-swagger repo demonstrating how to use most features of the proto-to-swagger conversion](https://github.com/grpc-ecosystem/grpc-gateway/blob/master/examples/proto/examplepb/a_bit_of_everything.proto)
- [ReDoc extensions to the Swagger spec](https://github.com/Redocly/redoc/blob/master/docs/redoc-vendor-extensions.md)
- [An alternate Swagger spec](https://swagger.io/docs/specification/2-0/basic-structure/)
- [Documentation in a code comment of how protoc-gen-swagger converts comments in proto files to Swagger fields](https://github.com/grpc-ecosystem/grpc-gateway/blob/c3787b4d95d15f4c89484b44e108a6640c1943ca/protoc-gen-swagger/genswagger/template.go#L1294-L1305)
