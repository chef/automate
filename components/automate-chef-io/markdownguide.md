# Markdown Guide

## Using Markdown

We build our docs using the [Hugo](https://gohugo.io/) static site generator, for more information.
Some of the conventions in markdown require special hugo shortcodes.
You'll see instructions for shortcodes where we want you to use them.

### Section Headers

A page will have one or more topics. Use section headers for different topics.
As general rule, limit the number of heading levels to no more than two within any given topic.
There can be exceptions, especially if the document is large,
but remember that HTML TOC structures have width limitations on the display side
and the greater the structure of information, the harder it is to figure out what's inside.

Unless the topics are about installing things or about API endpoints, the headers should never wrap.
Keep headers to a single line.

The quantity of octothorpe (#) before the heading determines its level.

# # H1

Auto-generated title from page metadata. Everything else is body.

## ## H2

Use for main topics.

### ### H3

Use for meaningful content groupings within a topic.

#### #### H4

Use for granular collections of information within groups.
Consider if a list provides more a meaningful context for information than a H4 header.

##### ##### H5

It exists, but consider alternatives.

###### ###### H6

Should you? Really?

### Other headers

Should you need more than six heading levels,
use **bold** emphasis and then white space to provide the visual treatment and content separation:

```md
**header name goes here**
blank line
content, as normally authored.
```

### Lists

The following sections describe conventions for lists and tables in Chef Automate docs.

#### Unordered Lists

Bulleted lists break up text blocks and draw attention to a group of items:

```md
   * text goes here
   * text goes here
   * text goes here
```

Use the asterisk symbol (*) for bulleted lists.

* text goes here
* text goes here
* text goes here

### Ordered Lists

Use the number one (1) to let markdown handle the actual ordering.

```md
   1. text goes here
   1. text goes here
   1. text goes here
```

1. text goes here
1. text goes here
1. text goes here

#### Nested lists

Hopefully, you will find a simpler solution, but if not, one _can_ create complex lists:

```md
1. Main text goes here
  1. ordered child text
  1. ordered child text
1. text goes here
  * unordered child text
  * unordered child text
```

1. Main text goes here
  1. ordered child text
  1. ordered child text
1. text goes here
  * unordered child text
  * unordered child text

#### Definition Lists

Definition lists are primarily used show the options available to a command line tool.

The markdown for a definition list is:

    term
    : definition

For example, these definitions

    --name-only
    : Show only the names of modified files

    --name-status
    : Show only the names of files with a status of "Added", "Deleted", "Modified", or "Type Changed."

Will display as:

--name-only
: Show only the names of modified files

--name-status
: Show only the names of files with a status of "Added", "Deleted", "Modified", or
"Type Changed."

### Tables

Markdown uses a flexible syntax for creating tables.

* Use at least three dashes (---) to separate header cells.
* Use a pipe (|) to divide columns
* The outer pipes (|) are optional
* Tables don't need to line up in Markdown
* Use inline Markdown for font changes
* Use Colons to align columns

Simple markdown tables render as well as pretty markdown tables.

```md
Markdown | Simple | Table
--- | --- | ---
*This* | _renders_ | **nicely**
1 | 2 | 3
```

Renders as:

Markdown | Simple | Table
--- | --- | ---
*This* | _renders_ | **nicely**
1 | 2 | 3

The more human-readable markdown table renders the same:

```md
| Markdown  | Pretty  | Tables  |
| --------- | ------- | ------- |
| _This_    | renders | **too** |
```

Also renders as:

| Markdown  | Pretty  | Tables  |
| --------- | ------- | ------- |
| _This_    | renders | **too** |
| 1         | 2       | 3       |

#### Table Formatting

Align column text by using colons `:` with the header separation dashes.

Row headers align left, regardless of the formatting applied to the column.

Left | Right | Center
---: | :--- | :---:
This| That | Those
1 | 2 | 3

### Inline Markup

Add emphasis in sentences by using `code strings` _italics_, and **bold**.

### Code Strings

Use one backtick to mark certain strings as code within a regular string of text

```md
`This is code string markdown`
```

`This is how code string markdown looks`

#### Italics

Use one underline (`_italics_`) to italicize a text string.
_An important statement_

#### Bold

Use two asterisks (`**bold**`) to **bold** a text string.
**A bold statement**

{{< info >}}
Hugo italicizes words surrounded by single asterisks or underlines (*)(_)
and emboldens all double asterisks or underlines (**)(__).
Help your editors understand your intent by following the established conventions
and use single underlines for _italics_ and double asterisks for **bold**.
{{< /info >}}

### Links

Chef Automate docs can contain reference and relative links. The only difference between `ref` and `relref` is if the resulting URL is relative `(/admin/)` or absolute `(http://automate.io/docs/cli-chef-automate/)`.

    This is an example [relative link]({{< relref "users-and-teams.md" >}})
    This is [an example reference link]({{< ref "cli-chef-automate.md" >}})

Renders as:

This is an example [relative link]({{< relref "users-and-teams.md" >}})
This is [an example reference link]({{< ref "cli-chef-automate.md" >}})

External links, however, follow conventional markdown:

    This is your example [external link](http://www.example.com).

becomes:

This is your example [external link](http://www.example.com).

### Code Blocks

Code blocks are used to show code samples, such as those for Ruby, JSON, and command-line strings.

Our code blocks follow these conventions

1. Code blocks are opened and closed with code fence lines (```)
1. The code language is declared on the first code fence line (```bash)
1. The code fences and code align to the first column of the document.

#### Bash

Use this approach to show code blocks that use any type of shell command:

    ```bash
    $ knife data bag create admins
    ```

```bash
knife data bag create admins
```

#### Javascript (and JSON)

Use this approach to show code blocks that use any type of JavaScript, including any JSON code sample:

    ```javascript
    {
      "id": "charlie",
      "uid": 1005,
      "gid":"ops",
      "shell":"/bin/zsh",
      "comment":"Crazy Charlie"
    }
    ```

```javascript
{
  "id": "charlie",
  "uid": 1005,
  "gid":"ops",
  "shell":"/bin/zsh",
  "comment":"Crazy Charlie"
}
```

#### Ruby

Use this approach to show code blocks that use Ruby:

    ```ruby
    ruby_block 'reload_client_config' do
      block do
        Chef::Config.from_file("/etc/chef/client.rb")
      end
      action :run
    end
    ```

```ruby
ruby_block 'reload_client_config' do
  block do
    Chef::Config.from_file("/etc/chef/client.rb")
  end
  action :run
end
```

#### Literal

Literals should be used sparingly, but sometimes there is a need for a block of text that doesn't fit
neatly into one of the options available for a code block, such as showing a directory structure,
basic syntax, or pseudocode.
Add a hard return to the preceding paragraph and then indent the literal text four spaces:

    a block of literal text indented four spaces
    with more
    text as required to
    complete the block of text.

## Notes and Warnings

Notes and warnings provide a visual signal that a piece of information is highly important or
somehow unusual. Use them sparingly, if at all.

In Chrome, it is possible that the styling for alerts and other custom elements won't show.
You can fix this by either:

1. Turning off the cache in chrome dev console
1. Running `hugo -D server --noHTTPCache`

{{% danger %}}
Danger! Here be monsters!
{{% /danger %}}
</br>

{{% info %}}
Info: What are monsters? Monsters are imaginary creatures that are typically large, ugly, and frightening.
{{% /info %}}
</br>

{{% warning %}}
Warning: Under certain conditions, there might be monsters.
{{% /warning %}}
</br>

## Images

TODO: Image type, size
TODO: Image storage
