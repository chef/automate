+++
title = "Disclosure Panel and Banner"

weight = 60
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Disclosure Panel and Banner"
    parent = "automate/configure"
    identifier = "automate/configure/disclosure_panel_and_banner.md Disclosure Panel and Banner"
    weight = 30
+++

Customize your Chef Automate installation with a **Disclosure Panel** to add critical information about your unique environment on the sign-in page and a **Banner** at the top of every page to highlight important information.

## Disclosure Panel

The **Disclosure Panel** is a space where you can add custom content to the Chef Automate sign-in page. Use this panel to display information, such as a message, warning, or instructions, that appears during the sign-in process.

An example disclosure panel:

{{< figure src="/images/automate/disclosure-panel-in-chef-automate.png" alt="An example Disclosure Panel in Chef Automate with Lorem Ipsum text" width="400" >}}

To add a disclosure panel to the sign-in page:

1. Create a file with the message in `.html` or `.txt` format and save to a location that is available to Chef Automate.
1. Create a `patch.toml` if one does not already exist for your Chef Automate installation.
1. Add the disclosure panel configuration to the `patch.toml` file. Your configuration should look like:

```toml
[global.v1]
  [global.v1.disclosure]
    show = true # Set 'show' to 'true' to enable the disclosure panel on the sign-in page. Set to 'false' to disable the disclosure panel. Default: false.

    message_file_path = "/src/dev/disclosure-panel-message.txt" # The location of the file containing the disclosure panel message.

    # Validate your HTML at https://validator.w3.org/
```

## Banner

The **Banner** displays custom content at the top of every page in Chef Automate, including the sign-in page. Use it to add a message, warning, or instructions to Chef Automate users. The banner can display only one message, warning or instruction throughout the interface.

An example banner:

{{< figure src="/images/automate/banner-in-chef-automate.png" width="800" alt="Banner in Chef Automate Interface">}}

To add a banner to Chef Automate:

1. Create a `patch.toml` if one does not already exist for your Chef Automate installation.
1. Add the banner configuration to the `patch.toml` file. Your configuration should look like:

```toml
[global.v1]
  [global.v1.banner]
    show = true # Set 'show' to 'true' to enable the banner. Set to 'false' to disable the banner. Default: false.

    message = "Lorem ipsum dolor sit amet" # Add the Message for the banner
    background_color = "3864f2" # Set the background color using the Hex Color Code (Do not add # to the code)
    text_color = "FFF" # Set the color of the text using the Hex Color Code (Do not add # to the code)

    # Find valid HEX codes at https://htmlcolorcodes.com/
```
