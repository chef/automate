+++
title = "Add Custom Message"

weight = 60
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Add Custom Message"
    parent = "automate/configuring_automate"
    identifier = "automate/configuring_automate/disclosure_panel_and_banner.md Add Custom Message"
    weight = 60
+++

Chef Automate platform let's you manage application which helps in speed up your operations. Chef Automate allows you add disclosure statement or a highlighted content which can display information, a warning or any instruction accordingly. You can display the content in two different places.

- The Chef Automate login page, i.e., **Disclosure Panel**
- The top of the Chef Automate Interface, i.e., **Banner**

## Disclosure Panel

**Disclosure Panel** is a content space in the login page of Chef Automate. In the panel, you can display custom message, warning, instruction or any important information for the one who is logging in should be well aware about.

The Disclosure Panel looks like:

IMAGE

To add a Disclosure Panel to the login page, follow the instructions given below:

- Create a `.txt` file and save to a location where the Chef Automate can track it.

- In the `.txt` file, you can write the message or the information you want to add to the disclosure panel.

- Create a patch file and save it in the `.toml` file format.

- In the `patch.toml` file, add the disclosure panel configuration settings as shown below:

```toml
[global.v1]
  [global.v1.disclosure]
    show = true # Set the value of `show` to `true` to enable the disclosure panel in the login page
    message_file_path = "/src/dev/disclosure-panel-message.txt" # The `.txt` file containing the message of the panel
```

{{< note >}}
To disable (default value) the disclosure panel, set the value of `show` to **true**.
{{< /note >}}

## Banner

**Banner** is a content space which is displayed across the top of the login page and the Chef Automate Interface to notify specific message about the product or their network. The Banner can be used to display custom message, warning, instruction or any important information.

The Banner looks like:

IMAGE

To add a Banner to Chef Automate, follow the instructions given below:

- Create a patch file and save it in the `.toml` file format.

- In the `patch.toml` file, add the banner configuration settings as shown below:

```toml
[global.v1]
  [global.v1.banner]
    show = true # Set the value of `show` to `true` to enable the banner
    message = "Lorem ipsum dolor sit amet" # Add the Message for the banner
    background_color = "3864f2" # Set the background color using the Hex Color Code (Do not add # to the code)
    text_color = "FFF" # Set the color of the text using the Hex Color Code (Do not add # to the code)

    # Find valid HEX codes at https://htmlcolorcodes.com/
    # check your HTML validation at https://validator.w3.org/
```

{{< note >}}
The Banner can display only one message, warning or an instruction throughout the interface.
{{< /note >}}
