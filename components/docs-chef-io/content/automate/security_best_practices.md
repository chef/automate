+++
title = "Security Best Practices"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Security Best Practices"
    parent = "automate/reference"
    identifier = "automate/reference/security_best_practices.md Security Best Practices"
    weight = 50
+++

## Ensuring autocomplete remains disabled on the login screen.

A configuration maintained in Chef Automate's nginx.conf file determines the autocomplete functionality.

    add_header Cache-Control "no-cache, no-store, must-revalidate";
    

Please ensure you don't update or change this configuration.
