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

## Insuring autocomplete remains disabled on the login screen.

A configuration maintained in automate determines to autocomplete functionality in the nginx config.

    add_header Cache-Control "no-cache, no-store, must-revalidate";
    

Please ensure you don't update or change this configuration.
