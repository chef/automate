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

## Insuring autocomplete remains disable on login screen.

Their is a configration maintained in automate this determine autocomplete functionlity

in the nginx config 

    
    add_header Cache-Control "no-cache, no-store, must-revalidate";
    

Please insure that you don't update or change this confugration.
