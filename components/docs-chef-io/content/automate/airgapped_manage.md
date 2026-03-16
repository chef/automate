## Overview

Normally, an Automate system has a list of Chef sites it will need access to in order to check for and perform upgrades.
When a system is configured to refer only to generated bundles as a source of new packages for upgrades, this is called air-gapped mode.

In an Automate system, that air-gapped behavior is determined by the presence of the following file:

`/hab/svc/deployment-service/data/airgap/manifest.json`

## Return to automatic Internet upgrades

You can follow these steps to return an air-gapped system to the standard behavior, where it contacts Chef provided sites on the Internet to update itself automatically.

shell```

systemctl stop chef-automate.service

rm -f /hab/svc/deployment-service/data/airgap/manifest.json

systemctl start chef-automate.service
```
