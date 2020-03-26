# Chef Automate Manifest URLs

Our CI pipeline uploads and promotes manifests, version information,
and the automate-cli command line tools. These can be found at the
following URLs

- `https://packages.chef.io/manifests/CHANNEL/automate/versions.json` all versions released to a particular channel
- `https://packages.chef.io/manifests/CHANNEL/automate/latest.json`: the latest manifest for a particular channel
- `https://packages.chef.io/manifests/automate/VERSION.json`: the manifest for a particular version
- `https://packages.chef.io/files/CHANNEL/latest/chef-automate-cli/chef-automate_linux_amd64.zip`:the latest version of the automate-cli command line tool for a particular channel
- `https://packages.chef.io/files/automate/VERSION/chef-automate_linux_amd64.zip`: the version of automate-cli that shipped with the given Chef Automate version
