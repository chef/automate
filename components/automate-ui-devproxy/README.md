# Automate UI Devproxy

`automate-ui-devproxy` is a simple nginx proxy that fulfils the bind contract of `automate-ui` and proxies
to Automate UI running on your mac host via `host.docker.internal:4200` to allow for local UI
development. Please follow the [UI development guide](../../dev-docs/ui-development.md) for details
on its use.

`$DEVPROXY_URL` must be set at runtime to wherever you want to proxy to, but fret not. The `.studiorc`
is setting it to either `localhost` or `host.docker.internal` depending on if you are in the Vagrant-based
or Docker-based dev env.
