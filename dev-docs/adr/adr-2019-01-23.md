# ADR 2018-08-15: Backwards Compatibility for Automate UI Routes

## Status

* PENDING (2019-01-23)

## Context

Previously, we did not have any rules for governing Automate UI route
lifecycle. In the past, we likely broke backwards compatibility when
re-naming or moving routes.

Additionally, the top nav highlighting in Automate UI was driven off the
root route (e.g. everything under the `Settings` tab must live under the `/settings`
route or it will not be highlighted properly for pages that live under it).
This means that if you want to move a page to a new tab, it will require a new route.

## Decision

Every route that originally linked to a page that is still in use in the webapp
must still work and backwards compatibility must be maintained. Only when the webpage
that the original route linked to is being removed from the app can a route be removed.

This is so that deep linking from our own sources (blog posts, etc.) as well as links
customers use internally will continue to work until they no longer make sense at all
and the original resource no longer exists.

To maintain backwards compatibility, the original route must redirect to the new route
(achievable in `app-routing.module.ts`) and an e2e test must be added to verify
that redirection in `deprecated-routes.e2e-spec.ts`. See
[this commit](https://github.com/chef/automate/commit/38779c5aabb1af8d6c22e4b6e1b07eaf9c8fa06c)
for an example of routes being renamed with proper backwards compatibility, what the e2e
tests should look like, and how a page can move between topnav tabs.


## Consequences

All deprecated Automate UI routes redirect to their new location until the relevant content
is removed from the webapp (with e2e testing verifying that the redirection works).

This also means that any route you add to the app will exist until the corresponding
part of the app is deleted, so choose your route namespaces carefully!
