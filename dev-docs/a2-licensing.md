# A2 Licensing

2018-03-19

Chef Automate 2.x (A2) requires a valid license to enable full
functionality. A2 licenses encode start and end dates and are able to
describe product/feature entitlements and limits (e.g. number of
nodes).

A license can be added to a deployment by placing the license in the
config prior to initial deployment or by adding it to running A2
instance via the `chef-automate license apply` sub-command.

Internally, we use [JWT][] as the format for A2 licenses. Standard JWT
tools can be used to inspect the data inside an A2 license. Unlike,
A1, A2 licenses are "string friendly" and can be transported via
copy/paste in email and placed directly into configuration files.

[JWT]: https://tools.ietf.org/html/rfc7519

In a given A2 deployment, license storage and policy is handled by the
license-control-service (l-c-s) component. The l-c-s receives and
persists the license. It provides an internal GRPC API that other A2
components use to determine the current license state.

As of this writing, the behavior of the system with respect to
missing, invalid, or expired licenses is as follows:

* You can successfully deploy A2 without a license.

* When the system is missing a license, has an invalid license, or has
  an expired license, the session-service prevents new logins. Thus,
  without a license it is not possible to login to the UI. Note that
  existing sessions are preserved.

* We intend to use contract start/end as the start/end dates in
  generated licenses. Given the current scheme, customers will be
  unable to establish new logins to the UI after their contract (and
  license) expire. We intend to further enhance the experience to
  display banner countdown when the license is close to its expiration
  date.

The design of the license format and the l-c-s will allow us to
support additional license based behavior in the system such as
enforcing node count limits and conditionally enabling/disabling
product feature sets. We are not planning to build additional license
behavior beyond UI login lockout (and banner countdown) for the
initial release.
