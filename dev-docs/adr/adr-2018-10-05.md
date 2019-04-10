# ADR 2018-10-05: Compliance Profile Namespaces and Authorization

## Status

* APPROVED (2018-10-24)
* PROPOSED (2018-10-05)

## Context

### Background on Compliance Profile Storage and API

Compliance profiles are stored and accessed using a base name that serves as a namespace and a profile name, and, optionally, a version. Here's an example of how a profile is specified using the [audit coobkook](https://github.com/chef-cookbooks/audit):

```ruby
default['audit']['profiles'].push(
    # Profile from Chef Compliance
    {
      'name': 'linux',
      'compliance': 'base/linux'
    },
    # Profile from Chef Compliance at a particular version
    {
      'name': 'linux-baseline',
      'compliance': 'user/linux-baseline',
      'version': '2.1.0'
    })
```

In the example above, two profiles are specified. The first is named `linux` and is stored in the `base` namespace. The second is named `linux-baseline` and stored in a namespace called `user`.

In A2, the API supports creating, reading, and deleting profiles (profiles are immutable, so there is no update).

Here's the read API from the gateway:

```proto
rpc Read (ProfileDetails) returns (Profile) {
		option (google.api.http) = {
			get: "/compliance/profiles/read/{owner}/{name}/version/{version}"
		};
		option (chef.automate.api.policy) = {
			resource: "compliance:profiles:storage:{owner}"
			action: "read"
		};
		option (chef.automate.api.iam.policy) = {
			resource: "compliance:profiles:{owner}"
			action: "compliance:profiles:get"
		};
	};
	
```

The delete REST API follows the same URL path.

### Current Usage and Authorization for Compliance Profiles

In A2 today, each user has an implicit compliance profile namespace mapped to their A2 username. Users can create and delete profiles within their own username-based namespace. Default authorization policy allows users to read any profile. The username being used in profile URLs does not include the authentication connector (e.g. local, LDAP, SAML). As a result, the username-based namespace is not unique across auth connectors and the system is unable to distinguish between a local user "olivia" and an LDAP user "olivia".

When we tested an improvement to the username-based namespace approach with SAML users, we identified a bug in the system in which the auth system and UI have a disagreement about which field to use as "username". For SAML users, this prevented the username-based namespace for compliance profiles to function properly.

## Decision

We will treat the base name of compliance profiles as a generic namespace.

A2 admins will create compliance profile namespaces and corresponding IAM policies that will determine which users are allowed to create, read, and delete profiles in a given namespace. In future, we can extend the system to support user creation and implicit ownership of profile namespaces, but this will not be supported initially.

We will represent compliance profile namespaces in the system (not just implicitly via IAM policies). This will allow listing profile namespaces and copying a profile from one namespace to another (note that this is a reference copy as internally profiles are stored by content SHA256).

We will address migration of current deployments through default policy behavior, documentation, and suggested workflow for A2 admins. The outline for migration is:

* On upgrade, all users can READ profiles in any namespace.
* On upgrade, only admin users can CREATE and DELETE profiles (in any namespace). All non-admin users have read-only access -- even for profiles that they previously created.
* Documentation will walk admin users through the process of creating compliance profile namespaces along with IAM policies to control access to those namespaces.
* Documentation will walk admin users through the process of consolidating existing username-based namespaces via copy operation.
* Documentation will walk admin users through process of using existing username-based namespaces and restoring access by adding IAM policies (not recommended approach). This is only relevant for customers who maintain a strong desire to maintain the existing behavior and layout for profile management.


## Consequences

