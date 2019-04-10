# ADR 2018-10-04: Names Not UUIDs for IAM API

## Status

* ACCEPTED (2018-10-24)
* PROPOSED (2018-10-04)

## Context

In our current IAM implementation, some services identify entities using human-readable names and other services identify entities using UUIDs (which we'll refer to as IDs in the following discussion). 

Using IDs makes ensuring entity uniqueness easier. With UUIDs we can allow independent and concurrent creation of identifiers across multiple instances of a given service. IDs have performance advantages for use in data stores since comparisons are faster and indices are smaller (in general) than those that use a natural key.

APIs that can be accessed using human-readable names tend to provide a more self-documenting protocol whereas those that deal only in IDs result in message payloads that require additional tooling to map into a human readable form. A named-based approach requires verifying uniqueness within a data store to maintain data consistency.

When users are the one to select a name, they often want the ability to change their mind and change the name. Rename functionality improves usability by letting users reflect their updated understanding into the system. Renames also introduce implementation challenges as one needs to decide when and how to cascade renames throughout the system.

When we consider building aggregate functionality combinging the capabilities of multiple services, we want to be able to share an entity identifier across the bounded context in which the entity resides. Once this occurs, changing the identifier (rename) becomes more complicated and, definitionally, will be outside of a transaction. Strategies to handle this distributed cascade problem include event driven architecture where such updates are published to the event bus and providing long-lived aliases such that previous identifiers continue to work (e.g. github repo rename behavior).

Looking across the A2 API, today most messages use an `id` field as the name for the entity's unique identifier. This does not align with Google's API design guidance (https://cloud.google.com/apis/design/resource_names) the requires the field to be called `name`. The proposed decision is to make use of `name` following Google's API design guide for new APIs and to commit to evolving existing APIs to support `name` as well. The alternative is to agree that our identifiers are called `id` regardless of their form. Either way, a goal of this decision is to define a standard that will result in increased consistency across the A2 API.

## Decision

For IAMv2 we will use human-readable resource identifiers for the user visible protocol. This means that policies, roles, scopes, teams, and users will all be identified using unique "names" in the public GRPC API and resulting HTTP/JSON API that are intended to be human readable "friendly" names.

We will disallow renames to an entity's identifying name so that the names can be shared outside of the IAMv2 system.

We will support a "display name" field for entities in IAMv2 that users can use to describe the item and that we can use for UI display. The display name can be renamed. The UX is undecided, but we can consider flows where resource name is derrived from display name or vice versa to reduce the number of inputs a user needs to provide when creating things. The GCP projects API serves as a good example. In general, uniqueness should be enforced for `display_name` since the intention is to provide a human readable and changable label for distinct items.

In the IAMv2 data store (postgresql), we will use UUIDs and the core IAM services will handle UUID/name mapping and uniqueness constraint enforcement.

We will use the `id` field to store the unique, unchanging identifier in our GRPC protocol message definitions. We will use `display_name` for the can-be-edited name. For now, we are deciding not to adopt `name` as the field name for an entity's unique identifier as suggested in Google's [design guidance](https://cloud.google.com/apis/design/resource_names). While we'd like to follow those guidelines, the change would cause more disruption than benefit at this time. We can reconsider in future.

When using names instead of UUIDs, special consideraton needs to be given to handling delete operations for entities that may be referenced outside of their originating bounded context. The system cannot rely on cascading delete behavior when a given identifier is stored across different bounded contexts in the system. In these situations, we have to consider what happens when an item is deleted and then a new item is created with the same id. If the system allows this re-use of id after delete, then it must ensure that all references are removed or invalidated. Alternatively, the system can implement a delete marker (aka "tombstone") approach and disallow deleted identifiers from ever being reused.

## Consequences

Once this decision is implemented, we will assess the resulting context by 2019-03-10.

