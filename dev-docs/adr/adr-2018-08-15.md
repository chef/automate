# ADR 2018-08-15: Introduce Architectural Decision Record (ADR)

## Status

* ACCEPTED (2018-10-04)

## Context

We want to improve our collective ability to document and share the
decisions we make about the architecture of a2. Here, architecture
decisions are those that impact the structure and interactions of the
code and components, dependencies, interfaces, as well as
cross-cutting tools and techniques.

We started with a Google doc collecting [architecture questions and
answers][1]. This provided a place to collect questions and open
decisions, allowing for some discussion via comments, as well as a
place to record answers. Having the answers recorded has provided
value. The document has grown large and has not been updated
recently. The Architecture Team, in the lead up to a2's initial
release, took on an ad-hoc agenda setting that left the Q&A doc
unattended.

On a few occasions, team members have shared a proposal for a2
architecture as a separate Google doc providing a write-up and a place
for discussion. We've seen more detailed documentation of proposals
and more engaged discussion and input from a wider set of engineers
with this approach. The Architecture Team wants more of that style of
engagement: anyone working on a2 putting together a proposal, input
from a wide set of engineers, and an Architecture Team making a
decision (when needed).

[1]: https://docs.google.com/document/d/1AHwqbTPtMEOhd-JL0CNE3u8a0C9eXQVD1nIuzTkBTPw/edit

## Decision

We will record architecture decisions and store them in the a2 git
repository. An architecture decision is something that impacts the
overall structure, dependencies, non-functional behavior,
cross-cutting concerns, or otherwise deemed relevant to a2 as a whole
or those working on a2.

We will adopt Architecture Decision Records (ADRs) as the mechanism
for proposing and recording architecture decisions for a2. We are
modeling our approach based on this [blog post][] by Michael
Nygard. This document is our first ADR and provides an example of the
format we will follow.

ADRs stored in the a2 repo at
`a2/dev-docs/adr`. Each ADR is a markdown file using a datestamp
naming pattern: `adr-YYYY-MM-DD.md`.

The ADR format has five parts described below.

**Title** Short descriptive summary prefixed with `ADR YYYY-MM-DD: `

**Context** Description of the current situation, relevant background,
what the problem is that we're aiming to solve, why it's a problem,
and other fact based details describing the current state of the
technology, teams, and project. Nygard calls these the "forces at
play" and recommends keeping the language value-neutral.

**Decision** What we are going to do to address the situation
described in the Context section. Use active voice (e.g. "We
will...").

**Status** An ADR starts off as "proposed". After discussion the ADR
will have status "accepted" or "declined". If we revisit an issue in
the future, we'll do so with a new ADR. This may cause a previously
"accepted" ADR to be updated to "deprecated" or "superseded".

**Consequences** Describe the resulting context after applying the
decision. We will set a check-in date to help us remember to return to
accepted ADRs and reflect on the impact (good and bad) of the
decision.

[blog post]: http://thinkrelevance.com/blog/2011/11/15/documenting-architecture-decisions


## Consequences

