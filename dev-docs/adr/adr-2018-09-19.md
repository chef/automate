# ADR 2018-09-19: Data Resilience on ingest

## Context

We want to ensure that the Data resilience (this is what we are calling the guarantee that data once sent to the ingest pipeline will be ingested into A2) in our system is well understood. We also want to make sure that this is captured here for future reference.

## Decision

1) Data resilience / durability - our current approach on leaving retries to the edges (Chef Infra Client, Chef Infra Server, Chef InSpec) is still valid and we will not change that at this time. However, there is no retry logic built into Chef Infra Server or Chef InSpec. We will add that to those products' backlog and triage according to those teams' priorities.
2) For Notifications right now, this is built in a way where notifications might get triggered but the data has not yet been fully ingested. We will change that behavior so that notifications only get triggered upon successful ingestion of data
3) There is an event service in compliance right now that we all agreed can be extracted and enhanced to use across the system. However, the right time to look at this is when there is a new use-case for it. While the approach is agreed upon, we will await a use-case before proceeding

## Status

* Accepted (2018-09-19)

## Consequences

The system will behave as above in terms of data consistency. We may need to revisit this as / when / if the requirement to guarantee 100% data resilience (no data loss once sent to ingest) changes.
