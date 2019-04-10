# ADR 2018-10-08: Architecture decisions - modus operandi

## Context

As we continue to improve and evolve Chef’s product architecture, we are in a position where there is a need to evolve the process itself of how architectural decisions get made and to expand the participants and seek input from a wider team. 
The important goals we are trying to accomplish via this process are:

1. More transparency and participation from everyone - including:
    1. Proposing new architectural changes / patterns
    2. An opportunity for anyone interested to chime in with their thoughts
    3. Having this decision be known to all parts of the organization for future reference
2. A more streamlined decision making process and tracking
3. Clarity on who makes decisions

Below is the process we are going to follow going forward.


## Decision
The following is the process:

1. <Optional step> If you have a topic that should be considered for an architecture discussion, write a google document describing the need for the decision, the options evaluated, the recommended decision, etc
    * The purpose of this document is to give enough context and information to proceed with the discussion. 
    * Note that there is no prescribed format for this document at this time. Just make sure that all relevant information that you can think of is captured
    * While we want to get as much information written down as possible, please don't get blocked by trying to document everything you can think of. This is really meant to get the conversation started
2. Once some initial discussions have happened and you are ready to bring this to the architecture team, follow this [ADR process][1] and create a new ADR with status Proposed
3. Create a PR for the topic in the A2 repository. 
    * We are limiting this process to A2 for the time being.
4. Drop a link to the PR in the #platform-architecture slack channel so that everyone becomes aware
We will automate this soon
5. The architecture team will then discuss all topics that have come in over the period of the week in the next architecture meeting
    * An agenda for each week’s topics will be posted a day before so people can come prepared
6. The person requesting a review will be invited to the meeting to present their topic and discuss.
    * This will be an open meeting and anyone who has input and  wants to join are welcome to the meeting
7. The architecture team and others are expected to come in having reviewed the content and ready with their questions and thoughts
8. We will discuss the topic and hopefully reach a decision right in that meeting. 
9. If further discussions are required, we will indicate that as next steps and reconvene at the earliest for follow ups
10. If there is no clear decision or if there are differing opinions, the engineering directors (Christoph Hartmann, Seth Falcon, Sudhir Reddy) will be responsible for resolving these and helping the team with a decision
11. NOTE that if a particular topic does not align with company or engineering strategy or is something not even worth considering, it is the Directors’ responsibility to indicate and articulate this as early in the process as feasible.
12. Here is a link to the [ADR process][1] set forth to document decisions
13. There are a lot of good practices documented here on [decision making][2] which we’ll use as appropriate over time.


[1]: https://github.com/chef/automate/blob/master/dev-docs/adr/adr-2018-08-15.md
[2]: https://medium.com/@barmstrong/how-we-make-decisions-at-coinbase-cd6c630322e9

## Status

* Accepted (2018-10-09)

## Consequences

The Chef prod-eng org will follow this process going forward for all architectural discussions and decisions. Thereby, every person who would like to participate in these decisions will have an opportunity. This is expected to lead to better architectural outcomes and decisions.
