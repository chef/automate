+++
title = "Event Feed"
description = "Using the Event Feed"
date = 2018-03-26T16:01:47-07:00
draft = false
bref = ""
toc = true
+++

## About the Event Feed

Use the event feed for actionable insights and operational visibility.
The guitar strings and improved query language help you drill into infrastructure & compliance automation events quickly to isolate errors.

## Event Guitar Strings

![Guitar Strings](/images/docs/guitar-strings.png)

The event guitar strings is a timeline representing Chef Server and Compliance events. The events are separated into create, update and delete. Blue circles show create events, red circles show delete events, and purple diamonds show update events. The icons inside of the shapes represent different types of events, and a multi-event icon denotes a case where multiple events happened within the same 4-hour window. Hovering over the icon shows a text box summarizing the events for that 4-hour window.

## Icon legend

![Event feed icon legend](/images/docs/event_icons.png)

## Event feed

The event feed shows the time of the event, its type, the object acted on, the action, and the initiating action.

![Event feed events](/images/docs/event-feed-events.png)

You can see additional information depending on the event type.

## Grouped events

In the event feed, events of the same type by the same user are grouped. The event list entry shows how many events are grouped into an individual entry, which is also a link. Selecting this link opens a side panel showing the details of the collapsed events.

## Filtering events

To filter the event feed and event timeline by event type, Chef Server or Chef Server organization use the search bar. Available event type filters are clients, cookbooks, data bags, environments, nodes, policyfiles, profiles, roles, and scan jobs.
![Event feed filters by type](/images/docs/event-feed-filters-types.png)

To filter by Chef Server or Chef Server organization select the Chef Server or Chef Organization in the search bar and start typing the name. You cannot filter compliance events--profiles and scan jobs--by organization or Chef Server. Compliance events are not visible when either of these filters are applied.

![Event feed filters by server](/images/docs/event-feed-filters-servers.png)

You can also filter the event feed event timeline by day or set of days within the past week. The event feed defaults to show all events in the past week. Moving the indicators as shown below to the right or left will activate the filter. You can move the indicators back to their start position or use `SHIFT + R` to reset the time scale.
![Event feed filters by day](/images/docs/event-feed-filters-days.png)
