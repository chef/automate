+++
title = "Desktop Dashboard"
description = "Using the Desktop Dashboard"
date = 2018-03-26T16:01:47-07:00
draft = false
bref = ""
toc = true
+++

The Chef Automate _Desktop_ dashboard displays status information about all desktops connected to Chef Automate.
Desktop information populates this dashboard after a Chef Infra Client run has executed.

## Setting Up the Desktop Dashboard

Enable the Desktop dashboard with: `chef-automate deploy --product automate --product infra-server --product desktop`.
For more information, see [Install Chef Infra Server with Automate](https://automate.chef.io/docs/infra-server/).
The Desktop dashboard has no supported compliance profiles, and installation with the `--product desktop` flag includes no compliance profiles.

{{< info >}}
When installing Chef Automate with the `--product desktop` flag, _Data Lifecycle_ settings will set by default to not mark nodes as missing and to not delete missing nodes.
We encourage users to not change these specific settings, and not defeat the monitoring purpose of the Desktop dashboard.
{{< /info >}}

## Desktop Dashboard Display

Within Chef Automate, the _Desktop_ dashboard uses four panels to summarize information: _Daily Check-in_ _Check-in History_, _Top 10 Errors_, and _Time since Last Check-in_.
Selecting rows within these displays will list relevant desktops and selecting an singular desktop reveals its detailed information.

The _Desktop_ dashboard does not support project filtering.
Node counts in the _Desktop_ dashboard may include liveness agents.

### Daily Check-in

The _Daily Check-in_ display shows a top-level view of daily desktop check-in statistics.  
A bar graphic illustrates the proportion of desktops with `unknown` and `checked-in` statuses.
Below the bar, boxes displays counts for desktops with `unknown` and `checked-in` statuses, and a total count of desktops.
`Checked-in` refers to desktops reporting into Chef Automate.
`Unknown` desktops did not report to Chef Automate during the last twenty-four hours.

### Check-in History

_Check-in History_ shows the history of checked-in desktop counts as graphed over a selected period of time.
Adjust the timescale with the timescale drop-down menu and change the graph display.
The timescale options are "Last 3 Days," "Last 7 Days," and "Last 14 Days".

### Top 10 Errors

The _Top 10 Errors_ display shows the ten most common errors that have occurred across all desktops over the last twenty-four hours, and a count of the machines that experienced each error.
Selecting an error opens the Filter Desktop List with the applied error filter to enable further investigation.

### Time Since Last Check-in

The _Time since Last Check-in_ display shows a count of desktops with an `unknown` status for defined timefames.
Selecting a row will reveal a Filtered Desktop List of desktops for the selected timeframe.

### Filtered Desktop Lists

The Filtered Desktop List displays a list of desktops filtered according to the selected timeframe or error in the previous display.
Selecting rows in either _Time since Last Check-in_ or _Top 10 Errors_ activates this summary list.

Apply more filters to this initial list if desired.
Specific filter values populate according to available desktop information and include the categories of "platform," "version," "domain," and "last run status."

Select an individual desktop row to display the individual desktop's node details.

#### Node Details

The _Node Details_ display shows the details of a single desktop and includes a visualization of its check-in history.

The individual check-in history displays the desktop's latest status for each day with an associated icon by default.
Statuses include "converged" (check box icon), "unchanged" (dash icon), "error" (exclamation point icon), or "unknown" (question mark icon).
The check-in history's format can appear in grid icon, status list, or selectable JSON form.
Select the gear icon at the top of the display to make these modifications.
The timescale of the check-in history can also switch between "Last 2 Weeks" and "Last 4 Weeks."

The individual desktop's information includes a detailed overview, Chef Infra Client, System, Virtualization, and Kernel.
