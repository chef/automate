+++
title = "Reports"
description = "The Compliance Dashboard"
date = 2018-03-26T16:02:09-07:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "compliance"
    weight = 10
+++

The _Reports_ page (Compliance > Reports) provides comprehensive insight into the compliance status of all scanned infrastructure.

{{% info %}}
The control count in the _Profile Status_ pane (Reports > Profile Status) shows the total number of executed controls, not the total number of unique controls.
{{% /info %}}

![Reports](/images/docs/reports.png)

### Dates in Compliance Reports

The dashboard shows the results of all scans with end times on the _currently selected day_.
The currently selected day, as highlighted in the search bar, bases itself on timestamps in Coordinated Universal Time (UTC).

The trend graph provides a historical overview of node status over time.
You can change it to display overviews of the past 10 days, 1 month, 3 months, or 1 year.

To view scan reports in the past, users can select a different date from the calendar located in the search bar.

![Reports Date Selector](/images/docs/reports-date.png)

### Compliance Data Sources

Users with audit cookbook configurations can expect to see the results of those scans land in this view.
The results of any scan jobs executed in Chef Automate also land in this view.

### Search Bar and Filters

The Compliance search allows you to view and filter compliance scan results based on a defined set of filters.
Wildcard searches on field values will not return suggestions, but they will filter the results.
For example, you can search for `Node Name: prod*` and filter the results to list all the node names that begin with `prod`.

Profile
: Filter your results by profile name. This lists executed profiles, which are profiles with received scan results.

Node
: Filter your results by node item. A node is any scannable for which we have received scan results, which can be a Chef node, an aws node, an aws account region, or any other target Chef InSpec supports.

Platform
: Filter your results by platform.

Environment
: Filter your results by environment.

Control
: Filter your results by control name. Filtered results display the status of nodes that ran the selected control.

Role
: Filter by Chef role, on all applicable nodes.

Recipe
: Filter by Chef recipe, on all applicable nodes.

### Deep Filtering

Deep Filtering allows you to view the state of your infrastructure from the perspective of a single profile, or a single profile and one of its child controls.
When you add a profile filter, it changes the compliance reports to show the status of the nodes that run that selected profile.
Adding a child control filter narrows the profile results to show the status of the nodes that run the selected control from that profile.

Please note the only supported filters for deep filtering are:

    * one profile
    * one profile and one of its child controls

### Waivers

A node's waived status appears if applicable in displays where a node's status appears in Chef Automate.
The Compliance Reports overview displays the node count and history of waived nodes, and the count and history of waived controls.
_Nodes_ and _Profiles_ views include _Waived Nodes_ and _Waived Profiles_ status filters respectively.
Select the _Waived_ status filter to display only the respective Node or Profile reporting with that status.
Hover over the control's Waived icon under the Node Status column in _Controls_ to view more details about the waiver applied to the control.

Use Chef InSpec to configure [waivers](https://docs.chef.io/inspec/waivers/).

### Download Report Results

The download button located to the right of the search bar allows the user to download a JSON or CSV format of the reports, based on all currently applied filters (including end time selected in calendar).

### Compliance Reports Results and Job ID Filters

Chef Automate users can see the results of their scan jobs from https://{{< example_fqdn "automate" >}}/compliance/scan-jobs/jobs by selecting _Report_, which will redirect to the compliance tab.
This Reports view uses the _job id_ filter as well as any additional filters, and ignores the _end time_ filter.

![Getting to Compliance from a Scan Job](/images/docs/compliance-jobid.png)

## Compliance Status and Report Metadata

The _Compliance Status and Report Metadata_ bar is directly beneath the search bar.
Expand the `Report Metadata` information by selecting the compliance status bar. The report metadata shows a summary of the nodes, report date, duration, status, number of platforms, number of environments, and number of profiles used in your scan.

![Reports Metadata](/images/docs/reports-metadata.png)

## Compliance Overview

The Compliance Overview provides insights into the status of your system.
Toggle between _Node Status_ and _Profile Status_ to view your system's overall compliance.

### Node Status

The _Node Status_ view provides insight into your system's compliance status from the operational perspective of nodes.

Node Status
: Shows the number of nodes that passed or failed compliance scans, and the number of skipped or waived nodes.

Severity of Node Failures
: Shows the severity of the compliance scan failures on your system's nodes.

Node Status Over Time
: Shows the changes in size and compliance status of your system over time. Use the dropdown menu in the upper left corner to change the date range. Changing the date range also changes the displays in related charts and tables.

Top Platform Failures
: Shows the number of compliance scan failures grouped by operating system. Hovering over an individual bubble shows the platform name and the number of impacted nodes.

 Top Environment Failures
: Shows the number of compliance scan failures grouped by environment. Hovering over an individual bubble shows the environment name and the number of impacted nodes.

### Profile Status

The _Profile Status_ view provides insight into your system's compliance status from the compliance perspective of Compliance profiles run during scans.

Control Status
: Shows the number of controls in your system, grouped by passing, failing, and skipped controls.

Severity of Control Failures
: Shows failed controls, ranked by number and importance of the control failure, grouped as critical, major, and minor.

Control Status Over Time
: A line graph showing the number of controls and Compliance scan results over time. Use the calendar button in the upper right corner of the chart to change the time frame.

Top Profile Failures
: Indicates the profiles with the highest failure rate. Hover over an individual bubble to show the full profile name and the number of impacted nodes.

Top Control Failures
: Indicates the most frequently failing controls. Hover over an individual bubble to show the control name and the number of impacted nodes.

## Switching Views

Switch your views by selecting the appropriate tabs and see compliance report results from the perspective of _Nodes_, _Profiles_, and _Controls_.

### Nodes

The _Nodes_ view provides more detailed insight into the compliance status of the nodes in your system.
Sort this table by node name, platform, environment, last scan, and the number of control failures from the most recent compliance scan.
Scroll to the bottom of the page for pagination navigation options.

![Reports Nodes](/images/docs/reports-nodes.png)

Node
: A node is any machine that is under management by Chef.

Platform
: The operating system on your node, such as AIX, Amazon Linux, Apache, macOS, CentOS, Oracle Linux, Oracle Solaris, RHEL, SUSE Linux, Ubuntu, and Microsoft Windows.

Environment
: You can filter compliance reports by the environments in any stage of your workflow.

Last Scan
: Time in hours, days, or months since the last scan on that node.

Control Failures
: Shows the number of failing controls, if any.

Filter
: Select the filter icon on the right side of the row to select a node.

More Options
: Select the (...) icon to display a menu with two additional options: _Add Filter_ and _Scan Results_. Use _Add Filter_ to narrow the results to the `id` of the selected node. Use _Scan Results_ to show the scan results of controls for the specific node in a side window.

### Profiles

Use the **Profiles** tab to examine the compliance profiles installed under your individual user account.

![Reports Profiles](/images/docs/reports-profiles.png)

Profile Title
: The name of the profile obtained from the _Profile Store_ or uploaded.

Version
: The semantic version identifier of the profile; you may need to update your profiles to get the most recent version.

Identifier
: The name under which the profile is installed and a short profile title.

More Options
: Select the (...) icon to display a menu with two additional options: _Add Filter_ and _Scan Results_. Use _Add Filter_ to narrow the results to the `id` of the selected profile. Use _Scan Results_ to show the scan results of controls for the specific node in a side window.

### Controls

Use the **Controls** tab to examine the compliance controls installed under your individual user account.

![Reports Controls](/images/docs/reports-controls.png)

Control Name
: Control name and short description of its purpose

Profile
: Profile containing the controls

Impact
: Importance of the control

Last Scan
: When the last scan occurred

Node Status
: Shows the number of failed, passed, and skipped nodes

More Information
: Select the (...) icon to display a menu with one additional option: _Add Filter_. Use _Add Filter_ to narrow the results to the `id` of the selected profile and to the `id` of the selected control.

### Individual Node Display

The node name is at the top of the header, directly above the node compliance status.

![Reports Node Detail](/images/docs/reports-node-detail.png)

The node history table displays the following information:

Control
: Control name and short description of its purpose

Severity
: Importance of the control

Root Profile
: Profile containing the controls

Test Results
: Number of tests within an individual control

More Information
: Select the plus icon to display a control description and to toggle between **Results** and **Source**. **Results** describes the node compliance status and **Source** displays the Chef InSpec control source code.

#### Scan History

Select **Scan History** in the upper right corner to open a side-window.
Choosing a compliance scan from this list redirects you to a view of all controls run during the selected scan.

![Scan History](/images/docs/reports-scan-history.png)
