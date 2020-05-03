+++
title = "Desktop"
description = "Using the Desktop View"
date = 2018-03-26T16:01:47-07:00
draft = false
bref = ""
toc = true
+++

## Overview
The _Desktop_ page displays information about all desktops connected to Chef Automate. It can be found via the top nav of your Automate instance, at `automate-url/desktop`
Desktops appear in this view after a Chef Infra Client run has executed.

### Daily Check In
Shows a count of desktops that have and are reporting in to Automate. Desktops that have not checked in in the last twenty-four hours will be counted as `unknown`.
Selecting a status filter opens the list view of the desktops (Insight) with the selected status.

### Check In History
Shows the history of checked-in and unknown desktop counts over time. Timescale can be adjusted using the timescale dropdown menu.

### Top Errors
Shows the most common errors that have occurred across all desktops over the last twenty-four hours, and a count of the machines that incurred the error.
Selecting an error opens the list view of the desktops (Insight) with the error to enable further investigation.

### Unknown Duration
Shows count of desktops that have been `unknown` (have not reported in to Automate) for the given time frames.

### Insight (Desktop List)
Shows a list of desktops. Available filters found at the top of the list. Additional filtering attributes may be selected using the `Choose Attributes` dropdown menu.

### Node Detail
Shows details of a single desktop and a graph displaying the latest status per day for the desktop (failed, converged, or unknown).

### Configuration
The Automate install for the desktop experience requires the use of a flag, `--with-desktop`.
When starting Automate with the desktop flag, data lifecycle configurations will be set to never mark nodes as missing and never delete missing nodes. Users are encouraged to not modify these settings.

### Other Useful Information
The desktop view does not support project filtering.
No compliance profiles are supported for the Desktop view; they will not be included when using the desktop flag.
Liveness agents are not excluded from node counts in the desktop view.