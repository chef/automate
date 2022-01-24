Automate DataCollector URL is a HTTP endpoint to be used by external software like Chef Server/Inspec/Infra Clients to send different types of data to Automate. This data after coming to the Data Collector HTTP end point is sent to the compliance-service through a GRPC call. This data then goes through a pipeline where the data processed and finally stored to Elastic Search.

# What is Large Compliance Report issue in Automate?
There are several issues:

* 4MB GRPC Limit
* Displaying Large Report on UI
* Exporting/Downloading Large Report from UI and API

# How can we solve it?
* The data will be streamed in chunks from the DC endpoint to the compliance service.
* Trim the report to the bare minimum and store the data in ES.
* Store the control messages in another index and fetch them on demand.

# Architecture Diagram
The following picture illustrates the complete architecture:

![Architecture](https://raw.githubusercontent.com/chef/automate/sahiba/lcr_documentation/Markdown/architecture.png)

# Config



