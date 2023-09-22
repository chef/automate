+++
title = "Large Compliance Report Ingestion"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Large Compliance Report Ingestion"
    parent = "automate/configure"
    identifier = "automate/configure/large_compliance_report.md Large Compliance Report Ingestion"
    weight = 70
+++

{{< note >}}

- The Large Compliance Report is supported from Chef Automate Version **4.2.x** and above.
- The Large Compliance Report feature is not available for Chef Automate High Availability (HA) as of now.

{{< /note >}}

Automate fails to ingest Compliance reports larger than 4MB sent through the data collector endpoint. Chef Automate data collector sends the error back to the client.

The following change in architecture enables Automate to ingest reports larger than 4MB without failure.

![LCR Architecture](/images/automate/lcr_architecture.jpg)

Automate with the configuration to allow ingestion of an extensive compliance report and allow the data to be sent to the OpenSearch data and in an externally deployed MinIO Service. Automate in the configuration expects that a MinIO Server is running externally to Automate ecosystem, which Automate can connect and transact to.

Automate with the configuration will enable Automate to:

- Allow ingestion of compliance reports larger than 4MB,
- Exporting or downloading complete reports larger than 4MB in size.

This change has implications for the performance of Automate data ingestion because:

- Automate processes much more extensive data than the usual.
- Automate uploads the data to an external service over a network.

The impact depends on different factors like network configuration and machine configuration.
Here is a benchmark test summary report run on

- Instance Type: **16 vCPU, 30 GB memory**
- Number of Compliance Targets: **20K**
- **OpenSearch** and **Postgres** are deployed internally

| Report Size | Supported Concurrency |
| --- | --- |
| 1 MB | 100 |
5 MB | 50 |
8 MB | 30 |
12 MB | 20 |

## Prerequisites

{{< note >}} Automate installation does not include MinIO server. {{< /note >}}

- An external MinIO server needs to be set up and available to connect

### MinIO

MinIO is a High-Performance Object Storage released under GNU Affero General Public License v3.0. It is API compatible with the Amazon S3 cloud storage service. MinIO is the only object storage suite available on public cloud, every Kubernetes distribution, the private cloud, and the edge.

Click [here](https://min.io) to learn more about MinIO and [set up](https://docs.min.io/minio/baremetal/) the MinIO on a baremetal server.

## Enable Automate to Ingest Large Compliance Report

To enable Automate to ingest Large Compliance reports:

1. Create a `patch.toml` if one does not already exist for your Chef Automate installation.

2. Add the following configuration to the `patch.toml` file:

```toml
[global.v1.external.minio]
    ##Do not add the protocol(http or https) for MinIO server endpoint. ex. mydomain.com:1234
    endpoint = "<minio server end point>:<port>"
    root_user = "<username>"
    root_password = "<password>"

    ## Uncomment and fill out if want to modify the number of workers to run in parallel to communicate with OpenSearch for preparing the doc for export, default value is 50
    #concurrent_open_search_requests = 50
    ## Uncomment and fill out if want to modify the number of workers to run in parallel to communicate with MINIO server for preparing the doc for export, default value is 10
    #concurrent_minio_requests = 10
    ## Uncomment and fill out if external MINIO is configured with SSL
    #enable_ssl = true
    #cert = """$(cat </path/to/cert_file.crt>)"""

[global.v1.large_reporting]
    enable_large_reporting = true

## Uncomment and fill out if you want to modify the bucket name used to store data in the MinIO server; the default name is 'default'.
## If MINIO is configured to use any public cloud platforms like S3, Azure blob storage, or Google Cloud Storage, It is mandatory to modify the bucket name other than `default`
#[report_manager.v1.sys.objstore]
#  bucket = "default"
```

3. Patch the config by running the following command:

```toml
`chef-automate config patch patch.toml`
```

## AWS S3 as Object Storage

{{< warning >}}  The below configuration is not tested to determine benchmark numbers. We recommend doing benchmark testing before considering the approach. {{< /warning >}}

Automate can connect to AWS S3 for extensive compliance reports if you reuse the MinIO Configuration in the following manner:

```toml
[global.v1.external.minio]
        ## endpoint has to be s3.amazonaws.com
        endpoint = "s3.amazonaws.com"
        root_user = "<access_key>"
        root_password = "<secret_key>"
```
