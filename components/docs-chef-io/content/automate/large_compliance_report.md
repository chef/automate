+++
title = "Large Compliance Report Ingestion"

+++

Automate fails to ingest Compliance report larger than 4MB size sent through data collector endpoint.
Automate data collector sends back error to the client.

The following change in architecture enables Automate to ingest reports larger than 4MB without any failure.
![](../../static/images/automate/lcr_architecture.jpg)

Automate with the configuration to allow ingestion of a large compliance report allow the data not only to be sent to 
the OpenSearch data but also in an externally deployed Minio Service. Automate in this configuration expects that the 
a Minio Server is running externally to Automate ecosystem which Automate can connect and transact to.
  
Automate with the configuration will enable Automate to:  

- allow ingestion of compliance report larger than 4MB
- exporting or download complete report of larger than 4MB size

This change has implications in the performance of Automate data ingestion because:
- Automate has to process much larger data than the usual
- Automate has to upload the data to an external service over network

The impact depends on different factors like network configuration, machine configuration.
Here is a benchmark test report run on






### Prerequisite
{{< note >}}  
Automate installation does not include Minio server.  
{{< /note >}}

- An external Minio server needs to be setup and available to connect

#### Minio

MinIO is a High Performance Object Storage released under GNU Affero General Public License v3.0. It is API compatible with Amazon S3 cloud storage service. MinIO is the only object storage suite available on every public cloud, every Kubernetes distribution, the private cloud and the edge.  

Refer https://min.io for more details and https://docs.min.io/minio/baremetal/ instructions to setup Minio on a baremetal server.

### Enabling Automate to ingest Large Compliance Report

To enable Automate to ingest Large Compliance report:
1. Create a patch.toml if one does not already exist for your Chef Automate installation.
2. Add the following configuration to the `patch.toml` file:
```toml
[global.v1.large_reporting]                                
enable_large_reporting = true
```
3. Patch the config by running   
`chef-automate config patch patch.toml`

### 



