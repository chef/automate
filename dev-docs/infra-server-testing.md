## Context
Do Performance testing and benchmarking to capture the API response times for 
the infra proxy APIs

## chef infra-proxy upload file API for sync org and users
API endpoint - "api/v0/infra/servers/migrations/upload"

Tested the different size of zip files with similar CPU and memory requirement of Automate as 
    - 16 GB of RAM
    - 80 GB of disk space (available to /hab)
    - 4 vCPUs

The following details are captured through the API

    | File Size  |  API response time
    | 10 MB      |  514ms
    | 512 MB     |  30.61s
    | 1 GB       |  58.73s
    | 1.5 GB     |  1.4min
    | 2 GB       |  Error: 413 Request Entity Too Large

The maximum size of zip file we can use is 1.5GB
