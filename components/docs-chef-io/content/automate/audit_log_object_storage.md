+++
title = "Store Audit Logs in S3 or MinIO"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Audit Logs in S3/MinIO"
    parent = "automate/configuring_automate"
    identifier = "automate/configuring_automate/Audit Logs in S3 or MinIO"
    weight = 43
+++

You can configure Chef Automate to use an object storage backend (AWS S3 or MinIO) for audit log storage.
This is helpful when you want audit log data to live outside the local filesystem and to integrate with object-storage-based retention and access controls.

## How audit logging works

When audit logging is enabled, Chef Automate does the following:

- The load balancer writes `POST`, `PUT`, `PATCH`, and `DELETE` HTTP operations to `/hab/svc/automate-load-balancer/data/audit.log` and rotates the file based on audit input settings.
- If audit storage is configured, Fluent Bit tails the active `audit.log` file and uploads matching log entries to S3 or MinIO.

## Prerequisites

- Network connectivity from Chef Automate services to your AWS S3 or MinIO endpoint
- An existing bucket for audit logs
- Credentials for the storage bucket, or an IAM role and instance profile when using AWS

## Quick start

Use this section if you want the minimum configuration to start uploading audit logs to your bucket.

To create the minimum configuration for uploading audit logs, follow these steps:

1. Create a TOML file that defines your storage settings.

   {{< foundation_tabs tabs-id="quickstart-storage-example-aws-s3-minio" >}}
     {{< foundation_tab active="true" panel-link="quickstart-storage-example-aws-s3" tab-text="AWS S3 storage">}}
     {{< foundation_tab panel-link="quickstart-storage-example-minio" tab-text="MinIO storage" >}}
   {{< /foundation_tabs >}}

   {{< foundation_tabs_panels tabs-id="quickstart-storage-example-aws-s3-minio" >}}
   {{< foundation_tabs_panel active="true" panel-id="quickstart-storage-example-aws-s3" >}}

    AWS S3 using an IAM role and instance profile:

    ```toml
    [global.v1.audit]

      [global.v1.audit.logging]
        enabled = true

      [global.v1.audit.storage]
        storage_type = "s3"
        endpoint = "<STORAGE_ENDPOINT>"
        bucket = "<BUCKET_NAME>"
        storage_region = "<AWS_REGION>"
    ```

    Replace the following:

    - `<STORAGE_ENDPOINT>` with your AWS S3 storage endpoint, for example `https://s3.amazonaws.com` or `https://s3.us-east-1.amazonaws.com`.
    - `<BUCKET_NAME>` with the S3 bucket.
    - `<AWS_REGION>` with the AWS regions, for example `us-east-1`.

   {{< /foundation_tabs_panel >}}

   {{< foundation_tabs_panel panel-id="quickstart-storage-example-minio" >}}

    MinIO using static credentials:

    ```toml
    [global.v1.audit]

      [global.v1.audit.logging]
        enabled = true

      [global.v1.audit.storage]
        storage_type = "minio"
        endpoint = "<MINIO_ENDPOINT>"
        bucket = "<BUCKET_NAME>"
        storage_region = "<REGION>"
        access_key = "<ACCESS_KEY>"
        secret_key = "<SECRET_KEY>"

        [global.v1.audit.storage.ssl]
          enabled = false
          verify_ssl = false
    ```

    Replace the following:

    - `<MINIO_ENDPOINT>` with your MinIO endpoint, for example `http://minio.example.com:9000`.
    - `<BUCKET_NAME>` with the MinIO bucket.
    - `<REGION>` with the MinIO region, for example `us-east-1`.
    - `<ACCESS_KEY>` with your MinIO access key.
    - `<SECRET_KEY>` with your MinIO secret key.

   {{< /foundation_tabs_panel >}}
   {{< /foundation_tabs_panels >}}

   For complete field descriptions and defaults, see [audit log settings reference](#audit-log-settings-reference).

1. Patch the Chef Automate configuration:

    ```bash
    sudo chef-automate config patch </PATH/TO/TOML/FILE> [flags]
    ```

    In a Chef Automate High Availability (HA) deployment, use the following flags:

    - `--automate` to patch the Automate node.
    - `--chef_server` to patch the Chef Infra Server node.

    For more information, see the [`chef-automate config patch` documentation](/automate/cli/#chef-automate-config-patch)

    After you patch the configuration, Automate starts running the audit log collector and uploads audit log data to the configured bucket.

1. Optional: Once audit logs are uploaded, you can optionally configure the following:

   - a prefix inside the bucket to group objects
   - TLS options enable and verify a `root_cert` for private certificate authorities
   - local log rotation and tail input tuning
   - batching and timeouts
   - worker and queue tuning

For details on each field, see [audit log settings reference](#audit-log-settings-reference).

## Verify audit log configuration

To verify audit log uploads, follow these steps:

1. Confirm the configuration was applied:

    ```shell
    chef-automate config show | sed -n '/\[global\.v1\.audit\]/,/^\[/p'
    ```

1. In your configured S3 or MinIO storage bucket, confirm new objects are being written.

    If you don't see new objects right away, wait at least the configured `upload_timeout` (or reduce it temporarily) and ensure `total_file_size` is reachable for your traffic volume.

## Configure audit logs

### Configure local audit log rotation

Chef Automate rotates the local audit log file written by the load balancer.
To change the rotation size threshold, patch your Automate configuration.

1. Create a TOML file with the following content:

    ```toml
    [global.v1.audit]

      [global.v1.audit.input]
        max_file_size = "<FILE_SIZE>"
    ```

    Replace `<FILE_SIZE>` with the maximum size of the local audit log file before rotation occurs.

    {{< note >}}
    If you also set `[global.v1.audit.input].refresh_interval` or `[global.v1.audit.input].mem_buf_limit`, those values are passed through to Fluent Bit's Tail input (`Refresh_Interval` and `Mem_Buf_Limit`).

    See the [Fluent Bit Tail input documentation](https://docs.fluentbit.io/manual/pipeline/inputs/tail).
    {{< /note >}}

    Rotation behavior:

    - Audit entries are written to `/hab/svc/automate-load-balancer/data/audit.log`.
    - When `audit.log` exceeds `max_file_size`, it is rotated to `audit.1.log`.
    - Older rotated files are shifted up (`audit.1.log` -> `audit.2.log`, etc.).
    - Chef Automate keeps up to 10 rotated files (`audit.1.log` through `audit.10.log`).

1. Patch the Chef Automate configuration:

    ```bash
    sudo chef-automate config patch </PATH/TO/TOML/FILE>
    ```

### Configure upload behavior

These settings control object size splitting, multipart chunk size, and upload timeouts for collector uploads to S3 or MinIO.

{{< note >}}

Audit logs are uploaded asynchronously and aren't available in S3 or MinIO immediately after an event occurs.
In general, new objects appear after the collector reaches either the `upload_timeout` threshold or the `total_file_size` threshold, whichever happens first, subject to multipart `upload_chunk_size` behavior.

{{< /note >}}

If you don't set `[global.v1.audit.output]`, Chef Automate uses these defaults:

- `total_file_size = "100M"`
- `upload_chunk_size = "6M"`
- `upload_timeout = "10m"`

1. Create a TOML file with the following content:

    ```toml
    [global.v1.audit]

      [global.v1.audit.output]
        total_file_size = "100M"
        upload_chunk_size = "6M"
        upload_timeout = "10m"
    ```

    The `[global.v1.audit.output]` settings control the Fluent Bit S3 output plugin behavior.
    For details and constraints, see the [Fluent Bit S3 output plugin documentation](https://docs.fluentbit.io/manual/pipeline/outputs/s3).

    Set the following values:

    - `total_file_size`: Total size threshold before the output is split into additional objects.
    - `upload_chunk_size`: Multipart upload part size.
    - `upload_timeout`: Upload timeout.

1. Patch the Chef Automate configuration:

    ```bash
    sudo chef-automate config patch </PATH/TO/TOML/FILE>
    ```

## Troubleshooting

- If uploads fail to MinIO with TLS enabled, verify the endpoint scheme (`http://` vs `https://`) matches the `ssl.enabled` setting.
- If you use a private CA for MinIO, provide `root_cert` and set `ssl.enabled = true`.
- If you use AWS IAM roles, omit `access_key` and `secret_key` to use the default AWS credential chain.

## Audit log settings reference

The following defines the default audit log storage settings.
For a complete set of log storage settings, see the [reference examples](#audit-log-configuration-file-examples) below.

`[global.v1.audit.logging]`

: Audit logging is disabled by default:

  ```toml
  [global.v1.audit.logging]
    enabled = false
  ```

: Configure whether logging is enabled with the following:

  `enabled`

  : Default value: `false`

    Must be `true` or `false`.

  : Audit validation is only enforced if `enabled` is `true`.
    If audit logging is disabled (`false`), Automate doesn't validate the audit `storage`, `input`, `output`, and `async` fields.

`[global.v1.audit.async]`

: Audit log generation settings have the following defaults:

  ```toml
  [global.v1.audit.async]
    max_concurrent_workers = 4
    queue_size = 100
    multipart_chunk_size = "10MB"
  ```

: `max_concurrent_workers`
  : Default value: `4`

    If set, must be between `1` and `100`.

    Higher values increase throughput but also CPU and memory usage.

: `queue_size`
  : Default value: `100`

    If set, must be between `1` and `10000`.

    If full, new requests may be rejected.

: `multipart_chunk_size`
  : Multipart upload part size.

    Must be a size in `M` or `G` (for example, `"10M"`, `"1G"`). Must be between 10 MB and 1 GB.

    Default value: `"10MB"`

    Format: `KB`, `MB`, or `GB` suffixes (use `"20MB"`, not `"20M"`).

`[global.v1.audit.input]`

: The audit log input settings have the following defaults:

  ```toml
  [global.v1.audit.input]
    max_file_size = "10MB"
    refresh_interval = "60"
    mem_buf_limit = "5M"
  ```

: `max_file_size`
  : The maximum size of the local audit log file before rotation occurs.

    Default value: `"100MB"`

    If set, must be greater than or equal to 1 MB using the following units: `K`/`KB`, `M`/`MB`, or `G`/`GB`. For example, `"500K"`, `"10M"`, `"1G"`, `"10MB"`. An empty string is invalid.

    The load balancer rotates the audit log file `/hab/svc/automate-load-balancer/data/audit.log` and keeps 10 rotated files.

: `refresh_interval`
  : Polling interval for Fluent Bit Tail input. If set, must be a positive integer number of seconds.

    Default value: `"60"`

    Positive value (seconds)

: `mem_buf_limit`

  : The in-memory buffer limit.

    Default value: `"5M"`

    Format: If set, must be a size in `M` without a suffix (for example, `"5M"`). An empty string is invalid.

`[global.v1.audit.storage]`

: The audit log storage settings have the following defaults:

  ```toml
  [global.v1.audit.storage]
    storage_type = "s3"
    endpoint = "https://s3.amazonaws.com"
    storage_region = "us-east-1"
    path_prefix = ""
  ```

: `storage_type`
  : Default value: `"s3"`

    Must be `"s3"` or `"minio"`

: `endpoint`
  : Required for S3 when `bucket` is set, and always required for MinIO. For AWS S3, use a regional endpoint (for example, `https://s3.<AWS_REGION>.amazonaws.com`). For `us-east-1`, `https://s3.amazonaws.com` also works.

  Default value: `"https://s3.amazonaws.com"`

    The AWS S3 or MinIO endpoint URL.

: `bucket`
  : Default value: n/a

    Required. Uploads don't run unless a bucket is configured.

: `storage_region`
  : Default value: `""`

    Required for AWS S3 when `bucket` is set. For MinIO, use `"us-east-1"`.

: `path_prefix`
  : Default value: `"audit-logs"`

    Optional key prefix inside bucket

: `access_key`
  : Default value: `""`

    - For MinIO: required
    - For AWS: optional if using an IAM role or instance

: `secret_key`
  : Default value: `""`

    Required if `access_key` is set.

`[global.v1.audit.storage.ssl]`

: The storage SSL settings have the following defaults:

  ```toml
  [global.v1.audit.storage.ssl]
    enabled = false
    verify_ssl = false
    root_cert = ""
  ```

: `enabled`
  : Default value: `false`

    If using an HTTPS endpoint, set to `true`. If using HTTP endpoint, set to `false`.

    If MinIO uses a private certificate authority or self-signed certificate, set to `true` and add PEM file contents to `root_cert`.

: `verify_ssl`
  : Default value: `false`

    Set to `true` if `enabled = true` and `root_cert` is set.

: `root_cert`
  : Required if `enabled` is `true`.

    Default value: `""`

    PEM-encoded CA certificate (optional, for private CAs or self-signed certs).

`[global.v1.audit.output]`

: The audit log output settings have the following defaults:

  ```toml
  [global.v1.audit.output]
    total_file_size = "12M"
    upload_chunk_size = "6M"
    upload_timeout = "10m"
  ```

: The audit output settings control the [Fluent Bit S3 output plugin behavior](https://docs.fluentbit.io/manual/pipeline/outputs/s3).

: `total_file_size`
  : Total size threshold before the output is split into additional objects.

    Default value: `"12M"`

    Min value: `"1M"`

    Max value: `"50G"`

    Units: `M` or `G` only. Must be between 1 MB and 50GB, and must be greater than or equal to twice the `upload_chunk_size` value.

: `upload_chunk_size`
  : Default value: `"6M"`

    Min value: `"6M"`

    Max value: `"50M"`

    Units: `M` or `G` only. Must be between 6 MB and 50 MB.

: `upload_timeout`
  : Default value: `"10m"`

    Units: minutes only (`m`).

### Audit log configuration file examples

The following examples show all audit log settings in a single TOML file (logging, local rotation, S3 or MinIO storage, TLS settings, and upload behavior).
For a complete description of all settings, see [audit log configuration reference](#audit-log-settings-reference).

{{< foundation_tabs tabs-id="audit-log-configuration-examples" >}}
  {{< foundation_tab active="true" panel-link="audit-log-configuration-examples-default" tab-text="Default config">}}
  {{< foundation_tab panel-link="audit-log-configuration-examples-complete" tab-text="Complete config" >}}
{{< /foundation_tabs >}}

{{< foundation_tabs_panels tabs-id="audit-log-configuration-examples" >}}
{{< foundation_tabs_panel active="true" panel-id="audit-log-configuration-examples-default" >}}

The following TOML shows the default audit log settings:

```toml
[global.v1.audit]

  [global.v1.audit.logging]
    enabled = false

  [global.v1.audit.input]
    max_file_size = "100M"
    refresh_interval = "60"
    mem_buf_limit = "5M"

  [global.v1.audit.async]
    max_concurrent_workers = 4
    queue_size = 100
    multipart_chunk_size = "10M"

  [global.v1.audit.storage]
    storage_type = "s3"
    # Chef Automate does not fill in `endpoint` or `storage_region` automatically.
    # When you enable S3 uploads (by setting `bucket`), set `endpoint` and `storage_region` too.
    path_prefix = "audit-logs/"

    [global.v1.audit.storage.ssl]
      enabled = false
      verify_ssl = false
      root_cert = ""

  [global.v1.audit.output]
    total_file_size = "12M"
    upload_chunk_size = "6M"
    upload_timeout = "10m"
```

{{< /foundation_tabs_panel >}}

{{< foundation_tabs_panel panel-id="audit-log-configuration-examples-complete" >}}

The following example includes all available settings:

```toml
[global.v1.audit]

  [global.v1.audit.logging]
    enabled = true

  [global.v1.audit.input]
    max_file_size = "100M"
    refresh_interval = "5"
    mem_buf_limit = "5M"

  [global.v1.audit.async]
    max_concurrent_workers = 4
    queue_size = 100
    multipart_chunk_size = "10M"

  [global.v1.audit.storage]
    # Use "s3" for AWS S3 or "minio" for MinIO.
    storage_type = "<s3|minio>"
    endpoint = "http://www.example.com:9000"
    bucket = "audit-logs"
    storage_region = "us-east-1"
    path_prefix = "audit-logs/"
    access_key = "<ACCESS_KEY>"
    secret_key = "<SECRET_KEY>"

    [global.v1.audit.storage.ssl]
      # For HTTPS endpoints, set enabled to true
      # For HTTP endpoints, set enable to false
      enabled = false
      verify_ssl = false
      # For private CAs/self-signed certs, provide a PEM-encoded CA certificate.
      # root_cert = """-----BEGIN CERTIFICATE-----
      # ...
      # -----END CERTIFICATE-----"""

  [global.v1.audit.output]
    total_file_size = "100M"
    upload_chunk_size = "6M"
    upload_timeout = "10m"
```

{{< /foundation_tabs_panel >}}
{{< /foundation_tabs_panels >}}

## Retrieve audit logs

Chef Automate provides APIs through `user-settings-service` to request and download audit logs asynchronously.

### Authentication

All audit APIs require authentication.

All endpoints accept a bearer token (JWT) in the `Authorization` header:

```bash
curl -sS \
  -H "Authorization: Bearer <TOKEN>" \
  "https://<FQDN>/api/v0/audit/..."
```

Alternatively, you can use the `api-token` header in some API contexts.

{{< note >}}

`api-token` authentication works with the Admin request API, Status API, and Download API.
The Self request API doesn't accept `api-token` authentication; use a bearer (JWT) token for `GET /api/v0/audit/self/request`.

{{< /note >}}

Example using an API token header:

```bash
curl -sS \
  -H "api-token: $API_TOKEN" \
  "https://$FQDN/api/v0/audit/admin/request"
```

### Request administrator audit logs

- Method/Path: `GET /api/v0/audit/admin/request`
- Authentication: Required (Chef Automate admin session)
- Access control: Admin only
- Query parameters (all optional):
  - `usernames`: Comma-separated list of users to filter logs. If omitted, returns logs for all users.
  - `from`: Start time (RFC 3339 timestamp). Default: 3 hours ago.
  - `to`: End time (RFC 3339 timestamp). Default: now.
  - `order`: Sort order, `asc` or `desc`. Default: `desc`.
- Constraints:
  - The requested time range (`to` - `from`) must be 30 days or less.
  - If you omit both `from` and `to`, the request defaults to the last 3 hours.

Example:

```shell
curl -sS \
  -H "Authorization: Bearer <TOKEN>" \
  "https://<FQDN>/api/v0/audit/admin/request?usernames=user1,user2&from=2025-11-10T00:00:00Z&to=2025-11-11T00:00:00Z&order=desc"
```

Response:

```json
{
  "request_id": "f47ac10b-58cc-4372-a567-0e02b2c3d479",
  "status": "processing",
  "message": "Admin audit log generation started. Use the request ID to check status and download when ready."
}
```

### Request your own audit logs

You can request audit logs for your own activity.

- Method/Path: `GET /api/v0/audit/self/request`
- Authentication: Required (Chef Automate session)
- Access control: Self only
- Query parameters (all optional):
  - `from`: Start time (RFC 3339 timestamp). Default: 3 hours ago.
  - `to`: End time (RFC 3339 timestamp). Default: now.
  - `order`: Sort order, `asc` or `desc`. Default: `desc`.
- Constraints:
  - The requested time range (`to` - `from`) must be 30 days or less.
  - If you omit both `from` and `to`, the request defaults to the last 3 hours.

Example:

```shell
curl -sS \
  -H "Authorization: Bearer $TOKEN" \
  "https://$FQDN/api/v0/audit/self/request?from=2025-11-10T00:00:00Z&to=2025-11-11T00:00:00Z&order=desc"
```

Response:

```json
{
  "request_id": "f47ac10b-58cc-4372-a567-0e02b2c3d479",
  "status": "processing",
  "message": "Self audit log generation started. Use the request ID to check status and download when ready."
}
```

### Check request status

- Method/Path: `GET /api/v1/audit/status`
- Authentication: Required
- Access control: Self only (you can only view your own requested logs)
- Query parameters:
  - `request_id` (optional):
    - If omitted, returns the status of the latest request for the current user.
    - If provided, returns the status for that specific request ID.

Examples:

To get your latest request status:

```shell
curl -sS \
  -H "Authorization: Bearer <TOKEN>" \
  "https://<FQDN>/api/v1/audit/status"
```

To get the status for a specific request ID:

```shell
curl -sS \
  -H "Authorization: Bearer <TOKEN>" \
  "https://<FQDN>/api/v1/audit/status?request_id=<REQUEST_ID>"
```

Response fields:

- `request_id`: The UUID for the request.
- `status`: One of `processing`, `completed`, `error`, `not_found`.
- `file_size`: Present when `status` is `completed`. Human-readable size string (for example, `"10.7 KB"`).
- `download_url`: Present when `status` is `completed`. Use the returned URL/path to download the generated file.
  - The returned value may be an absolute URL (when an external FQDN is configured) or a relative path.
  - The download URL is formatted like `/api/v1/audit/download?request_id=<request_id>`.
- `error`: Present when `status` indicates an error. Possible values include `audit_disabled`, `request_not_found`, and `no_requested_logs`.
- `message`: Human-readable status/error details.

Example (completed):

```json
{
  "request_id": "a1c977e1-96a1-4a09-85f8-364721ff9f11",
  "status": "completed",
  "file_size": "10.7 KB",
  "download_url": "https://<FQDN>/api/v1/audit/download?request_id=a1c977e1-96a1-4a09-85f8-364721ff9f11",
  "error": "",
  "message": ""
}
```

### Download audit logs

- Method/Path: `GET /api/v1/audit/download`
- Authentication: Required
- Access control: Self only (you can only download your own audit logs)
- Query parameters:
  - `request_id` (optional):
    - If omitted, returns the last requested audit log file for the current user.
    - If provided, returns the audit log file for that specific request ID.
- Returns: The audit log file as a gzip-compressed download.

Each request generates a single file containing the audit logs for the full requested time range (up to 30 days).

The downloaded audit log file is gzip-compressed (for example, `audit_logs.log.gz`). To extract the log file, run:

```shell
gunzip <file-name>.log.gz
```

This produces the uncompressed log file:

```text
<file-name>.log
```

Examples:

```shell
# Download latest/last audit logs
curl -sS -L \
  -H "Authorization: Bearer <TOKEN>" \
  "https://<FQDN>/api/v1/audit/download" \
  -o audit-last.log.gz

# Download audit logs for a specific request_id
curl -sS -L \
  -H "Authorization: Bearer <TOKEN>" \
  "https://<FQDN>/api/v1/audit/download?request_id=<REQUEST_ID>" \
  -o "audit-<REQUEST_ID>.log.gz"
```
