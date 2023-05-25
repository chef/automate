package constants

const (
	TEST_REPO_NAME                         = "S3BackupVerifyNew"
	TEST_SNAPSHOT_NAME                     = "test-snapshot-managed"
	TEST_INDEX_NAME                        = "test-index-managed"
	INDEX_CREATE_FAILED_MESSAGE            = "Failed to create an index on the Opensearch Domain"
	INDEX_CREATE_FAILED_RESOLUTION         = "Setup Opensearch with valid Configuration and provide the IAM user proper permissions to create an index."
	SNAPSHOT_REPO_CREATE_FAILED_MESSAGE    = "Failed to create the Snapshot Repository on the Opensearch Domain"
	SNAPSHOT_REPO_CREATE_FAILED_RESOLUTION = "Setup Opensearch with valid Configuration and provide the IAM user proper permissions to create the Snapsshot Repository."
	SNAPSHOT_CREATE_FAILED_MESSAGE         = "Failed to create the Snapshot on the Opensearch Domain"
	SNAPSHOT_CREATE_FAILED_RSOLUTION       = "Setup Opensearch with valid Configuration and provide the IAM user proper permissions to create a Snapshot."
	INDEX_DELETE_FAILED_MESSAGE            = "Failed to delete an index on the Opensearch Domain"
	INDEX_DELETE_FAILED_RESOLUTION         = "Provide the IAM user proper permissions to delete an index."
	SNAPSHOT_REPO_DELETE_FAILED_MESSAGE    = "Failed to delete the Snapshot Repository on the Opensearch Domain"
	SNAPSHOT_REPO_DELETE_FAILED_RESOLUTION = "Provide the IAM user proper permissions to delete the Snapshot Repository."
	SNAPSHOT_DELETE_FAILED_MESSAGE         = "Failed to delete the Snapshot on the Opensearch Domain"
	SNAPSHOT_DELETE_FAILED_RESOLUTION      = "Provide the IAM user proper permissions to delete the Snapshot."
	SNAPSHOT_STATUS_API                    = "/_snapshot/%s/%s/_status"
	INVALID_REQUEST_ERROR                  = "Invalid request body for S3 backup check"
)
