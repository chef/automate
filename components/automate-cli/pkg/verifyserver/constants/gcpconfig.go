package constants

const (
	GCP_CONNECTION_TITLE                = "GCS connection test"
	GCP_BUCKET_NOT_FOUND                = "Cannot find the Bucket in GCS cloud storage"
	GCP_BUCKET_NOT_FOUND_RESOLUTION_MSG = "Create a bucket in GCS cloud storage"
	GCP_CONNECTION_ERROR_MSG            = "Machine is not able to connect with GCS using the provided credentials"
	GCP_CONNECTION_RESOLUTION_MSG       = "Provide the correct GCS credentials"
	GCP_CONNECTION_SUCCESS_MSG          = "Machine is able to connect with GCS using the provided credentials"
	GCP_BUCKET_ACCESS_TITLE             = "GCS bucket access test"
	GCP_BUCKET_ACCESS_ERROR_MSG         = "Machine is not able to access the GCS bucket using the provided credentials"
	GCP_BUCKET_ACCESS_RESOLUTION_MSG    = "Please check if the provided GCS bucket exists or not. If it exists then provide the correct credentials."
)

const (
	GCP_CHECK_FILE_PREFIX = "gcp_check_test_"
)
