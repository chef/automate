package constants

const (
	GCP_CONNECTION_TITLE                = "GCP connection test"
	GCP_BUCKET_NOT_FOUND                = "Cannot fine the Bucket in GCP cloud storage"
	GCP_BUCKET_NOT_FOUND_RESOLUTION_MSG = "Create a bucket in GCP cloud storage"
	GCP_CONNECTION_ERROR_MSG            = "Machine is not able to connect with GCP using the provided credentials"
	GCP_CONNECTION_RESOLUTION_MSG       = "Provide the correct GCP credentials"
	GCP_CONNECTION_SUCCESS_MSG          = "Machine is able to connect with GCP using the provided credentials"
	GCP_BUCKET_ACCESS_TITLE             = "GCP bucket access test"
	GCP_BUCKET_ACCESS_ERROR_MSG         = "Machine is not able to access the GCP bucket using the provided access key and secret key"
	GCP_BUCKET_ACCESS_RESOLUTION_MSG    = "Please check if the provided GCP bucket exists or not. If it exists then provide the bucket access to the snapshot user."
	GCP_BUCKET_ACCESS_SUCCESS_MSG       = "Machine is able to access the GCP bucket using the provided access key and secret key"
)
