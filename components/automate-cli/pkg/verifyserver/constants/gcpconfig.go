package constants

const (
	GCP_CONNECTION_TITLE                  = "GCS connection test"
	GCP_BUCKET_NOT_FOUND                  = "Cannot find the Bucket in GCS cloud storage with provided credentials"
	GCP_BUCKET_NOT_FOUND_RESOLUTION_MSG   = "Create a bucket in GCS cloud storage"
	GCP_CONNECTION_ERROR_MSG              = "Machine is not able to connect with GCS using the provided credentials"
	GCP_CONNECTION_RESOLUTION_MSG         = "Provide the correct GCS credentials"
	GCP_CONNECTION_SUCCESS_MSG            = "Machine is able to connect with GCS using the provided credentials"
	GCP_CONNECTION_RESOLUTION_GENERAL_MSG = "Machine is not able to connect, check if the bucket exists and the provided credential is correct"
	GCP_BUCKET_ACCESS_TITLE               = "GCS bucket access test"
	GCP_BUCKET_ACCESS_ERROR_MSG           = "Machine is not able to access the GCS bucket using the provided credentials"
	GCP_BUCKET_ACCESS_RESOLUTION_MSG      = "Please check if the provided GCS bucket exists or not. If it exists then provide the correct credentials."
	GCP_BUCKET_UPLOAD_ERROR_MSG           = "Unable to upload the object to given bucket"
	GCP_BUCKET_UPLOAD_RESOLUTION_MSG      = "Please check if the valid permissions given to upload the object to the bucket"
	GCP_BUCKET_LIST_ERROR_MSG             = "Unable to list the objects in given bucket"
	GCP_BUCKET_LIST_RESOLUTION_MSG        = "Please check if the valid permissions given to list the objects in the bucket"
	GCP_BUCKET_DELETE_ERROR_MSG           = "Unable to delete the object from the given bucket"
	GCP_BUCKET_DELETE_RESOLUTION_MSG      = "Please check if the valid permissions given to delete the object in the bucket"

	GCP_CHECK_FILE_PREFIX = "gcp_check_test_"
)

const (
	GCS_TYPE_MISSING                = "'type' field is missing in gcs service account file"
	GCS_PROJECT_ID                  = "'project_id' field is missing in gcs service account file"
	GCS_PRIVATE_KEY_ID              = "'private_key_id' field is missing in gcs service account file"
	GCS_PRIVATE_KEY                 = "'private_key' field is missing in gcs service account file"
	GCS_CLIENT_EMAIL                = "'client_email' field is missing in gcs service account file"
	GCS_CLIENT_ID                   = "'client_id' field is missing in gcs service account file"
	GCS_AUTH_URI                    = "'auth_uri' field is missing in gcs service account file"
	GCS_TOKEN_URI                   = "'token_uri' field is missing in gcs service account file"
	GCS_AUTH_PROVIDER_x509_CERT_URL = "'auth_provider_x509_cert_url' field is missing in gcs service account file"
	GCS_CLIENT_x509_CERT_URL        = "'client_x509_cert_url' field is missing in gcs service account file"
	GCS_UNIVERSAL_DOMAIN            = "'universe_domain' field is missing in gcs service account file"

	// Resolutions
	GCS_TYPE_MISSING_RESOLUTION                = "Provide 'type' in gcs service account file"
	GCS_PROJECT_ID_RESOLUTION                  = "Provide 'project_id' in gcs service account file"
	GCS_PRIVATE_KEY_ID_RESOLUTION              = "Provide 'private_key_id' in gcs service account file"
	GCS_PRIVATE_KEY_RESOLUTION                 = "Provide 'private_key' in gcs service account file"
	GCS_CLIENT_EMAIL_RESOLUTION                = "Provide 'client_email' in gcs service account file"
	GCS_CLIENT_ID_RESOLUTION                   = "Provide 'client_id' in gcs service account file"
	GCS_AUTH_URI_RESOLUTION                    = "Provide 'auth_uri' in gcs service account file"
	GCS_TOKEN_URI_RESOLUTION                   = "Provide 'token_uri' in gcs service account file"
	GCS_AUTH_PROVIDER_x509_CERT_URL_RESOLUTION = "Provide 'auth_provider_x509_cert_url' in gcs service account file"
	GCS_CLIENT_x509_CERT_URL_RESOLUTION        = "Provide 'client_x509_cert_url' in gcs service account file"
	GCS_UNIVERSAL_DOMAIN_RESOLUTION            = "Provide 'universe_domain' in gcs service account file"
)
