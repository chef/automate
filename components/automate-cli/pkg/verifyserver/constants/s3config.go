package constants

const (
	S3_CONNECTION_TITLE             = "S3 connection test"
	S3_CONNECTION_ERROR_MSG         = "Machine is not able to connect with S3 using the provided access key and secret key"
	S3_CONNECTION_RESOLUTION_MSG    = "Provide the correct S3 url or access or secret keys"
	S3_CONNECTION_SUCCESS_MSG       = "Machine is able to connect with S3 using the provided access key and secret key"
	S3_BUCKET_ACCESS_TITLE          = "S3 bucket access test"
	S3_BUCKET_ACCESS_ERROR_MSG      = "Machine is not able to access the S3 bucket using the provided access key and secret key"
	S3_BUCKET_ACCESS_RESOLUTION_MSG = "Please check if the provided S3 bucket exists or not. If it exists then provide the bucket access to the snapshot user."
	S3_BUCKET_ACCESS_SUCCESS_MSG    = "Machine is able to access the S3 bucket using the provided access key and secret key"
)
