package constants

const (
	AUTOMATESTATUSONBEERROR      = "FileAccessError: Unable to access the file or directory: Failed to read deployment-service TLS certificates: Could not read the service cert: open /hab/svc/deployment-service/data/deployment-service.crt: no such file or directory"
	AUTOMATESTATUSUNHEALTHYERROR = "UnhealthyStatusError: System status is unhealthy: One or more services are unhealthy"

	NODES_NOT_PRESENT      = "HA nodes are not available"
	MISSING_CERTIFICATE    = "Certificate is missing"
	MOUNT_LOCATION_MISSING = "MountLocation is missing"
	S3_BACKUP_MISSING      = "S3 backup detail is missing"
	GCS_BACKUP_MISSING     = "GCS backup detail is missing"
	OBJECT_STORAGE_MISSING = "Object storage detail or OS detail is missing"
	PG_DETAILS_MISSING     = "PG configuration is missing"
	OS_DETAILS_MISSING     = "OS configuration is missing"
)
