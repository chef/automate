package constants

const (
	MOUNT_SUCCESS_MSG    = "NFS mount location found"
	MOUNT_ERROR_MSG      = "NFS mount location not found"
	MOUNT_RESOLUTION_MSG = "NFS volume should be mounted on %s"

	SHARE_SUCCESS_MSG    = "NFS mount location is shared across given nodes"
	SHARE_ERROR_MSG      = "NFS mount location %s is not shared across all given nodes"
	SHARE_RESOLUTION_MSG = "NFS volume should be common across all given nodes at mount location: %s"
)
