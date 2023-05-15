package constants

const (
	NFS_MOUNT = "NFS Mount"

	MOUNT_SUCCESS_MSG    = "NFS mount location found"
	MOUNT_ERROR_MSG      = "NFS mount location not found"
	MOUNT_RESOLUTION_MSG = "NFS volume should be mounted on %s"

	SHARE_SUCCESS_MSG                  = "NFS mount location is shared across given nodes"
	SHARE_ERROR_MSG                    = "NFS mount location %s is not shared across all given nodes"
	SHARE_ERROR_MSG_WITHOUT_MOUNT      = "No NFS Volume found at mount location: %s"
	SHARE_RESOLUTION_MSG               = "NFS volume %s should be common across all given nodes at mount location: %s"
	SHARE_RESOLUTION_MSG_WITHOUT_MOUNT = "NFS volume should be mounted and common across all given nodes at mount location: %s"

	TIMEOUT = 30
)
