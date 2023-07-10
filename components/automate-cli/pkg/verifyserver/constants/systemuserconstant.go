package constants

const (
	USER_NAME                                  = "hab"
	GROUP_NAME                                 = "hab"
	USER_ADD_CMD                               = "useradd"
	USER_DEL_CMD                               = "userdel"
	GROUP_DEL_CMD                              = "groupdel"
	SYSTEM_USER_HAB_VALIDATION_SUCCESS_TITLE   = "User creation/validation check"
	SYSTEM_USER_HAB_SUCCESS_MSG                = "User is created or found successfully"
	SYSTEM_GROUP_HAB_VALIDATION_SUCCESS_TITLE  = "Group creation/validation check"
	SYSTEM_GROUP_HAB_SUCCESS_MSG               = "Group is created or found successfully"
	SYSTEM_USERANDGROUP_MAPPING_SUCCESS_TITLE  = "User and group mapping successfully"
	SYSTEM_USERANDGROUP_MAPPING_SUCCESS_MSG    = "User and group mapping successful"
	SYSTEM_USER_HAB_VALIDATION_FAILURE_TITLE   = "User validation failure"
	SYSTEM_USER_HAB_ERROR_MSG                  = "Hab user creation failed"
	SYSTEM_USER_HAB_RESOLUTION_MSG             = "Failed to add/find ‘hab’ user. Please check the user privileges and provide access to create/find user."
	SYSTEM_GROUP_HAB_VALIDATION_FAILURE_TITLE  = "Group validation failure"
	SYSTEM_GROUP_HAB_ERROR_MSG                 = "Hab group not found"
	SYSTEM_GROUP_HAB_RESOLUTION_MSG            = "Failed to add/find ‘hab’ group. Please check the user privileges and provide access to create/find group."
	SYSTEM_USERANDGROUP_MAPPING_FAILURE_TITLE  = "User and group mapping failed"
	SYSTEM_PRIMARYGROUP_MATCH_ERROR_MSG        = "Primary group mapping for user hab is not hab group"
	SYSTEM_USERANDGROUP_MAPPING_RESOLUTION_MSG = "Failed to map ‘hab’ user to ‘hab’ group. Please check the user privileges and provide access to map users to a group."
)
