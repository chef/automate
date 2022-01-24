package constants

// MigrationPhase: enum of migration phases
type MigrationPhase int64

// Migration phases id should be between 100 & 5000
const (
	StartMigration    = 100
	CompleteMigration = 5000
)

// MigrationStatus: enum of migration status
type MigrationStatus int64

// IAM default project ID
const (
	UnassignedProjectID = "(unassigned)"
)

const (
	StartFileUpload MigrationPhase = iota + 1000
	CompleteFileUpload
	FailedFileUpload
	StartUnzip
	ComplteUnzip
	FailedUnzip
	StartZipParsing
	CompleteZipParsing
	FailedZipParsing
	StartOrgMigration
	CompleteOrgMigration
	FailedOrgMigration
	StartUserMigration
	CompleteUserMigration
	FailedUserMigration
	StartAssciation
	CompleteAssciation
	FailedAssciation
	StartPermissionMigration
	CompletePermissionMigration
	FailedPermissionMigration
)

const (
	InProgress MigrationStatus = iota + 100
	Completed
	Failed
)
