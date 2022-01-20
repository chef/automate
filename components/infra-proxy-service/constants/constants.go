package constants

// MigrationPhase: enum of migration phases
type MigrationPhase int64

// MigrationStatus: enum of migration status
type MigrationStatus int64

// IAM default project ID
const (
	UnassignedProjectID = "(unassigned)"
)

const (
	StartZipParsing MigrationPhase = iota + 1
	CompleteZipParsing
	StartOrgMigration
	CompleteOrgMigration
	StartUserMigration
	CompleteUserMigration
	StartAssciation
	CompleteAssciation
	StartPermissionMigration
	CompletePermissionMigration
)

const (
	InProgress MigrationStatus = iota + 1
	Completed
	Failed
)
