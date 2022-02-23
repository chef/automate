package constants

// MigrationPhase: enum of migration phases
type MigrationPhase int64

// Migration phases id should be between 100 & 5000
const (
	StartMigration    = 100
	CompleteMigration = 5000
	CancelMigration   = 6000
)

// MigrationStatus: enum of migration status
type MigrationStatus int64

// IAM default project ID
const (
	UnassignedProjectID = "(unassigned)"
)

const (
	FileUpload MigrationPhase = iota + 1000
	FileUnzip
	OrgsParsing
	UsersParsing
	UserAssociationParsing
	PermissionParsing
	CreatePreview
	OrgsMigration
	UserMigration
	UserAssociation
	PermissionMigration
)

const (
	InProgress MigrationStatus = iota + 100
	Completed
	Failed
)

const (
	TestServerName = "Chef infra server"
	TestServerId   = "chef-infra-server1"
)
