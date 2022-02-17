package storage

import (
	"context"
	"errors"
	"time"

	"github.com/chef/automate/components/infra-proxy-service/pipeline"
)

// Storage is the interface provided by our various storage backends.
type Storage interface {
	GetServer(ctx context.Context, id string) (Server, error)
	GetServers(ctx context.Context) ([]Server, error)
	StoreServer(ctx context.Context, id string, name string, fqdn string, ipAddress string, credentialId string) (Server, error)
	DeleteServer(ctx context.Context, id string) (Server, error)
	EditServer(ctx context.Context, id string, name string, fqdn string, ipAddress string) (Server, error)
	EditServerWebuiKey(ctx context.Context, id, credentialId string) (Server, error)

	GetOrg(ctx context.Context, orgID string, serverID string) (Org, error)
	GetOrgs(ctx context.Context, serverID string) ([]Org, error)
	StoreOrg(ctx context.Context, id string, name string, adminUser string, adminKey string, serverID string, projects []string) (Org, error)
	DeleteOrg(ctx context.Context, orgID string, serverID string) (Org, error)
	EditOrg(ctx context.Context, id string, name string, adminUser string, serverID string, projects []string) (Org, error)
	TouchOrg(ctx context.Context, id string, serverID string) (Org, error)

	InsertUser(ctx context.Context, id, serverID, infraServerUsername, credentialID, Connector, automateUserID string, IsServerAdmin bool) (User, error)
	GetUser(ctx context.Context, id string) (User, error)
	EditUser(ctx context.Context, id, serverID, infraServerUsername, credentialID, Connector, automateUserID string, IsServerAdmin bool) (User, error)
	DeleteUser(ctx context.Context, id string) (User, error)
	GetAutomateInfraServerUsers(ctx context.Context, serverId string) ([]User, error)
	GetAutomateOrgUsers(ctx context.Context, orgId string) ([]OrgUser, error)
}

type MigrationStorage interface {
	StartMigration(ctx context.Context, migrationId, serverId string) (Migration, error)
	StartFileUpload(ctx context.Context, migrationId, serverId string) (Migration, error)
	CompleteFileUpload(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	FailedFileUpload(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	StartUnzip(ctx context.Context, migrationId, serverId string) (Migration, error)
	CompleteUnzip(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	FailedUnzip(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	StartOrgParsing(ctx context.Context, migrationId, serverId string) (Migration, error)
	CompleteOrgParsing(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	FailedOrgParsing(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	StartUsersParsing(ctx context.Context, migrationId, serverId string) (Migration, error)
	CompleteUsersParsing(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	FailedUsersParsing(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	StartUserAssociationParsing(ctx context.Context, migrationId, serverId string) (Migration, error)
	CompleteUserAssociationParsing(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	FailedUserAssociationParsing(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	StartPermissionParsing(ctx context.Context, migrationId, serverId string) (Migration, error)
	CompletePermissionParsing(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	FailedPermissionParsing(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	StartCreatePreview(ctx context.Context, migrationId, serverId string) (Migration, error)
	CompleteCreatePreview(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	FailedCreatePreview(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	StartOrgMigration(ctx context.Context, migrationId, serverId string) (Migration, error)
	CompleteOrgMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	FailedOrgMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	StartUserMigration(ctx context.Context, migrationId, serverId string) (Migration, error)
	CompleteUserMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	FailedUserMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	StartAssociation(ctx context.Context, migrationId, serverId string) (Migration, error)
	CompleteAssociation(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	FailedAssociation(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	StartPermissionMigration(ctx context.Context, migrationId, serverId string) (Migration, error)
	CompletePermissionMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	FailedPermissionMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	CompleteMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	FailedMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	CancelMigration(ctx context.Context, migrationId, serverId string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)
	FailedCancelMigration(ctx context.Context, migrationId, serverId, message string, totalSucceeded, totalSkipped, totalFailed int64) (Migration, error)

	StoreMigrationStage(ctx context.Context, migrationId string, stagedData interface{}) (MigrationStage, error)
	GetMigrationStage(ctx context.Context, migrationId string) (MigrationStage, error)
	DeleteMigrationStage(ctx context.Context, migrationId string) (MigrationStage, error)

	GetActiveMigration(ctx context.Context, serverId string) (MigrationStatus, error)
	GetMigrationStatus(ctx context.Context, migrationId string) (MigrationStatus, error)
}

// Resetter is, if exposed, used for tests to reset the storage backend to a
// pristine state.
type Resetter interface {
	Reset(context.Context) error
}

// Server is the struct ingested and returned by our backend implementations.
type Server struct {
	ID           string
	Name         string
	Fqdn         string
	IPAddress    string
	OrgsCount    int32
	Projects     []string
	CredentialID string
	CreatedAt    time.Time
	UpdatedAt    time.Time
}

// Org is the struct ingested and returned by our backend implementations.
type Org struct {
	ID           string
	Name         string
	AdminUser    string
	CredentialID string
	ServerID     string
	Projects     []string
	CreatedAt    time.Time
	UpdatedAt    time.Time
}

// User is the struct ingested and returned by our backend implementations.
type User struct {
	ID                  string
	ServerID            string
	InfraServerUsername string
	CredentialID        string
	Connector           string
	AutomateUserID      string
	IsServerAdmin       bool
	CreatedAt           time.Time
	UpdatedAt           time.Time
}

type OrgUser struct {
	OrgId               string
	InfraServerUsername string
}

type Migration struct {
	ID               string    `json:"id"`
	MigrationID      string    `json:"migration_id"`
	ServerID         string    `json:"server_id"`
	TypeID           int64     `json:"type_id"`
	StatusID         int64     `json:"status_id"`
	TotalSucceeded   int64     `json:"total_succeeded"`
	TotalSkipped     int64     `json:"total_skipped"`
	TotalFailed      int64     `json:"total_failed"`
	Message          string    `json:"message"`
	UpdatedTimestamp time.Time `json:"updated_timestamp"`
}
type MigrationStage struct {
	ID          string          `json:"id"`
	MigrationID string          `json:"migration_id"`
	StagedData  pipeline.Result `json:"parsed_data"`
	CreatedAt   time.Time       `json:"created_at"`
	UpdatedAt   time.Time       `json:"updated_at"`
}

type MigrationStatus struct {
	MigrationID       string
	MigrationType     string
	MigrationStatus   string
	MigrationStatusID int64
	MigrationTypeID   int64
}

// Errors returned from the backend
var (
	// ErrNotFound is returned when a requested server wasn't found
	ErrNotFound = errors.New("not found")

	// ErrCannotDelete is returned when a request attempts to delete a server that
	// is not allowed to be deleted if orgs present
	ErrCannotDelete = errors.New("cannot delete")

	// ErrConflict is returned when a server there is a clash of server IDs
	ErrConflict = errors.New("conflict")

	// ErrForeignKeyViolation is returned when a server ID is not found
	ErrForeignKeyViolation = errors.New("not found")
)
