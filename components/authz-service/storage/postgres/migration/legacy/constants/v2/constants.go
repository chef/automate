package v2

// IAM v2 default policy IDs.
const (
	AdminPolicyID  = "administrator-access"
	EditorPolicyID = "editor-access"
	ViewerPolicyID = "viewer-access"
	IngestPolicyID = "ingest-access"
)

// IAM v2 system policy IDs. These are never shown to the enduser
// so GUIDs are fine.
const (
	UniversalAccessPolicyID  = "e729c61f-c40a-4bfa-affe-2a541368169f"
	IngestProviderPolicyID   = "e166f6f9-860d-464a-a91f-be3509369f92"
	SystemPolicyID           = "1074e13b-a918-4892-98be-47a5a8b2d2b6"
	SystemLocalUsersPolicyID = "00a38187-7557-4105-92a0-48db63af4103"
	ChefManagedPolicyID      = "e62bc524-d903-4708-92de-a4435ce0252e"
)

// V1 -> IAM v2 Legacy Policy IDs.
const (
	CfgmgmtPolicyID         = "infrastructure-automation-access-legacy"
	CompliancePolicyID      = "compliance-access-legacy"
	EventsPolicyID          = "events-access-legacy"
	LegacyIngestPolicyID    = "ingest-access-legacy"
	NodesPolicyID           = "nodes-access-legacy"
	NodeManagersPolicyID    = "node-managers-access-legacy"
	SecretsPolicyID         = "secrets-access-legacy"
	TelemetryPolicyID       = "telemetry-access-legacy"
	ComplianceTokenPolicyID = "compliance-profile-access-legacy"
)

// IAM v2 well-known role IDs
const (
	OwnerRoleID        = "owner"
	EditorRoleID       = "editor"
	ViewerRoleID       = "viewer"
	IngestRoleID       = "ingest"
	ProjectOwnerRoleID = "project-owner"
)

// Subjects
const (
	// LocalAdminsTeamSubject is the member for the local admins team.
	LocalAdminsTeamSubject = "team:local:admins"

	// LocalEditorsTeamSubject is the member for the local editors team.
	LocalEditorsTeamSubject = "team:local:editors"

	// LocalViewersTeamSubject is the member for the local viewers team.
	LocalViewersTeamSubject = "team:local:viewers"
)

// IAM v2 well-known project IDs
const (
	AllProjectsID         = "~~ALL-PROJECTS~~" // must match rego file!
	AllProjectsExternalID = "*"
	UnassignedProjectID   = "(unassigned)"
)

const (
	EnumPristine        = "init"
	EnumInProgress      = "in-progress"
	EnumSuccessful      = "successful"
	EnumSuccessfulBeta1 = "successful-beta1"
	EnumFailed          = "failed"
)
