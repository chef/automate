package constants

// UserNamespace is the namespace used for users
const UserNamespace = "user"

// TokenNamespace is the namespace used for tokens
const TokenNamespace = "token"

// AdvisoryLockID is used for migrations (Postgres Related)
const AdvisoryLockID = 8701 // Usher Baby!

// LocalConnectorID is the ID used for dex' local user connector
const LocalConnectorID = "local"

// UnassignedProjectID represents the project ID for
// resources (tokens) without projects
const UnassignedProjectID = "(unassigned)"

// AllProjectsExternalID is the representation of all projects to downstream services
const AllProjectsExternalID = "*"

// TestPgURL is the URL to connect to the authn test DB
const TestPgURL = "postgresql://postgres@127.0.0.1:5432/authn_test?sslmode=disable&timezone=UTC"
