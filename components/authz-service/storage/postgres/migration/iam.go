package migration

import (
	"context"
	"database/sql"
	"fmt"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
	"github.com/lib/pq"
	"github.com/pkg/errors"
)

const (
	enumPristine        = "init"
	enumInProgress      = "in-progress"
	enumSuccessful      = "successful"
	enumSuccessfulBeta1 = "successful-beta1"
	enumFailed          = "failed"
)

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

// IAM v2 well-known project IDs
const (
	AllProjectsID         = "~~ALL-PROJECTS~~" // must match rego file!
	AllProjectsExternalID = "*"
	UnassignedProjectID   = "(unassigned)"
)

func needsV2Migration(ctx context.Context, db *sql.DB) (bool, error) {
	var status string
	row := db.QueryRowContext(ctx, `SELECT state FROM migration_status`)
	err := row.Scan(&status)
	if err != nil {
		return true, err // shouldn't happen, migration initializes state
	}
	switch status {
	case enumPristine:
		return true, nil
	case enumSuccessful:
		return false, nil
	case enumSuccessfulBeta1:
		return false, nil
	// TODO how should we properly handle these cases? is re-running migration enough?
	case enumInProgress:
		return true, nil
	case enumFailed:
		return true, nil
	}
	return true, fmt.Errorf("unexpected migration status: %q", status)
}

// MigrateToV2 sets the V2 store to its factory defaults and then migrates
// any existing V1 policies, unless the install is already on IAM v2.
func migrateToV2(ctx context.Context, db *sql.DB) error {
	ifNotOnV2, err := needsV2Migration(ctx, db)
	if err != nil {
		return errors.Wrap(err, "could not query IAM migration state")
	}

	if ifNotOnV2 {
		for _, role := range defaultRoles() {
			if err := createRole(ctx, db, &role); err != nil {
				return errors.Wrap(err, "could not create default role")
			}
		}

		// defaultPolicies, err := storage.DefaultPolicies()
		// if err != nil {
		// 	return nil, status.Errorf(codes.Internal, "retrieve default policies: %s", err.Error())
		// }

		// for _, pol := range defaultPolicies {
		// 	if _, err := s.store.CreatePolicy(ctx, &pol, true); err != nil {
		// 		return nil, status.Errorf(codes.Internal, "reset to default policies: %s", err.Error())
		// 	}
		// }

		// // Added for testing only; these are handled by data migrations.
		// for _, project := range storage.DefaultProjects() {
		// 	if _, err := s.store.CreateProject(ctx, &project, false); err != nil {
		// 		return nil, status.Errorf(codes.Internal, "reset to default project: %s", err.Error())
		// 	}
		// }

		// recordFailure := func() {
		// 	// This should be unlikely, and it doesn't affect our returned error, which,
		// 	// in any case, is the more interesting error -- so, we merely log it.
		// 	if err := s.store.Failure(ctx); err != nil {
		// 		s.log.Errorf("failed to record migration failure status: %s", err)
		// 	}
		// }

		// var reports []string
		// if !req.SkipV1Policies {
		// 	errs, err := s.migrateV1Policies(ctx)
		// 	if err != nil {
		// 		recordFailure()
		// 		return nil, status.Errorf(codes.Internal, "migrate v1 policies: %s", err.Error())
		// 	}
		// 	for _, e := range errs {
		// 		reports = append(reports, e.Error())
		// 	}
		// } else {
		// 	// Note 2019/05/22 (sr): policies without subjects are silently ignored -- this
		// 	// is to be in line with the migration case, that does the same. However, this
		// 	// could be worth revisiting?
		// 	pols, err := s.v1.ListPoliciesWithSubjects(ctx)
		// 	if err != nil {
		// 		recordFailure()
		// 		return nil, status.Errorf(codes.Internal, "list v1 policies: %s", err.Error())
		// 	}
		// 	reports = append(reports, fmt.Sprintf("%d v1 policies", len(pols)))

		// }
	}

	// // we've made it!
	// var v api.Version
	// switch req.Flag {
	// case api.Flag_VERSION_2_1:
	// 	err = s.store.SuccessBeta1(ctx)
	// 	v = api.Version{Major: api.Version_V2, Minor: api.Version_V1}
	// default:
	// 	err = s.store.Success(ctx)
	// 	v = api.Version{Major: api.Version_V2, Minor: api.Version_V0}
	// }
	// if err != nil {
	// 	recordFailure()
	// 	return nil, status.Errorf(codes.Internal, "record migration status: %s", err.Error())
	// }

	// s.setVersionForInterceptorSwitch(v)
	// return &api.MigrateToV2Resp{Reports: reports}, nil

	return nil
}

/*
COPY PASTA DATABASE CODE

The below is code we've copied from our database functionality because we need
versions of the database functions needed for the migrations that do not change.
This is because this migration is run at a single point in time as part of the schema
upgrades. So this code need to be compatible with a specific schema version that never changes.
*/

// Role-related database copy-pasta

func createRole(ctx context.Context, db *sql.DB, role *Role) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return errors.Wrap(err, "begin create role tx")
	}

	row := tx.QueryRowContext(ctx, `INSERT INTO iam_roles (id, name, type, actions) VALUES ($1, $2, $3, $4)
		RETURNING db_id`,
		role.ID, role.Name, role.Type.String(), pq.Array(role.Actions))
	var dbID string
	if err := row.Scan(&dbID); err != nil {
		return errors.Wrap(err, "insert role")
	}

	_, err = tx.ExecContext(ctx,
		`INSERT INTO iam_role_projects (role_id, project_id)
		SELECT $1, project_db_id(p) FROM unnest($2::TEXT[]) as p`,
		dbID, pq.Array(role.Projects))
	if err != nil {
		return errors.Wrap(err, "insert role projects")
	}

	err = tx.Commit()
	if err != nil {
		return storage_errors.NewTxCommitError(err)
	}

	return nil
}

// Type is an enum to denote custom or chef-managed policy.
type Type int

const (
	// Custom represents a policy created by the enduser.
	Custom Type = iota
	// ChefManaged represents a policy created by Chef Software.
	ChefManaged
	// System represents a policy that is only loaded directly into OPA
	// to allow Automate to function correctly without revealing Automate's
	// internal policies to the customer
	// This type is only used in the OPA cache (not in API or database)
	System
)

const (
	customTypeString  = "custom"
	managedTypeString = "chef-managed"
	systemTypeString  = "system"
)

var strValues = [...]string{
	customTypeString,
	managedTypeString,
	systemTypeString,
}

func (t Type) String() string {
	if t < Custom || t > System {
		panic(fmt.Sprintf("unknown value from iota Type on String() conversion: %d", t))
	}

	return strValues[t]
}

type Role struct {
	ID       string   `json:"id"`
	Name     string   `json:"name"`
	Actions  []string `json:"actions"`
	Type     Type     `json:"type"`
	Projects []string `json:"projects"`
}

// DefaultRoles defines the default Chef-managed roles provided on storage reset
func defaultRoles() []Role {
	owner := Role{
		ID:      OwnerRoleID,
		Name:    "Owner",
		Actions: []string{"*"},
		Type:    ChefManaged,
	}

	editor := Role{
		ID:   EditorRoleID,
		Name: "Editor",
		Actions: []string{
			"infra:*",
			"compliance:*",
			"system:*",
			"event:*",
			"ingest:*",
			"secrets:*",
			"telemetry:*",
		},
		Type: ChefManaged,
	}

	viewer := Role{
		ID:   ViewerRoleID,
		Name: "Viewer",
		Actions: []string{
			"secrets:*:get",
			"secrets:*:list",
			"infra:*:get",
			"infra:*:list",
			"compliance:*:get",
			"compliance:*:list",
			"system:*:get",
			"system:*:list",
			"event:*:get",
			"event:*:list",
			"ingest:*:get",
			"ingest:*:list",
		},
		Type: ChefManaged,
	}

	ingest := Role{
		ID:   IngestRoleID,
		Name: "Ingest",
		Actions: []string{
			"infra:ingest:*",
			"compliance:profiles:get",
			"compliance:profiles:list",
		},
		Type: ChefManaged,
	}

	return []Role{owner, editor, viewer, ingest}
}
