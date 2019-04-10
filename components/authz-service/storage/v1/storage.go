package v1

import (
	"context"
	"time"

	constants "github.com/chef/automate/components/authz-service/constants/v1"
	"github.com/chef/automate/lib/stringutils"
	uuid "github.com/chef/automate/lib/uuid4"
)

// Policy represents the policy that will be stored.
type Policy struct {
	Action    string
	ID        uuid.UUID
	Resource  string
	Subjects  []string
	Effect    string
	CreatedAt time.Time
	UpdatedAt time.Time
	Version   int
}

// Storage is the interface provided by our various storage backends.
type Storage interface {
	StorePolicy(ctx context.Context, action string, subjects []string, resource string, effect string) (*Policy, error)
	DeletePolicy(ctx context.Context, id string) (*Policy, error)
	PurgeSubjectFromPolicies(ctx context.Context, subject string) ([]uuid.UUID, error)
	ListPolicies(ctx context.Context) ([]*Policy, error)

	PoliciesLister
}

// PoliciesLister is the interface that wraps the ListPoliciesWithSubjects
// method used for migrating to v2.
type PoliciesLister interface {
	ListPoliciesWithSubjects(ctx context.Context) ([]*Policy, error)
}

// Version is the internal policy version
const Version int = 1

// Resetter is, if exposed, used for tests to reset the storage backend to a
// pristine state.
type Resetter interface {
	Reset(context.Context) error
}

// IsNonDeletablePolicy returns true for UUID strings associated with
// policies marked as non-deletable and false otherwise.
//
// TODO (tc) This is used for testing and memstore as postgres
// handles non-deletable policies directly. I think we should consider getting
// rid of memstore.
func IsNonDeletablePolicy(id string) bool {
	return stringutils.SliceContains(constants.NonDeletablePolicyIDs[:], id)
}

// DefaultPolicies returns all the default policies initially created.
// It is strictly used for testing and memstore.
//
// TODO (tc) This is used for testing and memstore as postgres
// handles non-deletable policies directly. I think we should consider getting
// rid of memstore.
func DefaultPolicies() (map[string]*Policy, error) {
	ids := map[string]uuid.UUID{}
	for _, fixedID := range constants.DefaultPolicyIDs {
		id, err := uuid.FromString(fixedID)
		if err != nil {
			return nil, err
		}
		ids[fixedID] = id
	}
	defaultPolicies := map[string]*Policy{
		constants.AdminPolicyID: {
			ID:       ids[constants.AdminPolicyID],
			Subjects: []string{"team:local:admins"},
			Resource: "*",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.CfgmgmtStatsWildcardPolicyID: {
			ID:       ids[constants.CfgmgmtStatsWildcardPolicyID],
			Subjects: []string{"user:*"},
			Resource: "cfgmgmt:stats:*",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.CfgmgmtNodesWildcardPolicyID: {
			ID:       ids[constants.CfgmgmtNodesWildcardPolicyID],
			Subjects: []string{"user:*"},
			Resource: "cfgmgmt:nodes:*",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.CfgmgmtNodesContainerPolicyID: {
			ID:       ids[constants.CfgmgmtNodesContainerPolicyID],
			Subjects: []string{"user:*"},
			Resource: "cfgmgmt:nodes",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.ComplianceWildcardPolicyID: {
			ID:       ids[constants.ComplianceWildcardPolicyID],
			Subjects: []string{"user:*"},
			Resource: "compliance:*",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.ServiceInfoWildcardPolicyID: {
			ID:       ids[constants.ServiceInfoWildcardPolicyID],
			Subjects: []string{"user:*"},
			Resource: "service_info:*",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.EventsContainerPolicyID: {
			ID:       ids[constants.EventsContainerPolicyID],
			Subjects: []string{"user:*"},
			Resource: "events",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.EventsWildcardPolicyID: {
			ID:       ids[constants.EventsWildcardPolicyID],
			Subjects: []string{"user:*"},
			Resource: "events:*",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.IngestWildcardPolicyID: {
			ID:       ids[constants.IngestWildcardPolicyID],
			Subjects: []string{"token:*"},
			Resource: "ingest:*",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.AuthIntrospectionWildcardPolicyID: {
			ID:       ids[constants.AuthIntrospectionWildcardPolicyID],
			Subjects: []string{"user:*"},
			Resource: "auth_introspection:*",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.NodesContainerPolicyID: {
			ID:       ids[constants.NodesContainerPolicyID],
			Subjects: []string{"user:*"},
			Resource: "nodes",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.NodesWildcardPolicyID: {
			ID:       ids[constants.NodesWildcardPolicyID],
			Subjects: []string{"user:*"},
			Resource: "nodes:*",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.NodeManagersContainerPolicyID: {
			ID:       ids[constants.NodeManagersContainerPolicyID],
			Subjects: []string{"user:*"},
			Resource: "nodemanagers",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.NodeManagersWildcardPolicyID: {
			ID:       ids[constants.NodeManagersWildcardPolicyID],
			Subjects: []string{"user:*"},
			Resource: "nodemanagers:*",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.SecretsContainerPolicyID: {
			ID:       ids[constants.SecretsContainerPolicyID],
			Subjects: []string{"user:*"},
			Resource: "secrets",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.SecretsWildcardPolicyID: {
			ID:       ids[constants.SecretsWildcardPolicyID],
			Subjects: []string{"user:*"},
			Resource: "secrets:*",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.TelemetryConfigPolicyID: {
			ID:       ids[constants.TelemetryConfigPolicyID],
			Subjects: []string{"user:*"},
			Resource: "telemetry:config",
			Action:   "read",
			Effect:   "allow",
			Version:  1,
		},
		constants.ComplianceTokenReadProfilesPolicyID: {
			ID:       ids[constants.ComplianceTokenReadProfilesPolicyID],
			Subjects: []string{"token:*"},
			Resource: "compliance:profiles:*",
			Action:   "read",
			Effect:   "allow",
			Version:  1,
		},
		constants.ComplianceTokenUploadProfilesPolicyID: {
			ID:       ids[constants.ComplianceTokenUploadProfilesPolicyID],
			Subjects: []string{"token:*"},
			Resource: "compliance:profiles:*",
			Action:   "upload",
			Effect:   "allow",
			Version:  1,
		},
		constants.ComplianceTokenSearchProfilesPolicyID: {
			ID:       ids[constants.ComplianceTokenSearchProfilesPolicyID],
			Subjects: []string{"token:*"},
			Resource: "compliance:profiles",
			Action:   "search",
			Effect:   "allow",
			Version:  1,
		},
		constants.LicenseStatusPolicyID: {
			ID:       ids[constants.LicenseStatusPolicyID],
			Subjects: []string{"user:*"},
			Resource: "license:status",
			Action:   "read",
			Effect:   "allow",
			Version:  1,
		},
		constants.ReadOwnUserProfilePolicyID: {
			ID: ids[constants.ReadOwnUserProfilePolicyID],
			// This is vital! {ldap, saml} users shouldn't update local users' info
			// because their usernames clash
			Subjects: []string{"user:local:*"},
			Resource: "auth:users:${a2:username}",
			Action:   "read",
			Effect:   "allow",
			Version:  1,
		},
		constants.LocalUserSelfPolicyID: {
			ID: ids[constants.LocalUserSelfPolicyID],
			// This is vital! {ldap, saml} users shouldn't update local users' info
			// because their usernames clash
			Subjects: []string{"user:local:*"},
			Resource: "users:${a2:username}",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
		constants.PolicyVersionPolicyID: {
			ID:       ids[constants.PolicyVersionPolicyID],
			Subjects: []string{"user:*"},
			Resource: "auth:policies",
			Action:   "read",
			Effect:   "allow",
			Version:  1,
		},
		constants.OcErchefIngestStatusPolicyID: {
			ID:       ids[constants.OcErchefIngestStatusPolicyID],
			Subjects: []string{"tls:service:automate-cs-oc-erchef:*"},
			Resource: "ingest:status",
			Action:   "read",
			Effect:   "allow",
			Version:  1,
		},
		constants.OcErchefIngestEventsPolicyID: {
			ID:       ids[constants.OcErchefIngestEventsPolicyID],
			Subjects: []string{"tls:service:automate-cs-oc-erchef:*"},
			Resource: "ingest:unified_events",
			Action:   "create",
			Effect:   "allow",
			Version:  1,
		},
		constants.CSNginxComplianceProfilesPolicyID: {
			ID:       ids[constants.CSNginxComplianceProfilesPolicyID],
			Subjects: []string{"tls:service:automate-cs-nginx:*"},
			Resource: "compliance:profiles:storage:*",
			Action:   "read",
			Effect:   "allow",
			Version:  1,
		},
		constants.CSNginxComplianceDataCollectorPolicyID: {
			ID:       ids[constants.CSNginxComplianceDataCollectorPolicyID],
			Subjects: []string{"tls:service:automate-cs-nginx:*"},
			Resource: "ingest:unified_events",
			Action:   "create",
			Effect:   "allow",
			Version:  1,
		},
	}
	return defaultPolicies, nil
}
