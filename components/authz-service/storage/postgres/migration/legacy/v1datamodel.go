package legacy

import (
	"time"

	constants "github.com/chef/automate/components/authz-service/storage/postgres/migration/legacy/constants/v1"
	uuid "github.com/chef/automate/lib/uuid4"
)

type v1Policy struct {
	Action    string
	ID        uuid.UUID
	Resource  string
	Subjects  []string
	Effect    string
	CreatedAt time.Time
	UpdatedAt time.Time
	Version   int
}

func v1DefaultPolicies() (map[string]*v1Policy, error) {
	ids := map[string]uuid.UUID{}
	for _, fixedID := range constants.DefaultPolicyIDs {
		id, err := uuid.FromString(fixedID)
		if err != nil {
			return nil, err
		}
		ids[fixedID] = id
	}
	defaultPolicies := map[string]*v1Policy{
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
		constants.ApplicationsServiceGroupsPolicyID: {
			ID:       ids[constants.ApplicationsServiceGroupsPolicyID],
			Subjects: []string{"user:*"},
			Resource: "service_groups",
			Action:   "*",
			Effect:   "allow",
			Version:  1,
		},
	}
	return defaultPolicies, nil
}
