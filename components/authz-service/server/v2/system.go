package v2

import (
	constants "github.com/chef/automate/components/authz-service/constants/v2"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	uuid "github.com/chef/automate/lib/uuid4"
)

const (
	universalAccessStatementID        = "9edf285c-fc96-4363-a0b9-59e5b46f2a76"
	universalAccessMemberID           = "f04ed84e-37da-4d62-808b-cbf81d2da666"
	ingestNginxMemberID               = "f00f256a-53c9-4311-8f8a-1dda7402ca9f"
	ingestErchefMemberID              = "f777e560-663f-45b0-93a5-f955843be34c"
	ingestProviderStatementID         = "f777e560-663f-45b0-93a5-f955843be34c"
	systemMemberID                    = "123e3a47-4ebd-4162-a836-e2955c5b5034"
	systemLocalUsersMemberID          = "eedcb506-4aa0-4fc4-9800-ea2859b7ffb2"
	systemIntrospectionStatementID    = "4cc6e7cd-5e32-43e5-af04-d32b15796590"
	systemPolicyVersionStatementID    = "173a18a6-c102-4e45-a0d0-dfec68c2eee5"
	allMembersID                      = "ea130a63-6b97-41c9-84c4-b7d074d869ef"
	systemVersionStatementID          = "d7ed1d1f-1f6a-4055-a07e-23430c267e42"
	systemLicenseStatementID          = "e5e1da2d-bbda-4211-adf5-c83e13f1891a"
	systemLocalUsersStatementID       = "e9faa7a9-d458-4009-8192-edee6024b5d9"
	systemLocalUsersDeleteStatementID = "55754dbe-8aec-4346-aadb-90116bc0d867"
	chefManagedPoliciesStatementID    = "2b5a3f1d-9d25-46f3-b1dd-8f46c7d323a3"
	chefManagedRolesStatementID       = "0d516454-7a0c-4922-86f9-8b93d035e869"
	chefManagedProjectsStatementID    = "d49181d1-37b7-459d-8935-f11a9fa6b9e1"
)

// SystemPolicies returns a list of system policies that should always exist by default.
// These should not be visible to the enduser and therefore exist outside of the database.
func SystemPolicies() []*storage.Policy {
	universalAccess := storage.Policy{
		ID: constants.UniversalAccessPolicyID,
		// used by automate-cli for API requests through the gateway
		Name: "deployment-service universal access",
		Members: []storage.Member{
			{
				ID:   uuid.Must(uuid.FromString(universalAccessMemberID)),
				Name: "tls:service:deployment-service:*",
			},
		},
		Statements: []storage.Statement{{
			ID:        uuid.Must(uuid.FromString(universalAccessStatementID)),
			Effect:    storage.Allow,
			Actions:   []string{"*"},
			Resources: []string{"*"},
			Projects:  []string{constants.AllProjectsID},
		}},
	}

	system := storage.Policy{
		ID:   constants.SystemPolicyID,
		Name: "System",
		Members: []storage.Member{
			{
				ID:   uuid.Must(uuid.FromString(systemMemberID)),
				Name: "*",
			},
		},
		Statements: []storage.Statement{
			{
				ID:        uuid.Must(uuid.FromString(systemIntrospectionStatementID)),
				Effect:    storage.Allow,
				Actions:   []string{"iam:introspect:*"},
				Resources: []string{"iam:introspect"},
				Projects:  []string{constants.AllProjectsID},
			},
			{
				ID:        uuid.Must(uuid.FromString(systemVersionStatementID)),
				Effect:    storage.Allow,
				Actions:   []string{"system:serviceVersion:get", "system:serviceVersion:list"},
				Resources: []string{"system:service:version"},
				Projects:  []string{constants.AllProjectsID},
			},
			{
				ID:        uuid.Must(uuid.FromString(systemLicenseStatementID)),
				Effect:    storage.Allow,
				Actions:   []string{"system:license:get"},
				Resources: []string{"system:status"},
				Projects:  []string{constants.AllProjectsID},
			},
			{
				ID:        uuid.Must(uuid.FromString(systemPolicyVersionStatementID)),
				Effect:    storage.Allow,
				Actions:   []string{"iam:policies:get"},
				Resources: []string{"iam:policyVersion"},
				Projects:  []string{constants.AllProjectsID},
			},
		},
	}

	systemLocalUsers := storage.Policy{
		ID:   constants.SystemLocalUsersPolicyID,
		Name: "System Local Users",
		Members: []storage.Member{
			{
				ID:   uuid.Must(uuid.FromString(systemLocalUsersMemberID)),
				Name: "user:local:*",
			},
		},
		Statements: []storage.Statement{
			{
				ID:        uuid.Must(uuid.FromString(systemLocalUsersStatementID)),
				Effect:    storage.Allow,
				Actions:   []string{"iam:users:get", "iam:users:list", "iam:users:update", "iam:usersSelf:update"},
				Resources: []string{"iam:users:${a2:username}", "iam:usersSelf:${a2:username}"},
				Projects:  []string{constants.AllProjectsID},
			},
			{
				ID:        uuid.Must(uuid.FromString(systemLocalUsersDeleteStatementID)),
				Effect:    storage.Deny,
				Actions:   []string{"iam:users:delete"},
				Resources: []string{"iam:users:${a2:username}"},
				Projects:  []string{constants.AllProjectsID},
			},
		},
	}

	systemIngestProviders := storage.Policy{
		ID:   constants.IngestProviderPolicyID,
		Name: "System Ingest Providers",
		Members: []storage.Member{
			{
				ID:   uuid.Must(uuid.FromString(ingestNginxMemberID)),
				Name: "tls:service:automate-cs-nginx:*",
			},
			{
				ID:   uuid.Must(uuid.FromString(ingestErchefMemberID)),
				Name: "tls:service:automate-cs-oc-erchef:*",
			},
		},
		Statements: []storage.Statement{
			{
				ID:        uuid.Must(uuid.FromString(ingestProviderStatementID)),
				Effect:    storage.Allow,
				Actions:   []string{"infra:ingest:*", "compliance:profiles:get", "compliance:profiles:list"},
				Resources: []string{"*"},
				Projects:  []string{constants.AllProjectsID},
			},
		},
	}

	systemChefManaged := storage.Policy{
		ID:   constants.ChefManagedPolicyID,
		Name: "System policy to protect Chef-managed entities",
		Members: []storage.Member{
			{
				ID:   uuid.Must(uuid.FromString(allMembersID)),
				Name: "*",
			},
		},
		Statements: []storage.Statement{
			{
				ID:      uuid.Must(uuid.FromString(chefManagedPoliciesStatementID)),
				Effect:  storage.Deny,
				Actions: []string{"iam:policies:update", "iam:policies:delete"},
				Resources: []string{
					"iam:policies:" + constants.AdminPolicyID,
					"iam:policies:" + constants.EditorPolicyID,
					"iam:policies:" + constants.ViewerPolicyID,
					"iam:policies:" + constants.IngestPolicyID,
				},
				Projects: []string{constants.AllProjectsID},
			},
			{
				ID:      uuid.Must(uuid.FromString(chefManagedRolesStatementID)),
				Effect:  storage.Deny,
				Actions: []string{"iam:roles:update", "iam:roles:delete"},
				Resources: []string{
					"iam:roles:" + constants.OwnerRoleID,
					"iam:roles:" + constants.EditorRoleID,
					"iam:roles:" + constants.ViewerRoleID,
					"iam:roles:" + constants.IngestRoleID,
					"iam:roles:" + constants.ProjectAdminRoleID,
					"iam:roles:" + constants.IAMMembersViewerRoleID,
				},
				Projects: []string{constants.AllProjectsID},
			},
			{
				ID:      uuid.Must(uuid.FromString(chefManagedProjectsStatementID)),
				Effect:  storage.Deny,
				Actions: []string{"iam:projects:update", "iam:projects:delete"},
				Resources: []string{
					"iam:projects:" + constants.AllProjectsID,
				},
				Projects: []string{constants.AllProjectsID},
			},
		},
	}

	return []*storage.Policy{&systemIngestProviders, &universalAccess, &system, &systemLocalUsers, &systemChefManaged}
}
