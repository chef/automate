package v2

import (
	constants "github.com/chef/automate/components/authz-service/constants/v2"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
)

// SystemPolicies returns a list of system policies that should always exist by default.
// These should not be visible to the enduser and therefore exist outside of the database.
func SystemPolicies() []*storage.Policy {
	universalAccess := storage.Policy{
		ID:   constants.UniversalAccessPolicyID,
		Type: storage.System,
		// used by automate-cli for API requests through the gateway
		Name: "deployment-service universal access",
		Members: []storage.Member{
			{
				Name: "tls:service:deployment-service:*",
			},
		},
		Statements: []storage.Statement{{
			Effect:    storage.Allow,
			Actions:   []string{"*"},
			Resources: []string{"*"},
			Projects:  []string{constants.AllProjectsID},
		}},
	}

	system := storage.Policy{
		ID:   constants.SystemPolicyID,
		Type: storage.System,
		Name: "System",
		Members: []storage.Member{
			{
				Name: "*",
			},
		},
		Statements: []storage.Statement{
			{
				Effect:    storage.Allow,
				Actions:   []string{"iam:introspect:*"},
				Resources: []string{"iam:introspect"},
				Projects:  []string{constants.AllProjectsID},
			},
			{
				Effect:    storage.Allow,
				Actions:   []string{"system:serviceVersion:get", "system:serviceVersion:list"},
				Resources: []string{"system:service:version"},
				Projects:  []string{constants.AllProjectsID},
			},
			{
				Effect:    storage.Allow,
				Actions:   []string{"system:license:get"},
				Resources: []string{"system:status"},
				Projects:  []string{constants.AllProjectsID},
			},
			{
				Effect:    storage.Allow,
				Actions:   []string{"iam:policies:get"},
				Resources: []string{"iam:policyVersion"},
				Projects:  []string{constants.AllProjectsID},
			},
		},
	}

	systemLocalUsers := storage.Policy{
		ID:   constants.SystemLocalUsersPolicyID,
		Type: storage.System,
		Name: "System Local Users",
		Members: []storage.Member{
			{
				Name: "user:local:*",
			},
		},
		Statements: []storage.Statement{
			{
				Effect:    storage.Allow,
				Actions:   []string{"iam:users:get", "iam:users:list", "iam:users:update", "iam:usersSelf:update"},
				Resources: []string{"iam:users:${a2:username}", "iam:usersSelf:${a2:username}"},
				Projects:  []string{constants.AllProjectsID},
			},
			{
				Effect:    storage.Deny,
				Actions:   []string{"iam:users:delete"},
				Resources: []string{"iam:users:${a2:username}"},
				Projects:  []string{constants.AllProjectsID},
			},
		},
	}

	systemIngestProviders := storage.Policy{
		ID:   constants.IngestProviderPolicyID,
		Type: storage.System,
		Name: "System Ingest Providers",
		Members: []storage.Member{
			{
				Name: "tls:service:automate-cs-nginx:*",
			},
			{
				Name: "tls:service:automate-cs-oc-erchef:*",
			},
		},
		Statements: []storage.Statement{
			{
				Effect:    storage.Allow,
				Actions:   []string{"infra:ingest:*", "compliance:profiles:get", "compliance:profiles:list"},
				Resources: []string{"*"},
				Projects:  []string{constants.AllProjectsID},
			},
		},
	}

	systemChefManaged := storage.Policy{
		ID:   constants.ChefManagedPolicyID,
		Type: storage.System,
		Name: "System policy to protect Chef-managed entities",
		Members: []storage.Member{
			{
				Name: "*",
			},
		},
		Statements: []storage.Statement{
			{
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
