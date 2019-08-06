package v2

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	constants_v1 "github.com/chef/automate/components/authz-service/constants/v1"
	constants_v2 "github.com/chef/automate/components/authz-service/constants/v2"
	storage_v1 "github.com/chef/automate/components/authz-service/storage/v1"
	storage_v2 "github.com/chef/automate/components/authz-service/storage/v2"
	uuid "github.com/chef/automate/lib/uuid4"
)

func TestV1PolicyMigration(t *testing.T) {
	polID := id(t)
	cases := map[string]struct {
		input  storage_v1.Policy
		checks []checkFunc
	}{
		"custom policy": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "cfgmgmt:nodes",
			},
			checks(
				hasID(polID.String()),
				hasMembers("user:local:albertine", "team:local:admins"),
				hasName(fmt.Sprintf("%s (custom)", polID.String())),
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"v1 default admin policy": {
			wellknown(t, constants_v1.AdminPolicyID),
			checks(isSkipped()),
		},
		"v1 default cfgmgmt nodes container policy": {
			wellknown(t, constants_v1.CfgmgmtNodesContainerPolicyID),
			checks(
				hasID(constants_v2.CfgmgmtPolicyID),
				hasName("[Legacy] Infrastructure Automation Access"),
				hasMembers("user:*"),
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Actions:   []string{"infra:*"},
					Resources: []string{"*"},
				}),
			),
		},
		// for the rest of the cfgmgmt policies, we only check that they've been
		// mapped to the same policy, identified by its ID
		"v1 default cfgmgmt nodes wildcard policy": {
			wellknown(t, constants_v1.CfgmgmtNodesWildcardPolicyID),
			checks(hasID(constants_v2.CfgmgmtPolicyID)),
		},
		"v1 default cfgmgmt stats wildcard policy": {
			wellknown(t, constants_v1.CfgmgmtStatsWildcardPolicyID),
			checks(hasID(constants_v2.CfgmgmtPolicyID)),
		},
		"v1 default compliance wildcard policy": {
			wellknown(t, constants_v1.ComplianceWildcardPolicyID),
			checks(
				hasID(constants_v2.CompliancePolicyID),
				hasName("[Legacy] Compliance Access"),
				hasMembers("user:*"),
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Actions:   []string{"compliance:*"},
					Resources: []string{"*"},
				}),
			),
		},
		"v1 default service info wildcard policy": {
			wellknown(t, constants_v1.ServiceInfoWildcardPolicyID),
			checks(isSkipped()),
		},
		"v1 default events container policy": {
			wellknown(t, constants_v1.EventsContainerPolicyID),
			checks(
				hasID(constants_v2.EventsPolicyID),
				hasName("[Legacy] Events Access"),
				hasMembers("user:*"),
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Actions:   []string{"event:*"},
					Resources: []string{"*"},
				}),
			),
		},
		"v1 default events wildcard policy": {
			wellknown(t, constants_v1.EventsWildcardPolicyID),
			checks(hasID(constants_v2.EventsPolicyID)),
		},
		"v1 default ingest policy": {
			wellknown(t, constants_v1.IngestWildcardPolicyID),
			checks(
				hasID(constants_v2.LegacyIngestPolicyID),
				hasName("[Legacy] Ingest Access"),
				hasMembers("token:*"),
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Actions:   []string{"infra:ingest:*"},
					Resources: []string{"*"},
				}),
			),
		},
		"v1 default authz introspection policy": {
			wellknown(t, constants_v1.AuthIntrospectionWildcardPolicyID),
			checks(isSkipped()),
		},
		"v1 default nodes container policy": {
			wellknown(t, constants_v1.NodesContainerPolicyID),
			checks(
				hasID(constants_v2.NodesPolicyID),
				hasName("[Legacy] Nodes Access"),
				hasMembers("user:*"),
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Actions:   []string{"infra:nodes:*"},
					Resources: []string{"*"},
				}),
			),
		},
		"v1 default nodes wildcard policy": {
			wellknown(t, constants_v1.NodesWildcardPolicyID),
			checks(hasID(constants_v2.NodesPolicyID)),
		},
		"v1 default node managers container policy": {
			wellknown(t, constants_v1.NodeManagersContainerPolicyID),
			checks(
				hasID(constants_v2.NodeManagersPolicyID),
				hasName("[Legacy] Node Managers Access"),
				hasMembers("user:*"),
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Actions:   []string{"infra:nodeManagers:*"},
					Resources: []string{"*"},
				}),
			),
		},
		"v1 default node managers wildcard policy": {
			wellknown(t, constants_v1.NodeManagersWildcardPolicyID),
			checks(hasID(constants_v2.NodeManagersPolicyID)),
		},
		"v1 default secrets container policy": {
			wellknown(t, constants_v1.SecretsContainerPolicyID),
			checks(
				hasID(constants_v2.SecretsPolicyID),
				hasName("[Legacy] Secrets Access"),
				hasMembers("user:*"),
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Actions:   []string{"secrets:*"},
					Resources: []string{"*"},
				}),
			),
		},
		"v1 default secrets wildcard policy": {
			wellknown(t, constants_v1.SecretsWildcardPolicyID),
			checks(hasID(constants_v2.SecretsPolicyID)),
		},
		"v1 default telemetry config policy": {
			wellknown(t, constants_v1.TelemetryConfigPolicyID),
			checks(
				hasID(constants_v2.TelemetryPolicyID),
				hasName("[Legacy] Telemetry Access"),
				hasMembers("user:*"),
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Actions:   []string{"system:telemetryConfig:*"},
					Resources: []string{"*"},
				}),
			),
		},
		"v1 default compliance token read profiles policy": {
			wellknown(t, constants_v1.ComplianceTokenReadProfilesPolicyID),
			checks(
				hasID(constants_v2.ComplianceTokenPolicyID),
				hasName("[Legacy] Compliance Profile Access"),
				hasMembers("token:*"),
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Actions:   []string{"compliance:profiles:*"},
					Resources: []string{"*"},
				}),
			),
		},
		"service_info:version,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "service_info:version",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"system:service:version"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"auth:users:${a2:username},*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "auth:users:${a2:username}",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:users:${a2:username}"},
					Actions:   []string{"*"},
				}),
			),
		},
		"service_info:health,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "service_info:health",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"system:health"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"service_info:*,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "service_info:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"system:*"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"service_info:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "service_info:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"system:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		// TODO this gives access to iam:auth_introspection
		// which is slightly more than originally written but
		// probably fine.
		"auth:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "auth:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"auth:teams,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "auth:teams",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:teams"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"auth:teams:id,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "auth:teams:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:teams:id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"auth:teams,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "auth:teams",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:teams"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"auth:teams:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "auth:teams:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:teams:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"auth:teams:id,update": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "update",
				Resource: "auth:teams:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:teams:id"},
					Actions:   []string{"*:update"},
				}),
			),
		},
		// This policy corresponds to both deleting teams and removing users
		// in IAM v1, but since we are migrating as *:delete for the action,
		// it works for both in IAM v2 still.
		"auth:teams:id,delete": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "delete",
				Resource: "auth:teams:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:teams:id"},
					Actions:   []string{"*:delete"},
				}),
			),
		},
		"auth:teams:id:users,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "auth:teams:id:users",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:teams:id:users"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"auth:users:id:teams,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "auth:users:id:teams",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:users:id:teams"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"auth:api_tokens,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "auth:api_tokens",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:tokens"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"auth:api_tokens,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "auth:api_tokens",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:tokens"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"auth:api_tokens:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "auth:api_tokens:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:tokens:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"auth:api_tokens:id,update": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "update",
				Resource: "auth:api_tokens:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:tokens:id"},
					Actions:   []string{"*:update"},
				}),
			),
		},
		"auth:api_tokens:id,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "auth:api_tokens:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:tokens:id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"auth:api_tokens:id,delete": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "delete",
				Resource: "auth:api_tokens:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:tokens:id"},
					Actions:   []string{"*:delete"},
				}),
			),
		},
		"auth:users:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "auth:users:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:users:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"auth:users,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "auth:users",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:users"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"auth:users:username,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "auth:users:username",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:users:username"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"auth:users,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "auth:users",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:users"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"auth:users:username,delete": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "delete",
				Resource: "auth:users:username",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:users:username"},
					Actions:   []string{"*:delete"},
				}),
			),
		},
		"auth:users:username,update": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "update",
				Resource: "auth:users:username",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:users:username"},
					Actions:   []string{"*:update"},
				}),
			),
		},
		"users:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "users:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:usersSelf:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"users:username,update": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "update",
				Resource: "users:username",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:usersSelf:username"},
					Actions:   []string{"*:update"},
				}),
			),
		},
		"auth:policies,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "auth:policies",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:policies"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"auth:policies,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "auth:policies",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:policies"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"auth:policies:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "auth:policies:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:policies:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"auth:policies:id,delete": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "delete",
				Resource: "auth:policies:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:policies:id"},
					Actions:   []string{"*:delete"},
				}),
			),
		},
		"auth_introspection:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "auth_introspection:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:introspect"},
					Actions:   []string{"*"},
				}),
			),
		},
		"auth_introspection:introspect_all,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "auth_introspection:introspect_all",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:introspect"},
					Actions:   []string{"*:getAll", "*:getSome", "*:get"},
				}),
			),
		},
		"auth_introspection:introspect_some,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "auth_introspection:introspect_some",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:introspect"},
					Actions:   []string{"*:getAll", "*:getSome", "*:get"},
				}),
			),
		},
		"auth_introspection:introspect,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "auth_introspection:introspect",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"iam:introspect"},
					Actions:   []string{"*:getAll", "*:getSome", "*:get"},
				}),
			),
		},
		"cfgmgmt:nodes,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "cfgmgmt:nodes",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"cfgmgmt:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "cfgmgmt:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"cfgmgmt:nodes:node_id:runs,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "cfgmgmt:nodes:node_id:runs",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:node_id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"cfgmgmt:nodes:node_id:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "cfgmgmt:nodes:node_id:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:node_id"},
					Actions:   []string{"*"},
				}),
			),
		},
		"cfgmgmt:nodes:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "cfgmgmt:nodes:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"cfgmgmt:stats:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "cfgmgmt:stats:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes"},
					Actions:   []string{"*"},
				}),
			),
		},
		"cfgmgmt:stats:node_counts,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "cfgmgmt:stats:node_counts",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"cfgmgmt:stats:run_counts,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "cfgmgmt:stats:run_counts",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"cfgmgmt:nodes:node_id:runs:run_id,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "cfgmgmt:nodes:node_id:runs:run_id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:node_id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"cfgmgmt:nodes:node_id:attribute,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "cfgmgmt:nodes:node_id:attribute",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:node_id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"cfgmgmt:nodes:revision_id,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "cfgmgmt:nodes:revision_id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:revision_id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"compliance:profiles:storage:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "compliance:profiles:storage:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:profiles:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"compliance:profiles:storage:owner_id,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "compliance:profiles:storage:owner_id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:profiles:owner_id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"compliance:profiles:storage:owner_id,upload": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "upload",
				Resource: "compliance:profiles:storage:owner_id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:profiles:owner_id"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"compliance:profiles:market,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "compliance:profiles:market",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:marketProfiles"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"compliance:profiles:owner_id,delete": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "delete",
				Resource: "compliance:profiles:owner_id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:profiles:owner_id"},
					Actions:   []string{"*:delete"},
				}),
			),
		},
		"compliance:profiles,search": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "search",
				Resource: "compliance:profiles",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:profiles"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"compliance:reporting:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "compliance:reporting:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"compliance:reporting:reports,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "compliance:reporting:reports",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:reports"},
					Actions:   []string{"*"},
				}),
			),
		},
		"compliance:reporting:reports,search": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "search",
				Resource: "compliance:reporting:reports",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:reports"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"compliance:reporting:reports,export": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "export",
				Resource: "compliance:reporting:reports",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:reports"},
					Actions:   []string{"*:export"},
				}),
			),
		},
		"compliance:reporting:reports:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "compliance:reporting:reports:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:reports:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"compliance:reporting:reports:id,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "compliance:reporting:reports:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:reports:id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"compliance:reporting:suggestions,search": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "search",
				Resource: "compliance:reporting:suggestions",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:suggestions"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"compliance:reporting:profiles,search": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "search",
				Resource: "compliance:reporting:profiles",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:profiles"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"compliance:reporting:nodes:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "compliance:reporting:nodes:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:nodes:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"compliance:reporting:nodes:node_id,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "compliance:reporting:nodes:node_id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:nodes:node_id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"compliance:reporting:nodes,search": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "search",
				Resource: "compliance:reporting:nodes",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:nodes"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"compliance:reporting:version,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "compliance:reporting:version",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:version"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"compliance:reporting:stats:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "compliance:reporting:stats:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:stats:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"compliance:reporting:stats:summary,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "compliance:reporting:stats:summary",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:stats:summary"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"compliance:reporting:stats:trend,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "compliance:reporting:stats:trend",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:stats:trend"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"compliance:reporting:stats:profiles,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "compliance:reporting:stats:profiles",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:stats:profiles"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"compliance:reporting:stats:failures,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "compliance:reporting:stats:failures",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:reporting:stats:failures"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"compliance:scanner:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "compliance:scanner:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:scanner:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"compliance:scanner:jobs,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "compliance:scanner:jobs",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:scanner:jobs"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"compliance:scanner:jobs:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "compliance:scanner:jobs:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:scanner:jobs:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"compliance:scanner:jobs:id,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "compliance:scanner:jobs:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:scanner:jobs:id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"compliance:scanner:jobs:id,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "compliance:scanner:jobs:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:scanner:jobs:id"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"compliance:scanner:jobs:id,update": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "update",
				Resource: "compliance:scanner:jobs:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:scanner:jobs:id"},
					Actions:   []string{"*:update"},
				}),
			),
		},
		"compliance:scanner:jobs:id,delete": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "delete",
				Resource: "compliance:scanner:jobs:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:scanner:jobs:id"},
					Actions:   []string{"*:delete"},
				}),
			),
		},
		"compliance:scanner:jobs:id,rerun": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "rerun",
				Resource: "compliance:scanner:jobs:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:scanner:jobs:id"},
					Actions:   []string{"*:rerun"},
				}),
			),
		},
		"compliance:scanner:jobs,search": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "search",
				Resource: "compliance:scanner:jobs",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"compliance:scanner:jobs"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"events,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "events",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"event:events"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"events:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "events:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"events:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"events:types,count": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "count",
				Resource: "events:types",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"event:events"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"events:tasks,count": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "count",
				Resource: "events:tasks",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"event:events"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"events:strings,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "events:strings",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"event:events"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"ingest:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "ingest:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:*"},
					Actions:   []string{"infra:ingest:*"},
				}),
			),
		},
		"ingest:*,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "ingest:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:*"},
					Actions:   []string{"infra:ingest:create", "infra:ingestUnifiedEvents:create"},
				}),
			),
		},
		"ingest:*,delete": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "delete",
				Resource: "ingest:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:*"},
					Actions:   []string{"infra:ingest:delete"},
				}),
			),
		},
		"ingest:*,status": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "status",
				Resource: "ingest:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:*"},
					Actions:   []string{"infra:ingestStatus:get"},
				}),
			),
		},
		"ingest:nodes:entity_uuid:runs,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "ingest:nodes:entity_uuid:runs",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:entity_uuid:runs"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"ingest:nodes:entity_uuid:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "ingest:nodes:entity_uuid:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:entity_uuid:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"ingest:actions,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "ingest:actions",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:actions"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"ingest:nodes,delete": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "delete",
				Resource: "ingest:nodes",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes"},
					Actions:   []string{"*:delete"},
				}),
			),
		},
		"ingest:nodes:entity_uuid:liveness,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "ingest:nodes:entity_uuid:liveness",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:entity_uuid:liveness"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"cfgmgmt:nodes:marked-nodes,delete": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "delete",
				Resource: "cfgmgmt:nodes:marked-nodes",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:markedNodes"},
					Actions:   []string{"*:delete"},
				}),
			),
		},
		"ingest:unified_events,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "ingest:unified_events",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:unifiedEvents"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"ingest:status,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "ingest:status",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:ingest:status"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"license,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "license",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"system:license"},
					Actions:   []string{"*"},
				}),
			),
		},
		"license,apply": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "apply",
				Resource: "license",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"system:license"},
					Actions:   []string{"*:apply"},
				}),
			),
		},
		"license:status,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "license:status",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"system:status"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"license:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "license:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"system:status"},
					Actions:   []string{"*"},
				}),
			),
		},
		"nodemanagers,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "nodemanagers",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodeManagers"},
					Actions:   []string{"*"},
				}),
			),
		},
		"nodemanagers,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "nodemanagers",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodeManagers"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"nodemanagers:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "nodemanagers:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodeManagers:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"nodemanagers:id,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "nodemanagers:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodeManagers:id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"nodemanagers:id,list": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "list",
				Resource: "nodemanagers:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodeManagers:id"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"nodemanagers:id,update": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "update",
				Resource: "nodemanagers:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodeManagers:id"},
					Actions:   []string{"*:update"},
				}),
			),
		},
		"nodemanagers:id,delete": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "delete",
				Resource: "nodemanagers:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodeManagers:id"},
					Actions:   []string{"*:delete"},
				}),
			),
		},
		"nodemanagers:id:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "nodemanagers:id:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodeManagers:id:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"nodemanagers:id:fields,search": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "search",
				Resource: "nodemanagers:id:fields",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodeManagers:id:fields"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"nodemanagers:id:nodes,search": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "search",
				Resource: "nodemanagers:id:nodes",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodeManagers:id:nodes"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"nodes,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "nodes",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes"},
					Actions:   []string{"*"},
				}),
			),
		},
		"nodes,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "nodes",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"nodes:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "nodes:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"nodes:id,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "nodes:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"nodes:id,update": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "update",
				Resource: "nodes:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:id"},
					Actions:   []string{"*:update"},
				}),
			),
		},
		"nodes:id,delete": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "delete",
				Resource: "nodes:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:id"},
					Actions:   []string{"*:delete"},
				}),
			),
		},
		"nodes,list": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "list",
				Resource: "nodes",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"nodes:id,rerun": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "rerun",
				Resource: "nodes:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"infra:nodes:id"},
					Actions:   []string{"*:rerun"},
				}),
			),
		},
		"notifications:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "notifications:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"notifications:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"notifications:rules,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "notifications:rules",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"notifications:rules"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"notifications:rules:id,delete": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "delete",
				Resource: "notifications:rules:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"notifications:rules:id"},
					Actions:   []string{"*:delete"},
				}),
			),
		},
		"notifications:rules:id,update": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "update",
				Resource: "notifications:rules:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"notifications:rules:id"},
					Actions:   []string{"*:update"},
				}),
			),
		},
		"notifications:rules:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "notifications:rules:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"notifications:rules:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"notifications:rules:id,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "notifications:rules:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"notifications:rules:id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"notifications:rules,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "notifications:rules",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"notifications:rules"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"notifications:rules,validate": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "validate",
				Resource: "notifications:rules",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"notifications:rules"},
					Actions:   []string{"*:validate"},
				}),
			),
		},
		"secrets,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "secrets",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"secrets:secrets"},
					Actions:   []string{"*"},
				}),
			),
		},
		"secrets,create": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "create",
				Resource: "secrets",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"secrets:secrets"},
					Actions:   []string{"*:create"},
				}),
			),
		},
		"secrets,search": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "search",
				Resource: "secrets",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"secrets:secrets"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"secrets:id,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "secrets:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"secrets:secrets:id"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"secrets:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "secrets:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"secrets:secrets:*"},
					Actions:   []string{"*"},
				}),
			),
		},
		"secrets:id,update": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "update",
				Resource: "secrets:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"secrets:secrets:id"},
					Actions:   []string{"*:update"},
				}),
			),
		},
		"secrets:id,delete": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "delete",
				Resource: "secrets:id",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"secrets:secrets:id"},
					Actions:   []string{"*:delete"},
				}),
			),
		},
		"telemetry:*,*": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "*",
				Resource: "telemetry:*",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"system:config"},
					Actions:   []string{"*"},
				}),
			),
		},
		"telemetry:config,read": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "read",
				Resource: "telemetry:config",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"system:config"},
					Actions:   []string{"*:get", "*:list"},
				}),
			),
		},
		"telemetry:config,update": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "update",
				Resource: "telemetry:config",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"system:config"},
					Actions:   []string{"*:update"},
				}),
			),
		},
		"service_groups,list": {
			storage_v1.Policy{
				ID:       polID,
				Subjects: []string{"user:local:albertine", "team:local:admins"},
				Action:   "list",
				Resource: "service_groups",
			},
			checks(
				hasStatements(storage_v2.Statement{
					Effect:    storage_v2.Allow,
					Resources: []string{"applications:serviceGroups"},
					Actions:   []string{"*:list"},
				}),
			),
		},
		"v1 default compliance token upload profile policy": {
			wellknown(t, constants_v1.ComplianceTokenUploadProfilesPolicyID),
			checks(hasID(constants_v2.ComplianceTokenPolicyID)),
		},
		"v1 default compliance token search profile policy": {
			wellknown(t, constants_v1.ComplianceTokenSearchProfilesPolicyID),
			checks(hasID(constants_v2.ComplianceTokenPolicyID)),
		},
		"v1 default license status policy": {
			wellknown(t, constants_v1.LicenseStatusPolicyID),
			checks(isSkipped()),
		},
		"v1 default read own user profile policy": {
			wellknown(t, constants_v1.ReadOwnUserProfilePolicyID),
			checks(isSkipped()),
		},
		"v1 default update own user record policy": {
			wellknown(t, constants_v1.LocalUserSelfPolicyID),
			checks(isSkipped()),
		},
		"v1 default read policy version policy": {
			wellknown(t, constants_v1.PolicyVersionPolicyID),
			checks(isSkipped()),
		},
		"v1 default oc-erchef ingest status policy": {
			wellknown(t, constants_v1.OcErchefIngestStatusPolicyID),
			checks(isSkipped()),
		},
		"v1 default oc-erchef ingest events policy": {
			wellknown(t, constants_v1.OcErchefIngestEventsPolicyID),
			checks(isSkipped()),
		},
		"v1 default cn-nginx compliance profiles policy": {
			wellknown(t, constants_v1.CSNginxComplianceProfilesPolicyID),
			checks(isSkipped()),
		},
		"v1 default cn-nginx ingest events policy": {
			wellknown(t, constants_v1.CSNginxComplianceDataCollectorPolicyID),
			checks(isSkipped()),
		},
		"v1 default application policy": {
			wellknown(t, constants_v1.ApplicationsServiceGroupsPolicyID),
			checks(isSkipped()),
		},
	}

	for desc, test := range cases {
		t.Run(desc, func(t *testing.T) {
			pol, err := migrateV1Policy(&test.input)
			require.NoError(t, err)
			for _, check := range test.checks {
				check(t, pol)
			}
		})
	}
}

func TestCheckForAdminTokenPolicyConfirmsNotAnAdminPolicy(t *testing.T) {
	testCases := map[string]*storage_v1.Policy{
		"policy with multiple subjects": {
			ID:       id(t),
			Subjects: []string{"user:local:albertine", "team:local:admins"},
			Action:   "update",
			Resource: "telemetry:config",
		},
		"policy with two tokens as subjects": {
			ID:       id(t),
			Subjects: []string{"token:282f41f1-e763-4094-9c59-c4eec1b71532", "token:282f41f1-e763-4094-9c59-c4eec1b71532"},
			Action:   "*",
			Resource: "*",
		},
		"policy with two subjects but only one token": {
			ID:       id(t),
			Subjects: []string{"token:282f41f1-e763-4094-9c59-c4eec1b71532", "team:local:admins"},
			Action:   "*",
			Resource: "*",
		},
		"policy where action is not '*'": {
			ID:       id(t),
			Subjects: []string{"token:282f41f1-e763-4094-9c59-c4eec1b71532"},
			Action:   "update",
			Resource: "*",
		},
		"policy where resource is not '*'": {
			ID:       id(t),
			Subjects: []string{"token:282f41f1-e763-4094-9c59-c4eec1b71532"},
			Action:   "*",
			Resource: "telemetry:config",
		},
		"policy where subject is not a token": {
			ID:       id(t),
			Subjects: []string{"user:local:marie"},
			Action:   "*",
			Resource: "*",
		},
		"policy with empty subjects": {
			ID:       id(t),
			Subjects: []string{},
			Action:   "*",
			Resource: "*",
		},
	}

	for desc, pol := range testCases {
		t.Run(desc, func(t *testing.T) {
			adminPol, err := checkForAdminTokenPolicy(pol)
			assert.Nil(t, adminPol)
			assert.Nil(t, err)
		})
	}
}

func TestCheckForAdminTokenPolicyIsAdminTokenPolicy(t *testing.T) {
	pol := &storage_v1.Policy{
		ID:       id(t),
		Subjects: []string{"token:282f41f1-e763-4094-9c59-c4eec1b71532"},
		Action:   "*",
		Resource: "*",
	}

	adminPol, err := checkForAdminTokenPolicy(pol)
	assert.NotNil(t, adminPol)
	assert.Nil(t, err)
}

func wellknown(t *testing.T, wellknownID string) storage_v1.Policy {
	v1DefaultPols, err := storage_v1.DefaultPolicies()
	require.NoError(t, err)
	inputPol, found := v1DefaultPols[wellknownID]
	require.True(t, found)
	return *inputPol
}

type checkFunc func(*testing.T, *storage_v2.Policy)

func checks(checks ...checkFunc) []checkFunc {
	return checks
}

func isSkipped() checkFunc {
	return func(t *testing.T, pol *storage_v2.Policy) {
		assert.Nil(t, pol)
	}
}

func hasID(id string) checkFunc {
	return func(t *testing.T, pol *storage_v2.Policy) {
		assert.Equal(t, id, pol.ID)
	}
}

func hasName(name string) checkFunc {
	return func(t *testing.T, pol *storage_v2.Policy) {
		assert.Equal(t, name, pol.Name)
	}
}

func hasStatements(expected ...storage_v2.Statement) checkFunc {
	// expected statement in test case does not initialize projects slice
	// so we initialize it here
	for i := range expected {
		if expected[i].Projects == nil {
			expected[i].Projects = []string{}
		}
	}
	return func(t *testing.T, pol *storage_v2.Policy) {
		assert.ElementsMatch(t, expected, pol.Statements)
	}
}

func hasMembers(s ...string) checkFunc {
	return func(t *testing.T, pol *storage_v2.Policy) {
		assert.ElementsMatch(t, s, storage_v2.MemberSliceToStringSlice(pol.Members))
	}
}

func id(t *testing.T) uuid.UUID {
	t.Helper()
	i, err := uuid.NewV4()
	require.NoError(t, err)
	return i
}
