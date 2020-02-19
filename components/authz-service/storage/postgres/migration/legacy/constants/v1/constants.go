package v1

// These are only used for testing and memstore as the source of truth
// are the migrations that create the actual policies in postgres.
const (
	// AdminPolicyID is the well-known ID of the default admin policy. V1 only.
	AdminPolicyID = "b4b00330-22a4-4cd2-ac2a-820983c1c3b0"

	// CfgmgmtNodesContainerPolicyID correlates to policy for `cfgmgmt:nodes`
	CfgmgmtNodesContainerPolicyID = "ce23fd69-c4bd-4407-961b-60f9b26c4dfa"

	// CfgmgmtNodesWildcardPolicyID correlates to policy for `cfgmgmt:nodes:*`
	CfgmgmtNodesWildcardPolicyID = "76a38aae-a15e-463b-8c2b-4a09c1d62ef6"

	// CfgmgmtStatsWildcardPolicyID correlates to policy for `cfgmgmt:stats:*`
	CfgmgmtStatsWildcardPolicyID = "42ca2668-511a-48e2-b5e2-553b26d34698"

	// ComplianceWildcardPolicyID is the well-known ID of the default policy
	// allowing access to the `compliance:*` wildcard resource to everyone
	ComplianceWildcardPolicyID = "73eb6ea3-55af-4eae-b7e9-4d89334087ed"

	// ServiceInfoWildcardPolicyID is for all the version endpoints in our API.
	ServiceInfoWildcardPolicyID = "e9b37b6a-cf30-4167-bfcb-dfd50e45926c"

	// EventsContainerPolicyID is the well-known ID of the default policy
	// allowing access to the `events` container resource to everyone
	EventsContainerPolicyID = "3b8e626f-a46e-4f1c-8e84-d026111cd566"

	// EventsWildcardPolicyID is the well-known ID of the default policy
	// allowing access to the `events:*` wildcard resource to everyone
	EventsWildcardPolicyID = "fb1bf117-1941-4208-9771-b45bf288ff28"

	// IngestWildcardPolicyID is the well-known ID of the default policy
	// allowing access to the `ingest:*` wildcard resource to everyone
	// TODO: this should be a default policy for non-human actors only
	IngestWildcardPolicyID = "d4167a66-823d-4cf5-a7bb-4702094ec38c"

	// AuthIntrospectionWildcardPolicyID is the well-known ID of the default policy
	// allowing access to the `auth_introspection` wildcard resource to everyone
	// for the action `read`
	AuthIntrospectionWildcardPolicyID = "0632b04d-9453-402e-9ebe-12d8e44711ac"

	// NodesContainerPolicyID is the well-known ID of the default policy
	// allowing access to the `nodes` container resource to everyone
	NodesContainerPolicyID = "3222e45f-7b05-4b49-9bf3-c4357d6c032f"

	// NodesWildcardPolicyID is the well-known ID of the default policy
	// allowing access to the `nodes:*` wildcard resource to everyone
	NodesWildcardPolicyID = "254fdd71-276e-4a21-a43a-5f59e2b09d6e"

	// NodeManagersContainerPolicyID is the well-known ID of the default policy
	// allowing access to the `nodemanagers` container resource to everyone
	NodeManagersContainerPolicyID = "ecf04384-05d6-41b4-9544-9d39a6d2892f"

	// NodeManagersWildcardPolicyID is the well-known ID of the default policy
	// allowing access to the `nodemanagers:*` wildcard resource to everyone
	NodeManagersWildcardPolicyID = "167115ac-361b-448f-b28e-8ab5b16f3a67"

	// SecretsWildcardPolicyID correlates to policy for `secrets:*`
	SecretsWildcardPolicyID = "febfe2ed-be49-47d7-83f3-95b18a48328d" // nolint: gas

	// SecretsContainerPolicyID correlates to policy for `secrets` container
	SecretsContainerPolicyID = "a6a0031a-409a-40b8-83f5-cd3a92e4c527" // nolint: gas

	// Compliance token:* policies
	ComplianceTokenReadProfilesPolicyID   = "3db8c368-9cb2-4839-9de4-57ded0de9a55" // nolint: gas
	ComplianceTokenUploadProfilesPolicyID = "0d16e400-e6db-4694-a39c-a4d61f161bae" // nolint: gas
	ComplianceTokenSearchProfilesPolicyID = "9fb73dc9-ef08-498a-af64-a8a17176f18f" // nolint: gas

	// TelemetryConfigPolicyID - The telemetry config policy ID
	TelemetryConfigPolicyID = "f8d97cb1-a40e-46e4-bba8-a4ae8be51371"

	// LicenseStatusPolicyID is the policy allowing every user to retrieve the
	// currently used license's status
	LicenseStatusPolicyID = "ab2c89a9-a584-4501-b394-ec13d9991d61"

	// ReadOwnUserProfilePolicyID is the policy allowing every local user to read
	// their own `auth:users:USERNAME` resource.
	ReadOwnUserProfilePolicyID = "f9eb8c5a-3b8b-4695-ae39-ca434237f69b"

	// LocalUserSelfPolicyID is the policy allowing every local user to read
	// their own `users:USERNAME` resource for all actions (action *).
	LocalUserSelfPolicyID = "6ac3f062-0a84-437d-9f67-3d343fd8b0b8"

	// PolicyVersionPolicyID is the policy allowing every local user to read
	// the IAM policy version.
	PolicyVersionPolicyID = "7c0f72b0-d706-424e-b6e8-22abaa37d5cc"

	// OcErchefIngestStatusPolicyID is the policy allowing oc erchef to have access to
	// the ingest status endpoint
	OcErchefIngestStatusPolicyID = "8df0a55f-2a93-4119-b84f-f382d5019271"

	// OcErchefIngestEventsPolicyID is the policy allowing oc erchef to have access to
	// the ingest data collector endpoint
	OcErchefIngestEventsPolicyID = "9d67b657-2d37-40f6-a11a-0b6c86aac1d9"

	// CSNginxComplianceProfilesPolicyID is the policy allowing automate-cs-nginx
	// to access to the compliance:profiles:storage:* resource. This allows
	// chef-clients using the audit cookbook to download compliance profiles from
	// A2 using the chef server's client auth without the need of a legacy data
	// collector token.
	CSNginxComplianceProfilesPolicyID = "9ed85167-a659-4415-b984-fde88d68c6f0"

	// CSNginxComplianceDataCollectorPolicyID is the policy allowing automate-cs-nginx
	// to access to the ingest:unified_events resource. This allows chef-clients
	// using the audit cookbook to POST inspec resource reports to the chef server's
	// data collector endpoint and have them proxied to A2's data collector without
	// the need of a legacy data collector token
	CSNginxComplianceDataCollectorPolicyID = "6e792df9-e51f-4474-9539-40ca2a2b308c"

	// ApplicationsServiceGroupsPolicyID correlates to the policy applications:serviceGroups
	ApplicationsServiceGroupsPolicyID = "aee14d59-da0b-4974-ba6d-1a018b024874"
)

// These are only used for testing and memstore purposes.
// If you wish to add a new default policy, you should write a migration,
// please see 02_default_policies.up.sql for details.
var (
	// DefaultPolicyIDs is an array of default policy IDs.
	// Please also update storage.go for testing purposes.
	DeletablePolicyIDs = [...]string{
		CfgmgmtNodesContainerPolicyID,
		CfgmgmtNodesWildcardPolicyID,
		CfgmgmtStatsWildcardPolicyID,
		ComplianceWildcardPolicyID,
		ServiceInfoWildcardPolicyID,
		EventsContainerPolicyID,
		EventsWildcardPolicyID,
		IngestWildcardPolicyID,
		NodesContainerPolicyID,
		NodesWildcardPolicyID,
		NodeManagersContainerPolicyID,
		NodeManagersWildcardPolicyID,
		SecretsWildcardPolicyID,
		SecretsContainerPolicyID,
		ComplianceTokenReadProfilesPolicyID,
		ComplianceTokenUploadProfilesPolicyID,
		ComplianceTokenSearchProfilesPolicyID,
		ReadOwnUserProfilePolicyID,
		LocalUserSelfPolicyID,
		ApplicationsServiceGroupsPolicyID,
	}

	// NonDeletablePolicyIDs is an array of non-deletable policy IDs.
	NonDeletablePolicyIDs = [...]string{
		AdminPolicyID,
		AuthIntrospectionWildcardPolicyID,
		TelemetryConfigPolicyID,
		LicenseStatusPolicyID,
		PolicyVersionPolicyID,
		OcErchefIngestStatusPolicyID,
		OcErchefIngestEventsPolicyID,
		CSNginxComplianceProfilesPolicyID,
		CSNginxComplianceDataCollectorPolicyID,
	}

	DefaultPolicyIDs = append(DeletablePolicyIDs[:], NonDeletablePolicyIDs[:]...)
)
