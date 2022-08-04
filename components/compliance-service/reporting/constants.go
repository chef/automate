package reporting

const ESize = 999999

// These are filter types where we use ElasticSearch Term Queries
var FilterType = []string{"environment", "organization", "chef_server", "chef_tags",
	"policy_group", "policy_name", "status", "node_name", "platform", "platform_with_version",
	"role", "recipe", "inspec_version", "ipaddress", "profile_id", "resource_type", "node_id"}
