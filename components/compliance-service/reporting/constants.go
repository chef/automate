package reporting

const ESize = 999999

const (
	UNREPORTED  = "unreported"
	UNREACHABLE = "unreachable"
	UNCOLLECTED = "uncollected"
	COLLECTED   = "collected"
)

// These are filter types where we use ElasticSearch Term Queries
var FilterType = []string{"environment", "organization", "chef_server", "chef_tags",
	"policy_group", "policy_name", "status", "node_name", "platform", "platform_with_version",
	"role", "recipe", "inspec_version", "ipaddress", "resource_type"}

var EndTime = "end_time"

var StartTime = "start_time"
