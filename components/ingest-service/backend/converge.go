//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package backend

import "time"

// Run is the representation of a Run
type Run struct {
	NodeInfo
	Resources            []Resource `json:"resources"`
	RunID                string     `json:"run_id"`
	StartTime            time.Time  `json:"start_time"`
	EndTime              time.Time  `json:"end_time"`
	UpdatedResourceCount int        `json:"updated_resource_count"`
}

// UpsertNode The node data used when the node is first created in the node-state index.
type UpsertNode struct {
	Node
	Created time.Time `json:"created,omitempty"`
}

// NodeRunDateInfo The node data to store latest run datetime in elasticsearch
type NodeRunDateInfo struct {
	NodeID   string    `json:"node_uuid"`
	FirstRun time.Time `json:"first_run"`
	LastRun  time.Time `json:"last_run"`
}

// Node is the representation of a Node
type Node struct {
	NodeInfo
	Checkin           time.Time `json:"checkin"`
	LatestRunID       string    `json:"latest_run_id"`
	Exists            bool      `json:"exists"`
	Attributes        []string  `json:"attributes"`
	Ec2               Ec2       `json:"ec2,omitempty"`
	LastCCRReceived   time.Time `json:"lastCCRReceived,omitempty"`
	LivenessManaged   bool      `json:"liveness_managed,omitempty"`
	CloudID           string    `json:"cloud_id,omitempty"`
	CloudAccountID    string    `json:"cloud_account_id,omitempty"`
	CloudRegion       string    `json:"cloud_region,omitempty"`
	HasDeprecations   bool      `json:"has_deprecations"`
	DeprecationsCount int       `json:"deprecations_count"`
	Projects          []string  `json:"projects"`
	ErrorMessage      string    `json:"error_message,omitempty"`
	ErrorType         string    `json:"error_type,omitempty"`
}

// NodeAttribute is the representation of the
// attributes of a Node
type NodeAttribute struct {
	EntityUUID          string    `json:"entity_uuid"`
	Name                string    `json:"name"`
	RunList             []string  `json:"run_list"`
	ChefEnvironment     string    `json:"chef_environment"`
	Normal              string    `json:"normal"`
	NormalValueCount    int       `json:"normal_value_count"`
	Default             string    `json:"default"`
	DefaultValueCount   int       `json:"default_value_count"`
	Override            string    `json:"override"`
	OverrideValueCount  int       `json:"override_value_count"`
	Automatic           string    `json:"automatic"`
	AutomaticValueCount int       `json:"automatic_value_count"`
	AllValueCount       int       `json:"all_value_count"`
	LastUpdate          time.Time `json:"last_update"`
}

// NodeInfo defines common fields between Run and Node types
type NodeInfo struct {
	EntityUuid       string   `json:"entity_uuid"`
	EventAction      string   `json:"event_action"`
	NodeName         string   `json:"node_name"`
	OrganizationName string   `json:"organization_name"`
	RunList          []string `json:"run_list"`
	Source           string   `json:"source"`
	// In the backend, status may be modified by other systems/processes and have
	// new values, for example to mark a node missing
	Status string `json:"status"`
	// ChefRunStatus should not be modified and should always contain the Chef
	// Run's success/failure outcome. This is used to with elasticsearch
	// aggregations to compute success/failure statistics across different node
	// cohorts
	ChefRunStatus         string              `json:"chef_run_status"`
	TotalResourceCount    int                 `json:"total_resource_count"`
	Deprecations          []Deprecation       `json:"deprecations,omitempty"`
	Error                 ChefError           `json:"error,omitempty"`
	Tags                  []string            `json:"tags,omitempty"`
	ResourceNames         []string            `json:"resource_names"`
	Recipes               []string            `json:"recipes"`
	ChefTags              []string            `json:"chef_tags"`
	Cookbooks             []string            `json:"cookbooks"`
	Platform              string              `json:"platform"`
	PlatformFamily        string              `json:"platform_family"`
	PlatformVersion       string              `json:"platform_version"`
	ChefVersion           string              `json:"chef_version"`
	UptimeSeconds         int64               `json:"uptime_seconds"`
	Environment           string              `json:"environment"`
	Roles                 []string            `json:"roles"`
	PolicyName            string              `json:"policy_name"`
	PolicyGroup           string              `json:"policy_group"`
	PolicyRevision        string              `json:"policy_revision"`
	Fqdn                  string              `json:"fqdn"`
	Ipaddress             interface{}         `json:"ipaddress"`
	SourceFqdn            string              `json:"source_fqdn"`
	ExpandedRunList       ExpandedRunList     `json:"expanded_run_list"`
	Timestamp             time.Time           `json:"timestamp"`
	VersionedCookbooks    []VersionedCookbook `json:"versioned_cookbooks"`
	CloudProvider         string              `json:"cloud_provider"`
	Timezone              string              `json:"timezone"`
	KernelRelease         string              `json:"kernel_release"`
	KernelVersion         string              `json:"kernel_version"`
	VirtualizationSystem  string              `json:"virtualization_system"`
	VirtualizationRole    string              `json:"virtualization_role"`
	DmiSystemManufacturer string              `json:"dmi_system_manufacturer"`
	DmiSystemSerialNumber string              `json:"dmi_system_serial_number"`
	Domain                string              `json:"domain"`
	Hostname              string              `json:"hostname"`
	Macaddress            string              `json:"macaddress"`
	MemoryTotal           string              `json:"memory_total"`
	Ip6address            interface{}         `json:"ip6address"`
}

type Ec2 struct {
	InstanceId                string      `json:"instance_id"`
	InstanceType              string      `json:"instance_type"`
	PublicIpv4                interface{} `json:"public_ipv4"` // An interface bc ES type is `ip`
	PlacementAvailabilityZone string      `json:"placement_availability_zone"`
	Region                    string      `json:"region"`
	AccountID                 string      `json:"account_id"`
}

// Resource is the representation of a Chef Resource
type Resource struct {
	Type            string    `json:"type"`
	Name            string    `json:"name"`
	ID              string    `json:"id"`
	Duration        string    `json:"duration"`
	Delta           string    `json:"delta"`
	IgnoreFailure   bool      `json:"ignore_failure,omitempty"`
	Result          string    `json:"result"`
	Status          string    `json:"status"`
	CookbookName    string    `json:"cookbook_name,omitempty"`
	CookbookVersion string    `json:"cookbook_version,omitempty"`
	CookbookType    string    `json:"cookbook_type,omitempty"`
	RecipeName      string    `json:"recipe_name,omitempty"`
	Conditional     string    `json:"conditional,omitempty"`
	Error           ChefError `json:"error,omitempty"`
}

type Deprecation struct {
	Message  string `json:"message"`
	URL      string `json:"url"`
	Location string `json:"location"`
}

type ChefErrorDescription struct {
	Title    string                   `json:"title"`
	Sections []map[string]interface{} `json:"sections"`
}

// ChefError as reported by chef client converge message
type ChefError struct {
	Class       string               `json:"class"`
	Message     string               `json:"message"`
	Backtrace   []string             `json:"backtrace"`
	Description ChefErrorDescription `json:"description"`
}

// ExpandedRunList
type ExpandedRunList struct {
	ID      string                   `json:"id"`
	RunList []ExpandedRunListRunList `json:"run_list"`
}

type ExpandedRunListRunList struct {
	Type     string                   `json:"type"`
	Name     string                   `json:"name"`
	Version  interface{}              `json:"version"`
	Skipped  bool                     `json:"skipped"`
	Children []ExpandedRunListRunList `json:"children,omitempty"`
}

// VersionedCookbook links cookbook names and versions
type VersionedCookbook struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

type NodeBasics struct {
	EntityUuid string    `json:"entity_uuid"`
	Checkin    time.Time `json:"checkin"`
}
