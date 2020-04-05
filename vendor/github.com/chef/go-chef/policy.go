package chef

import (
	"fmt"
)

// PolicyService  is the service for interacting with chef server policies endpoint
type PolicyService struct {
	client *Client
}

// PolicyGetResponse is returned from the chef-server for Get Requests to /policies
type PoliciesGetResponse map[string]Policy
type Policy struct {
	Uri       string                 `json:"uri,omitempty"`
	Revisions map[string]interface{} `json:"revisions,omitempty"`
}

// PolicyGetResponse is returned from chef-server for Get Requests to /policies/<policy-name>
type PolicyGetResponse map[string]PolicyRevision
type PolicyRevision map[string]PolicyRevisionDetail
type PolicyRevisionDetail map[string]interface{}

// RevisionDetailsResponse is returned from the chef-server for Get Requests to /policies/<policy-name>/revisions/<rev-id>
type RevisionDetailsResponse struct {
	RevisionID           string                  `json:"revision_id,omitempty"`
	Name                 string                  `json:"name,omitempty"`
	RunList              []string                `json:"run_list,omitempty"`
	NamedRunList         map[string][]string     `json:"named_run_lists,omitempty"`
	IncludedPolicyLocks  []IncludedPolicyLocks   `json:"included_policy_locks,omitempty"`
	CookbookLocks        map[string]CookbookLock `json:"cookbook_locks,omitempty"`
	DefaultAttributes    map[string]interface{}  `json:"default_attributes,omitempty"`
	OverrideAttributes   map[string]interface{}  `json:"override_attributes,omitempty"`
	SolutionDependencies SolutionDep             `json:"solution_dependencies,omitempty"`
}

// IncludedPolicyLocks are the included policies locks
type IncludedPolicyLocks struct {
	Name          string            `json:"name,omitempty"`
	RevisionID    string            `json:"revision_id,omitempty"`
	SourceOptions map[string]string `json:"source_options,omitempty"`
}

type CookbookLock struct {
	Version          string            `json:"version,omitempty"`
	Identifier       string            `json:"identifier,omitempty"`
	DottedIdentifier string            `json:"dotted_decimal_identifier,omitempty"`
	Source           string            `json:"source,omitempty"`
	CacheKey         string            `json:"cache_key,omitempty"`
	SCM              SCMDetail         `json:"scm_info,omitempty"`
	SourceOptions    map[string]string `json:"source_options,omitempty"`
}
type SCMDetail struct {
	Name                       string   `json:"scm,omitempty"`
	Remote                     string   `json:"remote,omitempty"`
	Revision                   string   `json:"revision,omitempty"`
	WorkingTreeClean           bool     `json:"working_tree_clean,omitempty"`
	Published                  bool     `json:"published,omitempty"`
	SynchronizedRemoteBranches []string `json:"synchronized_remote_branches,omitempty"`
}
type SolutionDep struct {
	PolicyFile   [][]string  `json:"Policyfile,omitempty"`
	Dependencies interface{} `json:"dependencies,omitempty"`
}

// List lists the policies in the Chef server.
// Chef API docs: https://docs.chef.io/api_chef_server/#policies
// GET /policies
func (c *PolicyService) List() (data PoliciesGetResponse, err error) {
	err = c.client.magicRequestDecoder("GET", "policies", nil, &data)
	return
}

// Get retruns details for a specific policy
//  GET /policies/name
func (c *PolicyService) Get(name string) (data PolicyGetResponse, err error) {
	path := fmt.Sprintf("policies/%s", name)
	err = c.client.magicRequestDecoder("GET", path, nil, &data)
	return
}

// GetRevisionDetails retruns details of a specific revision from Chef Server
//  GET /policies/<policy-name>/revisions/<revision-id>
func (c *PolicyService) GetRevisionDetails(policyName string, revisionID string) (data RevisionDetailsResponse, err error) {
	path := fmt.Sprintf("policies/%s/revisions/%s", policyName, revisionID)
	err = c.client.magicRequestDecoder("GET", path, nil, &data)
	return
}
