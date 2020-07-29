package chef

import (
	"fmt"
)

// PolicyGroupService  is the service for interacting with chef server policies endpoint
type PolicyGroupService struct {
	client *Client
}

// PolicyGroupGetResponse is returned from the chef-server for Get Requests to /policy_groups
type PolicyGroupGetResponse map[string]PolicyGroup

type PolicyGroup struct {
	Uri      string              `json:"uri,omitempty"`
	Policies map[string]Revision `json:"policies,omitempty"`
}

type Revision map[string]string

// List lists the policy groups in the Chef server.
// Chef API docs: https://docs.chef.io/api_chef_server/#policy_groups
func (e *PolicyGroupService) List() (data PolicyGroupGetResponse, err error) {
	err = e.client.magicRequestDecoder("GET", "policy_groups", nil, &data)
	return
}

// Get gets the information for a specific policy group
// GET /policy_groups/GROUP
// Chef API docs: https://docs.chef.io/api_chef_server/#policy_groups
func (e *PolicyGroupService) Get(policyGroupName string) (data PolicyGroup, err error) {
	url := fmt.Sprintf("policy_groups/%s", policyGroupName)
	err = e.client.magicRequestDecoder("GET", url, nil, &data)
	return
}

// Delete deletes a policy group.
// DELETE /policy_groups/GROUP
// Chef API docs: https://docs.chef.io/api_chef_server/#policy_groups
func (e *PolicyGroupService) Delete(policyGroupName string) (data PolicyGroup, err error) {
	url := fmt.Sprintf("policy_groups/%s", policyGroupName)
	err = e.client.magicRequestDecoder("DELETE", url, nil, &data)
	return
}

// GetPolicy gets the information for a specific policy in a policy group
// GET /policy_groups/GROUP/policies/NAME
// Chef API docs: https://docs.chef.io/api_chef_server/#policy_groups
func (e *PolicyGroupService) GetPolicy(policyGroupName string, policyName string) (data RevisionDetailsResponse, err error) {
	url := fmt.Sprintf("policy_groups/%s/policies/%s", policyGroupName, policyName)
	err = e.client.magicRequestDecoder("GET", url, nil, &data)
	return
}

// DeletePolicy deletes a specific policy in a policy group
// DELETE /policy_groups/GROUP/policies/NAME
// Chef API docs: https://docs.chef.io/api_chef_server/#policy_groups
func (e *PolicyGroupService) DeletePolicy(policyGroupName string, policyName string) (data RevisionDetailsResponse, err error) {
	url := fmt.Sprintf("policy_groups/%s/policies/%s", policyGroupName, policyName)
	err = e.client.magicRequestDecoder("DELETE", url, nil, &data)
	return
}

// policy_group/GN/policies/PN  oc_chef_wm_named_policy_named_revision.erl
// PUT
