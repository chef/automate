package chef

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
