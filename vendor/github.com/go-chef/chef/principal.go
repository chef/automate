package chef

import "fmt"

type PrincipalService struct {
	client *Client
}

// Principal represents the native Go version of the deserialized Principal type
type Principal struct {
	Principals []Principals `json:"principals"`
}

type Principals struct {
	Name      string `json:"name"`
	Type      string `json:"type"`
	PublicKey string `json:"public_key"`
	AuthzId   string `json:"authz_id"`
	OrgMember bool   `json:"org_member"`
}

// Get gets a principal from the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#principals-name
func (e *PrincipalService) Get(name string) (principal Principal, err error) {
	url := fmt.Sprintf("principals/%s", name)
	err = e.client.magicRequestDecoder("GET", url, nil, &principal)
	return
}
