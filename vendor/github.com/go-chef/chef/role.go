package chef

import "fmt"

type RoleService struct {
	client *Client
}

// Role represents the native Go version of the deserialized Role type
type Role struct {
	Name               string      `json:"name"`
	ChefType           string      `json:"chef_type,omitempty"`
	DefaultAttributes  interface{} `json:"default_attributes,omitempty"`
	Description        string      `json:"description"`
	EnvRunList         EnvRunList  `json:"env_run_lists"`
	JsonClass          string      `json:"json_class,omitempty"`
	OverrideAttributes interface{} `json:"override_attributes,omitempty"`
	RunList            RunList     `json:"run_list"`
}

type RoleCreateResult map[string]string
type RoleEnvironmentsResult []string
type RoleListResult map[string]string

// String makes RoleListResult implement the string result
func (e RoleListResult) String() (out string) {
	return strMapToStr(e)
}

// String makes RoleCreateResult implement the string result
func (e RoleCreateResult) String() (out string) {
	return strMapToStr(e)
}

// List lists the roles in the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#roles
func (e *RoleService) List() (data *RoleListResult, err error) {
	err = e.client.magicRequestDecoder("GET", "roles", nil, &data)
	return
}

// Create a new role in the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#roles
func (e *RoleService) Create(role *Role) (data *RoleCreateResult, err error) {
	body, err := JSONReader(role)
	if err != nil {
		return
	}

	// BUG(fujiN): This is now both a *response* decoder and handles upload.. gettin smelly
	err = e.client.magicRequestDecoder("POST", "roles", body, &data)

	return
}

// Delete a role from the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#roles-name
func (e *RoleService) Delete(name string) (err error) {
	path := fmt.Sprintf("roles/%s", name)
	err = e.client.magicRequestDecoder("DELETE", path, nil, nil)
	return
}

// Get gets a role from the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#roles-name
func (e *RoleService) Get(name string) (data *Role, err error) {
	path := fmt.Sprintf("roles/%s", name)
	err = e.client.magicRequestDecoder("GET", path, nil, &data)
	return
}

// Update a role in the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#roles-name
// Trying to rename a role by specifying a new name in the body returns a 400
func (e *RoleService) Put(role *Role) (data *Role, err error) {
	path := fmt.Sprintf("roles/%s", role.Name)
	body, err := JSONReader(role)
	if err != nil {
		return
	}

	err = e.client.magicRequestDecoder("PUT", path, body, &data)
	return
}

// Get a list of environments that have environment specific run-lists for the given role
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#roles-name-environments
func (e *RoleService) GetEnvironments(role string) (data RoleEnvironmentsResult, err error) {
	path := fmt.Sprintf("roles/%s/environments", role)
	err = e.client.magicRequestDecoder("GET", path, nil, &data)
	return
}

// Get the environment-specific run-list for  a role
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#roles-name-environments-name
func (e *RoleService) GetEnvironmentRunlist(role string, environment string) (data EnvRunList, err error) {
	path := fmt.Sprintf("roles/%s/environments/%s", role, environment)
	err = e.client.magicRequestDecoder("GET", path, nil, &data)
	return
}
