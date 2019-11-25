package chef

import "fmt"

type GroupService struct {
	client *Client
}

// Group represents the native Go version of the deserialized Group type
type Group struct {
	Name      string   `json:"name"`
	GroupName string   `json:"groupname"`
	OrgName   string   `json:"orgname"`
	Actors    []string `json:"actors"`
	Clients   []string `json:"clients"`
	Groups    []string `json:"groups"`
	Users     []string `json:"users"`
}

type GroupResult struct {
	Uri string `json:"uri"`
}

// List lists the groups in the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#groups
func (e *GroupService) List() (grouplist map[string]string, err error) {
	err = e.client.magicRequestDecoder("GET", "groups", nil, &grouplist)
	return
}

// Get gets a group from the Chef server.
//
// Chef API docs: http://docs.opscode.com/api_chef_server.html#id28
func (e *GroupService) Get(name string) (group Group, err error) {
	url := fmt.Sprintf("groups/%s", name)
	err = e.client.magicRequestDecoder("GET", url, nil, &group)
	return
}

// Creates a Group on the chef server
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#groups
func (e *GroupService) Create(group Group) (data *GroupResult, err error) {
	body, err := JSONReader(group)
	if err != nil {
		return
	}

	err = e.client.magicRequestDecoder("POST", "groups", body, &data)
	return
}

// Update a group on the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#groups
func (e *GroupService) Update(g Group) (group Group, err error) {
	url := fmt.Sprintf("groups/%s", g.Name)
	body, err := JSONReader(g)
	if err != nil {
		return
	}

	err = e.client.magicRequestDecoder("PUT", url, body, &group)
	return
}

// Delete removes a group on the Chef server
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#groups
func (e *GroupService) Delete(name string) (err error) {
	err = e.client.magicRequestDecoder("DELETE", "groups/"+name, nil, nil)
	return
}
