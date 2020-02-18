package chef

import "fmt"

type OrganizationService struct {
	client *Client
}

// Organization represents the native Go version of the deserialized Organization type
type Organization struct {
	Name     string `json:"name"`
	FullName string `json:"full_name"`
	Guid     string `json:"guid"`
}

type OrganizationResult struct {
	ClientName string `json:"clientname"`
	PrivateKey string `json:"private_key"`
	Uri        string `json:"uri"`
}

// List lists the organizations in the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#organizations
func (e *OrganizationService) List() (organizationlist map[string]string, err error) {
	err = e.client.magicRequestDecoder("GET", "organizations", nil, &organizationlist)
	return
}

// Get gets an organization from the Chef server.
//
// Chef API docs: http://docs.opscode.com/api_chef_server.html#id28
func (e *OrganizationService) Get(name string) (organization Organization, err error) {
	url := fmt.Sprintf("organizations/%s", name)
	err = e.client.magicRequestDecoder("GET", url, nil, &organization)
	return
}

// Creates an Organization on the chef server
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#organizations
func (e *OrganizationService) Create(organization Organization) (data OrganizationResult, err error) {
	body, err := JSONReader(organization)
	if err != nil {
		return
	}

	var orglist map[string]string
	err = e.client.magicRequestDecoder("POST", "organizations", body, &orglist)
	data.ClientName = orglist["clientname"]
	data.PrivateKey = orglist["private_key"]
	data.Uri = orglist["uri"]
	return
}

// Update an organization on the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#organizations
func (e *OrganizationService) Update(g Organization) (organization Organization, err error) {
	url := fmt.Sprintf("organizations/%s", g.Name)
	body, err := JSONReader(g)
	if err != nil {
		return
	}

	err = e.client.magicRequestDecoder("PUT", url, body, &organization)
	return
}

// Delete removes an organization on the Chef server
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#organizations
func (e *OrganizationService) Delete(name string) (err error) {
	err = e.client.magicRequestDecoder("DELETE", "organizations/"+name, nil, nil)
	return
}
