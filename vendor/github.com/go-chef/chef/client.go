package chef

import "fmt"

type ApiClientService struct {
	client *Client
}

// ApiClient represents the native Go version of the deserialized Client type
//
type ApiClient struct {
	Name       string `json:"name"`
	ClientName string `json:"clientname"`
	OrgName    string `json:"orgname"` // returned after get, not returned after update
	Validator  bool   `json:"validator"`
	JsonClass  string `json:"json_class"`
	ChefType   string `json:"chef_type"`
}

// ApiNewClient structure to request a new client
type ApiNewClient struct {
	Name       string `json:"name,omitempty"` // name or clientname must be specified to create a client
	ClientName string `json:"clientname,omitempty"`
	Validator  bool   `json:"validator,omitempty"`
	Admin      bool   `json:"admin,omitempty"`      // not supported and ignored as of 12.1.0
	CreateKey  bool   `json:"create_key,omitempty"` // not supported for update requests
}

// ApiNewClientResult
type ApiClientCreateResult struct {
	Uri     string  `json:"uri,omitempty"`
	ChefKey ChefKey `json:"chef_key,omitempty"`
}

// ApiClientListResult is map of the client names to client Uri
type ApiClientListResult map[string]string

// String makes ApiClientListResult implement the string result
func (c ApiClientListResult) String() (out string) {
	for k, v := range c {
		out += fmt.Sprintf("%s => %s\n", k, v)
	}
	return out
}

// List lists the clients in the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server/#get-11
func (e *ApiClientService) List() (data ApiClientListResult, err error) {
	err = e.client.magicRequestDecoder("GET", "clients", nil, &data)
	return
}

// Create makes a Client on the chef server
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#clients
func (e *ApiClientService) Create(client ApiNewClient) (data *ApiClientCreateResult, err error) {
	body, err := JSONReader(client)
	if err != nil {
		return
	}
	err = e.client.magicRequestDecoder("POST", "clients", body, &data)
	return
}

// Delete removes a client on the Chef server
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#clients-name
func (e *ApiClientService) Delete(name string) (err error) {
	url := fmt.Sprintf("clients/%s", name)
	err = e.client.magicRequestDecoder("DELETE", url, nil, nil)
	return
}

// Get gets a client from the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#clients-name
func (e *ApiClientService) Get(name string) (client ApiClient, err error) {
	url := fmt.Sprintf("clients/%s", name)
	err = e.client.magicRequestDecoder("GET", url, nil, &client)
	return
}

// Put updates a client on the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#clients-name
func (e *ApiClientService) Update(name string, client ApiNewClient) (data *ApiClient, err error) {
	body, err := JSONReader(client)
	url := fmt.Sprintf("clients/%s", name)
	if err != nil {
		return
	}
	err = e.client.magicRequestDecoder("PUT", url, body, &data)
	return
}

// ListKeys lists the keys associated with a client on the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#clients-client-keys
func (e *ApiClientService) ListKeys(name string) (data []KeyItem, err error) {
	url := fmt.Sprintf("clients/%s/keys", name)
	err = e.client.magicRequestDecoder("GET", url, nil, &data)
	return
}

// AddKey add a key for a client on the Chef server.
// /clients/USERNAME/keys POST
// 201 - created
// 401 - not authenticated
// 403 - not authorizated
// 404 - client doesn't exist
// 409 - new name is already in use
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#clients-name
func (e *ApiClientService) AddKey(name string, keyadd AccessKey) (key KeyItem, err error) {
	url := fmt.Sprintf("clients/%s/keys", name)
	body, err := JSONReader(keyadd)
	err = e.client.magicRequestDecoder("POST", url, body, &key)
	return
}

// DeleteKey delete a key for a client.
// /clients/USERNAME/keys/KEYNAME DELETE
// 200 - successful
// 401 - not authenticated
// 403 - not authorizated
// 404 - client doesn't exist
//
// Chef API docs: https://docs.chef.io/api_chef_server/#clientskeys
func (e *ApiClientService) DeleteKey(name string, keyname string) (key AccessKey, err error) {
	url := fmt.Sprintf("clients/%s/keys/%s", name, keyname)
	err = e.client.magicRequestDecoder("DELETE", url, nil, &key)
	return
}

// GetKey gets a client key from the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#clients-client-keys-key
func (e *ApiClientService) GetKey(name string, keyname string) (key AccessKey, err error) {
	url := fmt.Sprintf("clients/%s/keys/%s", name, keyname)
	err = e.client.magicRequestDecoder("GET", url, nil, &key)
	return
}

// UpdateKey updates a key for a client.
// /clients/USERNAME/keys/KEYNAME PUT
// 200 - successful
// 401 - not authenticated
// 403 - not authorizated
// 404 - client doesn't exist
//
// Chef API docs: https://docs.chef.io/api_chef_server/#clientskeys
func (e *ApiClientService) UpdateKey(name string, keyname string, keyupd AccessKey) (key AccessKey, err error) {
	url := fmt.Sprintf("clients/%s/keys/%s", name, keyname)
	body, err := JSONReader(keyupd)
	err = e.client.magicRequestDecoder("PUT", url, body, &key)
	return
}
