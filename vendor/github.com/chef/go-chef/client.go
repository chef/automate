package chef

import "fmt"

type ApiClientService struct {
	client *Client
}

// Client represents the native Go version of the deserialized Client type
type ApiClient struct {
	Name        string `json:"name"`
	ClientName  string `json:"clientname"`
	OrgName     string `json:"orgname"`
	Admin       bool   `json:"admin"`
	Validator   bool   `json:"validator"`
	Certificate string `json:"certificate,omitempty"`
	PublicKey   string `json:"public_key,omitempty"`
	PrivateKey  string `json:"private_key,omitempty"`
	Uri         string `json:"uri,omitempty"`
	JsonClass   string `json:"json_class"`
	ChefType    string `json:"chef_type"`
}

type ApiNewClient struct {
	Name  string `json:"name"`
	Admin bool   `json:"admin"`
}

type ApiClientCreateResult struct {
	Uri        string `json:"uri,omitempty"`
	PrivateKey string `json:"private_key,omitempty"`
}

type ApiClientListResult map[string]string

type ApiClientKey struct {
	Name           string `json:"name"`
	PublicKey      string `json:"public_key"`
	ExpirationDate string `json:"expiration_date"`
}

type ApiClientKeyListResultItem struct {
	Name    string `json:"name"`
	Expired bool   `json:"expired"`
}

type ApiClientKeyListResult []ApiClientKeyListResultItem

// String makes ApiClientListResult implement the string result
func (c ApiClientListResult) String() (out string) {
	for k, v := range c {
		out += fmt.Sprintf("%s => %s\n", k, v)
	}
	return out
}

// List lists the clients in the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#clients
func (e *ApiClientService) List() (data ApiClientListResult, err error) {
	err = e.client.magicRequestDecoder("GET", "clients", nil, &data)
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

// Create makes a Client on the chef server
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#clients
func (e *ApiClientService) Create(clientName string, admin bool) (data *ApiClientCreateResult, err error) {
	post := ApiNewClient{
		Name:  clientName,
		Admin: admin,
	}
	body, err := JSONReader(post)
	if err != nil {
		return
	}

	err = e.client.magicRequestDecoder("POST", "clients", body, &data)
	return
}

// Put updates a client on the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#clients-name

// Delete removes a client on the Chef server
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#clients-name
func (e *ApiClientService) Delete(name string) (err error) {
	err = e.client.magicRequestDecoder("DELETE", "clients/"+name, nil, nil)
	return
}

// ListKeys lists the keys associated with a client on the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#clients-client-keys
func (e *ApiClientService) ListKeys(clientName string) (data *ApiClientKeyListResult, err error) {
	url := fmt.Sprintf("clients/%s/keys", clientName)
	err = e.client.magicRequestDecoder("GET", url, nil, &data)
	return
}

// GetKey gets a client key from the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#clients-client-keys-key
func (e *ApiClientService) GetKey(clientName string, keyName string) (data *ApiClientKey, err error) {
	url := fmt.Sprintf("clients/%s/keys/%s", clientName, keyName)
	err = e.client.magicRequestDecoder("GET", url, nil, &data)
	return
}
