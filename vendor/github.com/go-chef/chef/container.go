package chef

import "fmt"

type ContainerService struct {
	client *Client
}

// Container represents the native Go version of the deserialized Container type
//
type Container struct {
	ContainerName string `json:"containername"`
	ContainerPath string `json:"containerpath"`
}

//NewContainerResult
type ContainerCreateResult struct {
	Uri string `json:"uri,omitempty"`
}

// ContainerListResult is map of the container names to container Uri
type ContainerListResult map[string]string

// String makes ContainerListResult implement the string result
func (c ContainerListResult) String() (out string) {
	for k, v := range c {
		out += fmt.Sprintf("%s => %s\n", k, v)
	}
	return out
}

// List lists the containers in the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server/containers
func (e *ContainerService) List() (data ContainerListResult, err error) {
	err = e.client.magicRequestDecoder("GET", "containers", nil, &data)
	return
}

// Create makes a Container on the chef server
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#containers
func (e *ContainerService) Create(container Container) (data *ContainerCreateResult, err error) {
	body, err := JSONReader(container)
	if err != nil {
		return
	}
	err = e.client.magicRequestDecoder("POST", "containers", body, &data)
	return
}

// Delete removes a containers on the Chef server
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#container
func (e *ContainerService) Delete(name string) (err error) {
	url := fmt.Sprintf("containers/%s", name)
	err = e.client.magicRequestDecoder("DELETE", url, nil, nil)
	return
}

// Get gets a container from the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#containers
func (e *ContainerService) Get(name string) (container Container, err error) {
	url := fmt.Sprintf("containers/%s", name)
	err = e.client.magicRequestDecoder("GET", url, nil, &container)
	return
}
