package chef

import (
	"fmt"
	"strings"
)

type UserService struct {
	client *Client
}

// User represents the native Go version of the deserialized User type
type User struct {
	UserName                      string `json:"username,omitempty"` // V1 name instead of name for V0
	DisplayName                   string `json:"display_name,omitempty"`
	Email                         string `json:"email,omitempty"`
	ExternalAuthenticationUid     string `json:"external_authentication_uid,omitempty"` // this or password
	FirstName                     string `json:"first_name,omitempty"`
	FullName                      string `json:"full_name,omitempty"`
	LastName                      string `json:"last_name,omitempty"`
	MiddleName                    string `json:"middle_name,omitempty"`
	Password                      string `json:"password,omitempty"`   // Valid password
	PublicKey                     string `json:"public_key,omitempty"` // not for Create
	RecoveryAuthenticationEnabled bool   `json:"recovery_authentication_enabled,omitempty"`
}

type UserResult struct {
	Uri        string `json:"uri,omitempty"`
	PrivateKey string `json:"private_key,omitempty"`
}

type UserKey struct {
	KeyName        string `json:"name,omitempty"`
	PublicKey      string `json:"public_key,omitempty"`
	ExpirationDate string `json:"expiration_date,omitempty"`
}

type UserKeyResult struct {
	KeyName string `json:"name,omitempty"`
	Uri     string `json:"uri,omitempty"`
	Expired string `json:"expired,omitempty"`
}

// /users GET
// List lists the users in the Chef server.
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#users
func (e *UserService) List(filters ...string) (userlist map[string]string, err error) {
	url := "users"
	if len(filters) > 0 {
		url += "?" + strings.Join(filters, "&")
	}
	err = e.client.magicRequestDecoder("GET", url, nil, &userlist)
	return
}

// /users POST
// Creates a User on the chef server
//  201 =  sucesss
//  400 - invalid  (missing display_name, email,( password or external) among other things)
//        username must be lower case without spaces
//  403 - unauthorized
//  409 - already exists
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#users
func (e *UserService) Create(user User) (data UserResult, err error) {
	body, err := JSONReader(user)
	if err != nil {
		return
	}

	err = e.client.magicRequestDecoder("POST", "users", body, &data)
	return
}

// Delete removes a user on the Chef server
// /users/USERNAME DELETE
//  200 - deleted
//  401 - not authenticated
//  403 - not authorized
//  404 - not found
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#users-name
func (e *UserService) Delete(name string) (err error) {
	err = e.client.magicRequestDecoder("DELETE", "users/"+name, nil, nil)
	return
}

// Get gets a user from the Chef server.
// /users/USERNAME GET
// 200 - got it
// 401 - not authenticated
// 403 - not authorizated
// 404 - user doesn't exist
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#users-name
func (e *UserService) Get(name string) (user User, err error) {
	url := fmt.Sprintf("users/%s", name)
	err = e.client.magicRequestDecoder("GET", url, nil, &user)
	return
}

// TODO:
// API /users/USERNAME GET external_authentication_uid and email filters
// API /users/USERNAME GET verbose parameter
// API /users/USERNAME PUT
// API /users/USERNAME/keys GET
// API /users/USERNAME/keys POST
// API /users/USERNAME/keys/Key DELETE
// API /users/USERNAME/keys/Key GET
// API /users/USERNAME/keys/Key PUT
