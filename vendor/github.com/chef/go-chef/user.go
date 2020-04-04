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
	LastName                      string `json:"last_name,omitempty"`
	MiddleName                    string `json:"middle_name,omitempty"`
	Password                      string `json:"password,omitempty"`   // Valid password
	CreateKey                     bool   `json:"create_key,omitempty"` // Cannot be passed with PublicKey
	PublicKey                     string `json:"public_key,omitempty"` // Cannot be passed with CreateKey
	RecoveryAuthenticationEnabled bool   `json:"recovery_authentication_enabled,omitempty"`
}

type UserResult struct {
	Uri     string  `json:"uri,omitempty"`
	ChefKey ChefKey `json:"chef_key,omitempty"`
}

type ChefKey struct {
	Name           string `json:"name"`
	PublicKey      string `json:"public_key"`
	ExpirationDate string `json:"expiration_date"`
	Uri            string `json:"uri"`
	PrivateKey     string `json:"private_key"`
}

type UserVerboseResult struct {
	Email     string `json:"email,omitempty"`
	FirstName string `json:"first_name,omitempty"`
	LastName  string `json:"last_name,omitempty"`
}

type UserKey struct {
	KeyName        string `json:"name,omitempty"`
	PublicKey      string `json:"public_key,omitempty"`
	ExpirationDate string `json:"expiration_date,omitempty"`
}

type UserKeyItem struct {
	KeyName string `json:"name,omitempty"`
	Uri     string `json:"uri,omitempty"`
	Expired bool   `json:"expired,omitempty"`
}

// List lists the users in the Chef server.
// /users GET
// Chef API docs: https://docs.chef.io/api_chef_server.html#users
func (e *UserService) List(filters ...string) (userlist map[string]string, err error) {
	url := "users"
	if len(filters) > 0 {
		url += "?" + strings.Join(filters, "&")
	}
	err = e.client.magicRequestDecoder("GET", url, nil, &userlist)
	return
}

// VerboseList lists the users in the Chef server in verbose format.
// /users GET
// Chef API docs: https://docs.chef.io/api_chef_server.html#users
func (e *UserService) VerboseList(filters ...string) (userlist map[string]UserVerboseResult, err error) {
	url := "users"
	filters = append(filters, "verbose=true")
	if len(filters) > 0 {
		url += "?" + strings.Join(filters, "&")
	}
	err = e.client.magicRequestDecoder("GET", url, nil, &userlist)
	return
}

// Create Creates a User on the chef server
// /users POST
//  201 =  success
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
// 403 - not authorized
// 404 - user doesn't exist
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#users-name
func (e *UserService) Get(name string) (user User, err error) {
	url := fmt.Sprintf("users/%s", name)
	err = e.client.magicRequestDecoder("GET", url, nil, &user)
	return
}

// Update updates a user on the Chef server.
// /users/USERNAME PUT
// 200 - updated
// 401 - not authenticated
// 403 - not authorized
// 404 - user doesn't exist
// 409 - new user name is already in use
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#users-name
func (e *UserService) Update(name string, user User) (userUpdate UserResult, err error) {
	url := fmt.Sprintf("users/%s", name)
	body, err := JSONReader(user)
	err = e.client.magicRequestDecoder("PUT", url, body, &userUpdate)
	return
}

// ListUserKeys gets all the keys for a user.
// /users/USERNAME/keys GET
// 200 - successful
// 401 - not authenticated
// 403 - not authorized
// 404 - user doesn't exist
//
// Chef API docs: https://docs.chef.io/api_chef_server/#usersuserkeys
func (e *UserService) ListUserKeys(name string) (userkeys []UserKeyItem, err error) {
	url := fmt.Sprintf("users/%s/keys", name)
	err = e.client.magicRequestDecoder("GET", url, nil, &userkeys)
	return
}

// AddUserKey add a key for a user on the Chef server.
// /users/USERNAME/keys POST
// 201 - created
// 401 - not authenticated
// 403 - not authorized
// 404 - user doesn't exist
// 409 - new name is already in use
//
// Chef API docs: https://docs.chef.io/api_chef_server.html#users-name
func (e *UserService) AddUserKey(name string, keyadd UserKey) (userkey UserKeyItem, err error) {
	url := fmt.Sprintf("users/%s/keys", name)
	body, err := JSONReader(keyadd)
	err = e.client.magicRequestDecoder("POST", url, body, &userkey)
	return
}

// DeleteUserKey delete a key for a user.
// /users/USERNAME/keys/KEYNAME DELETE
// 200 - successful
// 401 - not authenticated
// 403 - not authorized
// 404 - user doesn't exist
//
// Chef API docs: https://docs.chef.io/api_chef_server/#usersuserkeys
func (e *UserService) DeleteUserKey(username string, keyname string) (userkey UserKey, err error) {
	url := fmt.Sprintf("users/%s/keys/%s", username, keyname)
	err = e.client.magicRequestDecoder("DELETE", url, nil, &userkey)
	return
}

// GetUserKey gets a key for a user.
// /users/USERNAME/keys/KEYNAME GET
// 200 - successful
// 401 - not authenticated
// 403 - not authorized
// 404 - user doesn't exist
//
// Chef API docs: https://docs.chef.io/api_chef_server/#usersuserkeys
func (e *UserService) GetUserKey(username string, keyname string) (userkey UserKey, err error) {
	url := fmt.Sprintf("users/%s/keys/%s", username, keyname)
	err = e.client.magicRequestDecoder("GET", url, nil, &userkey)
	return
}

// UpdateUserKey updates a key for a user.
// /users/USERNAME/keys/KEYNAME PUT
// 200 - successful
// 401 - not authenticated
// 403 - not authorized
// 404 - user doesn't exist
//
// Chef API docs: https://docs.chef.io/api_chef_server/#usersuserkeys
func (e *UserService) UpdateUserKey(username string, keyname string, keyUp UserKey) (userkey UserKey, err error) {
	url := fmt.Sprintf("users/%s/keys/%s", username, keyname)
	body, err := JSONReader(keyUp)
	err = e.client.magicRequestDecoder("PUT", url, body, &userkey)
	return
}
