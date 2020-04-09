package chef

type AuthenticateUserService struct {
	client *Client
}

// Authenticate represents the body of the /authenticate_user request
type Authenticate struct {
	UserName string `json:"username"`
	Password string `json:"password"`
}

// Authenticate performs an authentication attempt.
//
// https://docs.chef.io/api_chef_server.html#authenticate-user
func (e *AuthenticateUserService) Authenticate(authenticate_request Authenticate) (err error) {
	body, err := JSONReader(authenticate_request)
	err = e.client.magicRequestDecoder("POST", "authenticate_user", body, nil)
	return
}
