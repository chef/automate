package dex

import (
	"context"
	"io/ioutil"
	"net/http"
	"net/url"
	"regexp"
	"strings"

	"github.com/dexidp/dex/api"
	"github.com/pkg/errors"
	"go.uber.org/zap"
	"golang.org/x/crypto/bcrypt"

	"github.com/chef/automate/components/local-user-service/users"
	"github.com/chef/automate/components/session-service/oidc"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
)

// Config is used for configuring dex adapters
type Config struct {
	GRPCHost string `json:"grpcHost"`

	// OIDC (public) client config
	IssuerURL string `json:"issuerURL"`
	DexURL    string `json:"dexURL"`
}

type state struct {
	c      api.DexClient
	oc     oidc.Client
	dexURL *url.URL
	l      *zap.Logger
}

var (
	localLoginRegexp = regexp.MustCompile(`(/dex/auth/local\?req=[^"]+)`)
	oobCodeRegexp    = regexp.MustCompile(`Please copy this code, switch to your application and paste it there:`)
)

// Open initializes the dex adapter
func (c *Config) Open(l *zap.Logger, cs *certs.ServiceCerts) (users.Adapter, error) {
	connFactory := secureconn.NewFactory(*cs)
	conn, err := connFactory.Dial("automate-dex", c.GRPCHost)
	if err != nil {
		return nil, err
	}
	//defer conn.Close()

	if c.IssuerURL == "" {
		l.Warn("no OIDC config, not initializing Dex password validation client")
		return &state{
				c: api.NewDexClient(conn),
				l: l},
			nil
	}

	issuer, err := url.Parse(c.IssuerURL)
	if err != nil {
		return nil, errors.Wrapf(err, "parse issuer URL %q", c.IssuerURL)
	}
	dex, err := url.Parse(c.DexURL)
	if err != nil {
		return nil, errors.Wrapf(err, "parse dex URL %q", c.DexURL)
	}
	// good enough for our purposes here, and what a public client needs to use
	redir, err := url.Parse("urn:ietf:wg:oauth:2.0:oob")
	if err != nil {
		return nil, errors.Wrap(err, "parse redirect URL")
	}

	cfg := oidc.Config{
		ClientID:     "automate-api", // public client registered in dex' config.yml
		ClientSecret: "",
		IssuerURL:    issuer,
		RedirectURL:  redir,
		DexURL:       dex,
	}

	o, err := oidc.New(cfg, 2, cs, l.Sugar())
	if err != nil {
		return nil, errors.Wrap(err, "open OIDC client")
	}

	return &state{
			c:      api.NewDexClient(conn),
			oc:     o,
			dexURL: issuer,
			l:      l},
		nil
}

func (s *state) GetUsers(ctx context.Context) (users.Users, error) {
	// GRPC call to return all users from dex
	resp, err := s.c.ListPasswords(ctx, &api.ListPasswordReq{})
	if err != nil {
		return nil, errors.Wrap(err, "get passwords")
	}

	// generates map of returned users
	usrs := make(map[string]users.ShowUser, len(resp.Passwords))
	for _, p := range resp.Passwords {
		usrs[p.Email] = users.ShowUser{
			ID:    p.UserId,
			Name:  p.Username,
			Email: p.Email,
		}
	}
	return usrs, nil
}

// NOTE: dex only has ListPasswords, no singular ListPassword
func (s *state) GetUser(ctx context.Context, email string) (*users.ShowUser, error) {
	// GRPC call to return all users from dex
	resp, err := s.c.ListPasswords(ctx, &api.ListPasswordReq{})
	if err != nil {
		return nil, errors.Wrap(err, "get password")
	}

	// searches returned users for user
	for _, p := range resp.Passwords {
		if p.Email == email {
			usr := users.ShowUser{
				ID:    p.UserId,
				Name:  p.Username,
				Email: p.Email,
			}
			return &usr, nil
		}
	}

	return nil, &users.NotFoundError{}
}

func (s *state) GetPassword(ctx context.Context, email string) (*users.UserWithHashedPass, error) {
	// GRPC call to return all users from dex
	resp, err := s.c.ListPasswords(ctx, &api.ListPasswordReq{})
	if err != nil {
		return nil, errors.Wrap(err, "get password")
	}

	// searches returned users for user
	for _, p := range resp.Passwords {
		if p.Email == email {
			editUser := users.UserWithHashedPass{
				ShowUser: users.ShowUser{ID: p.UserId,
					Name:  p.Username,
					Email: p.Email,
				},
				HashedPass: p.Hash,
			}
			return &editUser, nil
		}
	}

	return nil, &users.NotFoundError{}
}

func (s *state) CreateUser(ctx context.Context, user users.User) (*users.ShowUser, error) {
	// hashes user's password string
	hashedPass, err := s.HashPassword(user.Password)
	if err != nil {
		return nil, errors.Wrap(err, "create password")
	}

	// creates password request
	createReq := &api.CreatePasswordReq{
		Password: &api.Password{
			Email:    user.Email,
			UserId:   user.ID,
			Username: user.Name,
			Hash:     hashedPass,
		}}

	return s.createPassword(ctx, createReq)
}

func (s *state) CreateUserWithHashedPass(ctx context.Context, user users.UserWithHashedPass) (*users.ShowUser, error) {
	// creates password request
	createReq := &api.CreatePasswordReq{
		Password: &api.Password{
			Email:    user.Email,
			UserId:   user.ID,
			Username: user.Name,
			Hash:     user.HashedPass,
		}}

	return s.createPassword(ctx, createReq)
}

func (s *state) createPassword(ctx context.Context, req *api.CreatePasswordReq) (*users.ShowUser, error) {
	// GRPC call to create Password (User) in dex
	resp, err := s.c.CreatePassword(ctx, req)
	if err != nil {
		return nil, errors.Wrap(err, "create password")
	}
	if resp.AlreadyExists {
		return nil, &users.AlreadyExistsError{}
	}

	newUser := users.ShowUser{
		ID:    req.GetPassword().UserId,
		Email: req.GetPassword().Email,
		Name:  req.GetPassword().Username,
	}
	return &newUser, nil
}

// UpdateUser makes a GRPC call to update an existing user in dex
func (s *state) UpdateUser(ctx context.Context, user users.UserWithHashedPass) (*users.ShowUser, error) {
	// update request
	updateReq := &api.UpdatePasswordReq{
		Email:       user.Email,
		NewHash:     user.HashedPass,
		NewUsername: user.Name,
	}

	// GRPC call to update Password
	resp, err := s.c.UpdatePassword(ctx, updateReq)
	if err != nil {
		return nil, errors.Wrap(err, "update password")
	}
	if resp.NotFound {
		return nil, &users.NotFoundError{}
	}

	updatedUser := users.ShowUser{
		ID:    user.ID,
		Email: user.Email,
		Name:  user.Name,
	}
	return &updatedUser, nil
}

func (s *state) DeleteUser(ctx context.Context, email string) (bool, error) {
	// delete request
	deleteReq := &api.DeletePasswordReq{
		Email: email,
	}
	// GRPC call to delete a user in dex
	resp, err := s.c.DeletePassword(ctx, deleteReq)
	if err != nil {
		return false, errors.Wrap(err, "delete password")
	}
	if resp.NotFound {
		return false, &users.NotFoundError{}
	}

	return true, nil
}

func (s *state) HashPassword(password string) ([]byte, error) {
	if password == "" {
		return nil, errors.New("password cannot be nil")
	}

	cost := 10
	hashedPass, err := bcrypt.GenerateFromPassword([]byte(password), cost)
	if err != nil {
		return nil, errors.Wrap(err, "set password field")
	}

	return hashedPass, nil
}

// TODO, brenna darroch 9/8/17
// validate user function
// need guidance from Product/UX on what to validate

func (s *state) ValidatePassword(ctx context.Context, username, password string) (bool, error) {
	// (tc) We don't need to worry about checking the OIDC state in a callback for the public
	// clients we are using internally since all requests are happening within automate itself
	// (in this instance, it's local user service talking directly to dex, not even going through ALB).
	// Also, all of those requests are using HTTPS and mTLS. Therefore, there isn't a situation
	// where these could be sniffed or a CSRF attack[1] could occur without automate being completely pwned already.
	//
	// [1] https://tools.ietf.org/html/rfc6749#section-10.12
	oidcState := "thisShouldNotMatter"
	authURL := s.oc.AuthCodeURL(oidcState)
	req, err := http.NewRequest("GET", authURL, nil)
	if err != nil {
		return false, errors.Wrap(err, "create HTTP request")
	}
	req.Header.Add("content-type", "form-www-urlencoded")
	req = req.WithContext(ctx)
	resp, err := s.oc.Client().Do(req)
	if err != nil {
		return false, errors.Wrap(err, "execute HTTP request")
	}
	defer resp.Body.Close() // nolint: errcheck

	respBody, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return false, errors.Wrap(err, "read HTTP response body")
	}

	// Note: this works both for "only local connector" and "multiple connectors"
	// Dex settings (e.g. local + saml), since our RE is loose enough to capture
	// either the login form POST (single connector) or the link to the local
	// connector (multiple connectors)

	// find local login link with auth code
	if fs := localLoginRegexp.Find(respBody); fs != nil {
		vals := url.Values{}
		vals.Add("login", username)
		vals.Add("password", password)
		body := strings.NewReader(vals.Encode())
		loginURL, err := s.dexURL.Parse(string(fs))
		if err != nil {
			return false, errors.Wrap(err, "construct HTTP request path")
		}
		req, err := http.NewRequest("POST", loginURL.String(), body)
		if err != nil {
			return false, errors.Wrap(err, "create HTTP login request")
		}
		req.Header.Add("content-type", "application/x-www-form-urlencoded")
		req = req.WithContext(ctx)
		resp, err := s.oc.Client().Do(req)
		if err != nil {
			return false, errors.Wrap(err, "execute HTTP login request")
		}
		defer resp.Body.Close() // nolint: errcheck

		respBody, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			return false, errors.Wrap(err, "read HTTP login response body")
		}

		// Success means: we've been given a _code_ to redeem for a token.
		// We don't care for the token, we only care for having been able to get
		// one. So this stops here.
		if code := oobCodeRegexp.Find(respBody); code != nil {
			return true, nil
		}
	}

	return false, nil
}
