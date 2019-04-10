package tokens

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"net/url"
	"os"

	"go.uber.org/zap"

	"github.com/chef/automate/components/authn-service/authenticator"
	token "github.com/chef/automate/components/authn-service/tokens"
	"github.com/chef/automate/components/authn-service/tokens/types"
	"github.com/chef/automate/lib/tls/certs"
)

// HeaderTokenConfig is used for configuring tokens-storage-based header-token
// authenticators
type HeaderTokenConfig struct {
	Headers []string      `json:"headers"`
	Storage StorageConfig `json:"storage"`
}

// StorageConfig is the type for unmarshalling token storage-adapter configs
type StorageConfig struct {
	Type string `json:"type"`

	Config types.TokenConfig `json:"config"`
}

// HeaderTokenAuthenticator is carrying the state of token-adapter-based
// header-token authenticators
type HeaderTokenAuthenticator struct {
	headers []string
	tokens  types.Storage
	logger  *zap.Logger
}

type apiClient struct {
	id string
}

func (a *apiClient) Subject() string {
	return "token:" + a.id
}

func (a *apiClient) Teams() []string {
	return nil
}

// NewHeaderTokenAuthenticator returns an authenticator checking requests based
// on the passed header, and the passed storage adapter
func NewHeaderTokenAuthenticator(headers []string, ts types.Storage, logger *zap.Logger) authenticator.Authenticator {
	return &HeaderTokenAuthenticator{
		headers: headers,
		tokens:  ts,
		logger:  logger,
	}
}

// Open returns an storage-adapter-based header token authenticator
func (c *HeaderTokenConfig) Open(u *url.URL, serviceCerts *certs.ServiceCerts,
	logger *zap.Logger) (authenticator.Authenticator, error) {

	ts, err := c.Storage.Config.Open(serviceCerts, logger)
	if err != nil {
		return nil, err
	}
	return NewHeaderTokenAuthenticator(c.Headers, ts, logger), nil
}

// Authenticate processes the passed request, checking if the configured
// header's FIRST value matches any of the the hard-coded tokens list.
func (a *HeaderTokenAuthenticator) Authenticate(r *http.Request) (authenticator.Requestor, error) {
	ctx, cancel := context.WithCancel(r.Context())
	defer cancel()

	for _, hdr := range a.headers {
		value := r.Header.Get(hdr)
		if value == "" {
			continue
		}
		id, err := a.tokens.GetTokenIDWithValue(ctx, value)
		if err != nil {
			return nil, err
		}
		requestor := apiClient{
			id: id,
		}
		return &requestor, nil
	}

	return nil, errors.New("header-token-authenticator: no token in request")
}

// UnmarshalJSON allows Token to implement the unmarshaler interface to dynamically
// determine the type of the storage adapter config.
func (c *StorageConfig) UnmarshalJSON(b []byte) error {
	var cs struct {
		Type string `json:"type"`

		Config json.RawMessage `json:"config"`
	}
	if err := json.Unmarshal(b, &cs); err != nil {
		return fmt.Errorf("parse storage adapter: %v", err)
	}
	f, ok := token.TokenConfigs[cs.Type]
	if !ok {
		return fmt.Errorf("unknown storage adapter type %q", cs.Type)
	}

	tokenConfig := f()
	if len(cs.Config) != 0 {
		data := []byte(os.ExpandEnv(string(cs.Config)))
		if err := json.Unmarshal(data, tokenConfig); err != nil {
			return fmt.Errorf("parse storage adapter config: %v", err)
		}
	}
	*c = StorageConfig{
		Type:   cs.Type,
		Config: tokenConfig,
	}
	return nil
}
