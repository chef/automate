package main

import (
	"encoding/json"
	"fmt"
	"os"

	server "github.com/chef/automate/components/authn-service/server"
	token "github.com/chef/automate/components/authn-service/tokens"
	"github.com/chef/automate/components/authn-service/tokens/types"
	"github.com/chef/automate/lib/tls/certs"
)

// Config is the config format for the main application.
type Config struct {
	Authenticators           []Authenticator `json:"authenticators"`
	GRPC                     string          `json:"grpc"`  // GRPC endpoint hosting all GRPC services
	HTTP1                    string          `json:"http1"` // HTTP 1 endpoint for hosting all HTTP1 services
	Upstream                 string          `json:"upstream"`
	Logger                   Logger          `json:"logger"`
	Token                    Token           `json:"tokens"` //!\\ only one token adapter
	AuthzAddress             string          `json:"authz_address"`
	LegacyDataCollectorToken string          `json:"legacy_data_collector_token"`
	certs.TLSConfig          `json:"tls"`
}

// Logger holds configuration required to customize logging
type Logger struct {
	// Level sets logging level severity.
	Level string `json:"level"`

	// Format specifies the format to be used for logging.
	Format string `json:"format"`
}

// Authenticator is a magical type that can unmarshal YAML dynamically. The type
// field determines the authenticator type, which is then customized for Config.
type Authenticator struct {
	Type string `json:"type"`
	ID   string `json:"id"`

	Config server.AuthenticatorConfig `json:"config"`
}

// Token is the type for unmarshalling token-adapter configs
type Token struct {
	Type string `json:"type"`

	Config types.TokenConfig `json:"config"`
}

// UnmarshalJSON allows Authenticator to implement the unmarshaler interface to
// dynamically determine the type of the authenticator config.
// This works like this: this gets called when the JSON unmarshalling reaches
// the `authenticators` key for the `Config` struct. It will then take the
// value, allocate an `Authenticator` struct, and call this method on it,
// passing the value.
func (a *Authenticator) UnmarshalJSON(b []byte) error {
	var authn struct {
		Type string `json:"type"`
		ID   string `json:"id"`

		Config json.RawMessage `json:"config"`
	}
	if err := json.Unmarshal(b, &authn); err != nil {
		return fmt.Errorf("parse authenticator: %v", err)
	}
	f, ok := server.AuthenticatorsConfig[authn.Type]
	if !ok {
		return fmt.Errorf("unknown authenticator type %q", authn.Type)
	}

	authnConfig := f()
	if len(authn.Config) != 0 {
		data := []byte(os.ExpandEnv(string(authn.Config)))
		if err := json.Unmarshal(data, authnConfig); err != nil {
			return fmt.Errorf("parse authenticator config: %v", err)
		}
	}
	*a = Authenticator{
		Type:   authn.Type,
		ID:     authn.ID,
		Config: authnConfig,
	}
	return nil
}

// UnmarshalJSON allows Token to implement the unmarshaler interface to dynamically
// determine the type of the token adapter config.
func (t *Token) UnmarshalJSON(b []byte) error {
	var cs struct {
		Type string `json:"type"`

		Config json.RawMessage `json:"config"`
	}
	if err := json.Unmarshal(b, &cs); err != nil {
		return fmt.Errorf("parse token storage adapter: %v", err)
	}
	f, ok := token.TokenConfigs[cs.Type]
	if !ok {
		return fmt.Errorf("unknown token storage adapter type %q", cs.Type)
	}

	tokenConfig := f()
	if len(cs.Config) != 0 {
		data := []byte(os.ExpandEnv(string(cs.Config)))
		if err := json.Unmarshal(data, tokenConfig); err != nil {
			return fmt.Errorf("parse token adapter config: %v", err)
		}
	}
	*t = Token{
		Type:   cs.Type,
		Config: tokenConfig,
	}
	return nil
}
