package config

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/chef/automate/components/local-user-service/server"
	"github.com/chef/automate/lib/tls/certs"
)

// Config is the config format for the main application.
type Config struct {
	GRPC            string `json:"grpc"` // GRPC endpoint hosting all GRPC services
	A1UserData      string `json:"a1_user_data"`
	A1UserRolesData string `json:"a1_user_roles_data"`
	Logger          Logger `json:"logger"`
	Users           Users  `json:"users"` //!\\ only one users adapter
	AuthzAddress    string `json:"authz_address"`
	certs.TLSConfig `json:"tls"`
}

// Logger holds configuration required to customize logging
type Logger struct {
	// Level sets logging level severity.
	Level string `json:"level"`

	// Format specifies the format to be used for logging.
	Format string `json:"format"`
}

// Users is the type for "magically" unmarshalling users-adapter configs
type Users struct {
	Type string `json:"type"`

	Config server.UsersConfig `json:"config"`
}

// UnmarshalJSON allows Users to implement the unmarshaler interface to dynamically
// determine the type of the users config.
func (u *Users) UnmarshalJSON(b []byte) error {
	var users struct {
		Type string `json:"type"`

		Config json.RawMessage `json:"config"`
	}
	if err := json.Unmarshal(b, &users); err != nil {
		return fmt.Errorf("parse users adapter: %v", err)
	}
	f, ok := server.UsersConfigs[users.Type]
	if !ok {
		return fmt.Errorf("unknown users adapter type %q", users.Type)
	}

	usersConfig := f()
	if len(users.Config) != 0 {
		data := []byte(os.ExpandEnv(string(users.Config)))
		if err := json.Unmarshal(data, usersConfig); err != nil {
			return fmt.Errorf("parse users adapter config: %v", err)
		}
	}
	*u = Users{
		Type:   users.Type,
		Config: usersConfig,
	}
	return nil
}
