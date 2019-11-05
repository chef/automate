package pgw

import (
	"fmt"
	"strings"

	"github.com/lib/pq"
)

// AlterRoleQuery represents the available options for an ALTER ROLE query
type AlterRoleQuery struct {
	Role            string                 `json:"role"`
	Superuser       bool                   `json:"superuser"`
	NoSuperuser     bool                   `json:"nosuperuser"`
	CreateDB        bool                   `json:"createdb"`
	NoCreateDB      bool                   `json:"nocreatedb"`
	CreateRole      bool                   `json:"createrole"`
	NoCreateRole    bool                   `json:"nocreaterole"`
	CreateUser      bool                   `json:"createuser"`
	NoCreateUser    bool                   `json:"nocreateuser"`
	Inherit         bool                   `json:"inherit"`
	NoInherit       bool                   `json:"noinherit"`
	Login           bool                   `json:"login"`
	NoLogin         bool                   `json:"nologin"`
	Replication     bool                   `json:"replication"`
	NoReplication   bool                   `json:"noreplication"`
	ConnectionLimit int                    `json:"connection_limit"`
	Password        AlterRoleQueryPassword `json:"password"`
}

// AlterRoleQueryPassword represents an ALTER ROLE PASSWORD option
type AlterRoleQueryPassword struct {
	Value       string `json:"value"`
	Unencrypted bool   `json:"unencrypted"`
}

// NewAlterRoleQuery returns a pointer to a new AlterRoleQuery
func NewAlterRoleQuery() *AlterRoleQuery {
	return &AlterRoleQuery{
		Password: AlterRoleQueryPassword{},
	}
}

// String returns the options as query
func (o *AlterRoleQuery) String() string {
	options := []string{}

	if o.Superuser {
		options = append(options, "SUPERUSER")
	}
	if o.NoSuperuser {
		options = append(options, "NOSUPERUSER")
	}
	if o.CreateDB {
		options = append(options, "CREATEDB")
	}
	if o.NoCreateDB {
		options = append(options, "NOCREATEDB")
	}
	if o.CreateRole {
		options = append(options, "CREATEROLE")
	}
	if o.NoCreateRole {
		options = append(options, "NOCREATEROLE")
	}
	if o.CreateUser {
		options = append(options, "CREATEUSER")
	}
	if o.NoCreateUser {
		options = append(options, "NOCREATEUSER")
	}
	if o.Inherit {
		options = append(options, "INHERIT")
	}
	if o.NoInherit {
		options = append(options, "NOINHERIT")
	}
	if o.Login {
		options = append(options, "LOGIN")
	}
	if o.NoLogin {
		options = append(options, "NOLOGIN")
	}
	if o.Replication {
		options = append(options, "REPLICATION")
	}
	if o.NoReplication {
		options = append(options, "NOREPLICATION")
	}
	if o.ConnectionLimit != 0 {
		options = append(options, fmt.Sprintf("CONNECTION LIMIT %d", o.ConnectionLimit))
	}
	if o.Password.Value != "" {
		if o.Password.Unencrypted {
			options = append(options, "UNENCRYPTED")
		}
		options = append(options, fmt.Sprintf("PASSWORD %s", pq.QuoteLiteral(o.Password.Value)))
	}

	if len(options) == 0 {
		return ""
	}

	return fmt.Sprintf("ALTER ROLE %s WITH %s", pq.QuoteIdentifier(o.Role), strings.Join(options, " "))
}
