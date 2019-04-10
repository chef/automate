package server

import (
	"go.uber.org/zap"

	"github.com/chef/automate/components/local-user-service/users"
	"github.com/chef/automate/components/local-user-service/users/dex"
	"github.com/chef/automate/components/local-user-service/users/mock"
	"github.com/chef/automate/lib/tls/certs"
)

// UsersConfig is a configuration that can open a users adapter
type UsersConfig interface {
	Open(*zap.Logger, *certs.ServiceCerts) (users.Adapter, error)
}

// UsersConfigs variable provides an easy way to return a config struct depending
// on the users adapter type.
var UsersConfigs = map[string]func() UsersConfig{
	"mock": func() UsersConfig { return new(mock.Config) },
	"dex":  func() UsersConfig { return new(dex.Config) },
}
