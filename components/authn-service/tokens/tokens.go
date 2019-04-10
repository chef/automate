package token

import (
	"github.com/chef/automate/components/authn-service/tokens/mock"
	"github.com/chef/automate/components/authn-service/tokens/pg"
	"github.com/chef/automate/components/authn-service/tokens/types"
)

// TokenConfigs variable provides an easy way to return a config struct depending
// on the storage adapter type.
var TokenConfigs = map[string]func() types.TokenConfig{
	"mock":       func() types.TokenConfig { return new(mock.Config) },
	"postgresql": func() types.TokenConfig { return new(pg.Config) },
}
