package genconfig

import (
	"github.com/chef/automate/lib/toml"
)

type PgHaConfig struct {
	Fqdn string `json:"fqdn,omitempty" toml:"fqdn,omitempty" mapstructure:"fqdn,omitempty"`
}

func PgHaConfigFactory() *PgHaConfig {
	return &PgHaConfig{}
}

func (c *PgHaConfig) Toml() (tomlBytes []byte, err error) {
	return toml.Marshal(c)
}

func (c *PgHaConfig) Prompts() (err error) {
	return
}
