package genconfig

import (
	"github.com/chef/automate/lib/toml"
)

type OsHaConfig struct {
	Fqdn string `json:"fqdn,omitempty" toml:"fqdn,omitempty" mapstructure:"fqdn,omitempty"`
}

func OsHaConfigFactory() *OsHaConfig {
	return &OsHaConfig{}
}

func (c *OsHaConfig) Toml() (tomlBytes []byte, err error) {
	return toml.Marshal(c)
}

func (c *OsHaConfig) Prompts() (err error) {
	return
}
