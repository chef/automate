package genconfig

import (
	"github.com/chef/automate/lib/toml"
)

type AutomateStandaloneConfig struct {
	Fqdn string `json:"fqdn,omitempty" toml:"fqdn,omitempty" mapstructure:"fqdn,omitempty"`
}

func AutomateStandaloneConfigFactory() *AutomateStandaloneConfig {
	return &AutomateStandaloneConfig{}
}

func (c *AutomateStandaloneConfig) Toml() (tomlBytes []byte, err error) {
	return toml.Marshal(c)
}

func (c *AutomateStandaloneConfig) Prompts() (err error) {
	return
}
