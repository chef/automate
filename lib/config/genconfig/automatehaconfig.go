package genconfig

import (
	"github.com/chef/automate/lib/toml"
)

type AutomateHaConfig struct {
	Fqdn string `json:"fqdn,omitempty" toml:"fqdn,omitempty" mapstructure:"fqdn,omitempty"`
}

func AutomateHaConfigFactory() *AutomateHaConfig {
	return &AutomateHaConfig{}
}

func (c *AutomateHaConfig) Toml() (tomlBytes []byte, err error) {
	return toml.Marshal(c)
}

func (c *AutomateHaConfig) Prompts() (err error) {
	return
}
