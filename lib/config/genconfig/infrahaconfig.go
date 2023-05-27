package genconfig

import (
	"github.com/chef/automate/lib/toml"
)

type InfraHaConfig struct {
	Fqdn string `json:"fqdn,omitempty" toml:"fqdn,omitempty" mapstructure:"fqdn,omitempty"`
}

func InfraHaConfigFactory() *InfraHaConfig {
	return &InfraHaConfig{}
}

func (c *InfraHaConfig) Toml() (tomlBytes []byte, err error) {
	return toml.Marshal(c)
}

func (c *InfraHaConfig) Prompts() (err error) {
	return
}
