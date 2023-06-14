package config

import (
	pgc "github.com/chef/automate/components/automate-cli/pkg/pullandgenerateconfig"
)

func (c *HaDeployConfig) Generate() error {
	pgc.PullConfigs()
	return nil
}
