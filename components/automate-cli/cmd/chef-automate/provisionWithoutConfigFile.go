package main

import (
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/pkg/errors"
)

type provisonWithoutConfig struct{}

func newProvisionWithoutConfig() *provisonWithoutConfig {
	return &provisonWithoutConfig{}
}

func (h *provisonWithoutConfig) doProvisionJob(args []string) error {
	return status.Annotate(errors.New("config.toml file path expected as argument to run chef-atomate provision-infra"), status.DeployError)
}
