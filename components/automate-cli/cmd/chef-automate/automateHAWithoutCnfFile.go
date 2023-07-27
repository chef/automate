package main

import (
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/pkg/errors"
)

type haWithoutConfig struct{}

func newHaWithoutConfig() *haWithoutConfig {
	return &haWithoutConfig{}
}

func (h *haWithoutConfig) doDeployWork(args []string) error {
	return status.Annotate(errors.New("config.toml file path expected as argument."), status.DeployError)
}

func (h *haWithoutConfig) doProvisionJob(args []string) error {
	return status.Annotate(errors.New("config.toml file path expected as argument to run chef-automate provision-infra"), status.DeployError)
}

func (h *haWithoutConfig) generateConfig(state string) error {
	return nil
}
