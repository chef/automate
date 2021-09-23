package main

import (
	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	mc "github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
)

type existingInfra struct{}

func newExistingInfa() *existingInfra {
	return &existingInfra{}
}

func (e *existingInfra) doDeployWork(args []string) error {
	if !deployCmdFlags.acceptMLSA {
		agree, err := writer.Confirm(promptMLSA)
		if err != nil {
			return status.Wrap(err, status.InvalidCommandArgsError, errMLSA)
		}

		if !agree {
			return status.New(status.InvalidCommandArgsError, errMLSA)
		}
	}
	conf := new(dc.AutomateConfig)
	manifestProvider := manifest.NewLocalHartManifestProvider(
		mc.NewDefaultClient(conf.Deployment.GetV1().GetSvc().GetManifestDirectory().GetValue()),
		conf.Deployment.GetV1().GetSvc().GetHartifactsPath().GetValue(),
		conf.Deployment.GetV1().GetSvc().GetOverrideOrigin().GetValue())
	err := client.DeployHA(writer, manifestProvider)
	if err != nil && !status.IsStatusError(err) {
		return status.Annotate(err, status.DeployError)
	}
	err = readConfigAndWriteToFile(args[0])
	if err != nil {
		return status.Annotate(err, status.DeployError)
	}
	err = deployA2HA(args)
	return err
}
