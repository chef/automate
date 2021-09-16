package main

import (
	"bytes"
	"errors"
	"io"
	"os"
	"os/exec"

	"github.com/spf13/cobra"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	mc "github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
)

func newProvisionInfraCmd() *cobra.Command {
	var provisionInfraCmd = &cobra.Command{
		Use:   "provision-infra",
		Short: "Provison automate HA infra.",
		Long:  "Provison automate HA infra for automate HA deployment.",
		Args:  cobra.RangeArgs(0, 1),
		RunE:  runProvisionInfraCmd,
	}

	return provisionInfraCmd
}

func runProvisionInfraCmd(cmd *cobra.Command, args []string) error {
	if isA2ha, mode := isA2HADeployment(args); isA2ha && mode == AWS_MODE || mode == HA_MODE {
		err := bootstrapEnv(args, mode)
		if err != nil {
			return err
		}
		writer.Printf("prvisioning infra for automate HA \n\n\n\n")
		args = args[1:]
		args = append([]string{"provision"}, args...)
		c := exec.Command("automate-cluster-ctl", args...)
		c.Dir = "/hab/a2_deploy_workspace"
		c.Stdin = os.Stdin
		var out bytes.Buffer
		var stderr bytes.Buffer
		c.Stdout = io.MultiWriter(os.Stdout, &out)
		c.Stderr = io.MultiWriter(os.Stderr, &stderr)
		err = c.Run()
		if err != nil {
			writer.Printf(stderr.String())
			return status.Wrap(err, status.CommandExecutionError, provisionInfraHelpDocs)
		}
		outStr, errStr := string(out.Bytes()), string(stderr.Bytes())
		writer.Printf("\nout:\n%s\nerr:\n%s\n", outStr, errStr)
		return err
	} else {
		return status.Wrap(errors.New("provision-infra only works for HA mode of automate"), status.ConfigError, provisionInfraHelpDocs)
	}

}

func bootstrapEnv(args []string, mode string) error {
	writer.Printf(mode)
	if len(args) == 0 || mode == HA_MODE {
		return status.Annotate(errors.New("config.toml file path expected as argument."), status.DeployError)
	}
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
	return nil
}

func init() {
	RootCmd.AddCommand(newProvisionInfraCmd())
}
