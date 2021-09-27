package main

import (
	"bytes"
	"errors"
	"io"
	"os"
	"os/exec"
	"path/filepath"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	mc "github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
	"github.com/chef/automate/lib/version"
)

func executeAutomateClusterCtlCommand(command string, args []string, helpDocs string) error {
	if len(command) < 1 {
		return errors.New("Invalid or empty command")
	}
	writer.Printf("%s command execution started \n\n\n", command)
	args = append([]string{command}, args...)
	c := exec.Command("automate-cluster-ctl", args...)
	c.Dir = "/hab/a2_deploy_workspace"
	c.Stdin = os.Stdin
	var out bytes.Buffer
	var stderr bytes.Buffer
	c.Stdout = io.MultiWriter(os.Stdout, &out)
	c.Stderr = io.MultiWriter(os.Stderr, &stderr)
	err := c.Run()
	if err != nil {
		writer.Printf(stderr.String())
		return status.Wrap(err, status.CommandExecutionError, helpDocs)
	} else {
		writer.Printf("No error in executing commands")
	}
	outStr, errStr := string(out.Bytes()), string(stderr.Bytes())
	if len(outStr) > 0 {
		writer.Printf("\nout:\n%s", outStr)
	}
	if len(errStr) > 0 {
		writer.Printf("\nerr:\n%s\n", errStr)
	}
	writer.Printf("%s command execution done, exiting\n", command)
	return err
}

func executeAutomateClusterCtlCommandAsync(command string, args []string, helpDocs string) error {
	if len(command) < 1 {
		return errors.New("Invalid or empty command")
	}
	if _, err := os.Stat(AUTOMATE_HA_RUN_LOG_DIR); os.IsNotExist(err) {
		err = os.Mkdir(AUTOMATE_HA_RUN_LOG_DIR, os.ModeDir)
		if err != nil {
			panic(err)
		}
	}
	writer.Printf("%s command execution started \n\n\n", command)
	args = append([]string{command}, args...)
	c := exec.Command("automate-cluster-ctl", args...)
	c.Dir = AUTOMATE_HA_WORKSPACE_DIR
	c.Stdin = os.Stdin
	var out bytes.Buffer
	var stderr bytes.Buffer
	outfile, err := os.Create(filepath.Join(AUTOMATE_HA_RUN_LOG_DIR, "/a2ha-run.log"))
	if err != nil {
		panic(err)
	}
	defer outfile.Close()
	c.Stdout = outfile
	c.Stderr = outfile
	err = c.Start()
	if err != nil {
		writer.Printf(stderr.String())
		return status.Wrap(err, status.CommandExecutionError, helpDocs)
	}
	outStr, errStr := string(out.Bytes()), string(stderr.Bytes())
	if len(outStr) > 0 {
		writer.Printf("\nout:\n%s", outStr)
	}
	if len(errStr) > 0 {
		writer.Printf("\nerr:\n%s\n", errStr)
	}
	writer.Printf("%s command execution done, exiting\n", command)
	return err
}

func bootstrapEnv(args []string) error {
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
	offlineMode := deployCmdFlags.airgap != ""
	err := client.DeployHA(writer, conf, manifestProvider, version.BuildTime, offlineMode)
	if err != nil && !status.IsStatusError(err) {
		return status.Annotate(err, status.DeployError)
	}
	err = readConfigAndWriteToFile(args[0])
	if err != nil {
		return status.Annotate(err, status.DeployError)
	}
	return nil
}

func isA2HARBFileExist() bool {
	if checkIfFileExist(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "a2ha.rb")) {
		return true
	}
	return false
}

func checkIfFileExist(path string) bool {
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return false
	}
	return true
}
