package main

import (
	"bufio"
	"errors"
	"os"
	"os/exec"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	mc "github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
)

func executeAutomateClusterCtlCommand(command string, args []string, helpDocs string) error {
	if len(command) < 1 {
		return errors.New("Invalid or empty command")
	}
	writer.Printf("%s command execution started \n\n\n", command)
	args = append([]string{command}, args...)
	c := exec.Command("automate-cluster-ctl", args...)
	c.Dir = "/hab/a2_deploy_workspace"
	//stdIn, _ := c.StdinPipe()
	stdOut, _ := c.StdoutPipe()
	stdErr, _ := c.StderrPipe()
	/* c.Stdin = io.MultiReader(os.Stdin)
	var out bytes.Buffer
	var stderr bytes.Buffer
	c.Stdout = io.MultiWriter(os.Stdout, &out)
	c.Stderr = io.MultiWriter(os.Stderr, &stderr)
	err := c.Run()*/
	c.Start()

	scanner := bufio.NewScanner(stdErr)
	scanner.Split(bufio.ScanWords)
	for scanner.Scan() {
		m := scanner.Text()
		writer.Println(m)
	}

	scanner = bufio.NewScanner(stdOut)
	scanner.Split(bufio.ScanWords)
	for scanner.Scan() {
		m := scanner.Text()
		writer.Println(m)
	}

	c.Wait()
	return nil

	/* if err != nil {
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
	return err */
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

func isA2HARBFileExist() bool {
	if checkIfFileExist(initConfigHabA2HAPathFlag.a2haDirPath + "a2ha.rb") {
		return true
	} else {
		return false
	}
}

func checkIfFileExist(path string) bool {
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return false
	} else {
		return true
	}
}
