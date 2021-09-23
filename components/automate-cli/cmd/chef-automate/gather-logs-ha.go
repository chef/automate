package main

func executeHAGatherLogsA2HA(args []string) error {
	/*writer.Printf("gathering A2HA logs \n")
	args = append([]string{"gather-logs"}, args...)
	c := exec.Command("automate-cluster-ctl", args...)
	c.Dir = "/hab/a2_deploy_workspace"
	c.Stdin = os.Stdin
	var out bytes.Buffer
	var stderr bytes.Buffer
	c.Stdout = &out
	c.Stderr = &stderr
	err := c.Run()
	if err != nil {
		writer.Printf(stderr.String())
		return status.Wrap(err, status.CommandExecutionError, "please refer \n"+gatherLogsHelpDoc)
	}
	writer.Print(out.String())
	return err */
	return nil
}





git add components/automate-cli/cmd/chef-automate/AutomateClusterCtlUtils.go
git add components/automate-cli/cmd/chef-automate/automateConfigUtils.go
git add components/automate-cli/cmd/chef-automate/automateConstants.go
git add components/automate-cli/cmd/chef-automate/automateDeployExecutor.go
git add components/automate-cli/cmd/chef-automate/automateDeployHelpDocs.go
git add components/automate-cli/cmd/chef-automate/automateDeploymentFactory.go
git add components/automate-cli/cmd/chef-automate/automateHAAwsDeployment.go
git add components/automate-cli/cmd/chef-automate/automateHADeployExistingInfra.go
git add components/automate-cli/cmd/chef-automate/automateHAModeWithoutConfigFile.go
git add components/automate-cli/cmd/chef-automate/deployer.go
git add components/automate-cli/cmd/chef-automate/gather-logs-ha-template.go
git add components/automate-cli/cmd/chef-automate/gather-logs-ha.go
git add components/automate-cli/cmd/chef-automate/provisionInfraAws.go
git add components/automate-cli/cmd/chef-automate/provisionInfraExecutor.go
git add components/automate-cli/cmd/chef-automate/provisionInfraFactory.go
git add components/automate-cli/cmd/chef-automate/provisionWithoutConfigFile.go
git add components/automate-cli/cmd/chef-automate/secrets-ha-template.go
git add components/automate-cli/cmd/chef-automate/secrets-ha.go
git add components/automate-cli/cmd/chef-automate/status-ha.go
git add components/automate-cli/cmd/chef-automate/statusHaHelpDoc.go
git add components/automate-cli/cmd/chef-automate/testHA.go
git add components/automate-cli/cmd/chef-automate/workspace-ha-template.go
git add components/automate-cli/cmd/chef-automate/workspace-ha.go

