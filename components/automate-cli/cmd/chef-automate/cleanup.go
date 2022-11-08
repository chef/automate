package main

import (
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/spf13/cobra"
)

var (
	AWS        string = "aws"
	DEPLOYMENT string = "deployment"

	AWS_PROVISION = `
	for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done
	%s
	for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy  -state=/hab/a2_deploy_workspace/terraform/destroy/aws/terraform.tfstate -auto-approve;cd $i;done
`
	DEPLOYMENT_CLEANUP = `hab pkg uninstall chef/automate-ha-deployment`
)

var cleanupFlags = struct {
	onprem bool
	aws    bool
	force  bool
}{}

func init() {
	RootCmd.AddCommand(cleanupCmd)
	cleanupCmd.PersistentFlags().BoolVar(&cleanupFlags.onprem, "onprem-deployment", false, "Cleaning up all the instances related to onprem ")
	cleanupCmd.PersistentFlags().BoolVar(&cleanupFlags.aws, "aws-deployment", false, "Remove AWS resources created by provisioning and clean-up hab workspace")
	cleanupCmd.PersistentFlags().BoolVar(&cleanupFlags.force, "force", false, "Remove backup storage")

}

var cleanupCmd = &cobra.Command{
	Use:   "cleanup",
	Short: "cleanup the Automate HA instances",
	Long:  "cleaning up the instance of all the Automate HA related Applications.",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	RunE:   runCleanupCmd,
	Hidden: true,
}

const (
	FRONTENDCLEANUP_COMMANDS = `
		sudo systemctl stop chef-automate;
		sudo rm -rf /hab;
		sudo rm -rf /var/automate-ha;
		`

	BACKENDCLEANUP_COMMANDS = `
		sudo systemctl stop hab-sup;
		sudo rm -rf /hab; 
		sudo rm -rf /var/automate-ha;
		`
)

func runCleanupCmd(cmd *cobra.Command, args []string) error {
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return err
	}
	if infra != nil {
		if isA2HARBFileExist() {
			sshUser := infra.Outputs.SSHUser.Value
			sshKeyFile := infra.Outputs.SSHKeyFile.Value
			sshPort := infra.Outputs.SSHPort.Value
			if cleanupFlags.onprem {

				res, err := writer.Prompt("Cleanup will remove all the Automate HA resources created by deploy command. Do you want to continue?\nPress y to agree, n to disagree? [y/n]")
				if err != nil {
					return err
				}
				if strings.ToLower(res) == "n" {
					writer.Println("Cleanup execution cancelled.")
					return nil
				}
				automateIps := infra.Outputs.AutomatePrivateIps.Value
				err = executeCommandForArrayofIPs(sshUser, sshPort, sshKeyFile, automateIps, FRONTENDCLEANUP_COMMANDS, "automate")
				if err != nil {
					return err
				}
				chefserverIps := infra.Outputs.ChefServerPrivateIps.Value
				err = executeCommandForArrayofIPs(sshUser, sshPort, sshKeyFile, chefserverIps, FRONTENDCLEANUP_COMMANDS, "chef-server")
				if err != nil {
					return err
				}
				postgresqlIps := infra.Outputs.PostgresqlPrivateIps.Value
				err = executeCommandForArrayofIPs(sshUser, sshPort, sshKeyFile, postgresqlIps, BACKENDCLEANUP_COMMANDS, "postgresql")
				if err != nil {
					return err
				}
				opensearchIps := infra.Outputs.OpensearchPrivateIps.Value
				err = executeCommandForArrayofIPs(sshUser, sshPort, sshKeyFile, opensearchIps, BACKENDCLEANUP_COMMANDS, "opensearch")
				if err != nil {
					return err
				}

				// Cleanup HA Deployment workspace
				args := []string{
					"-c",
					DEPLOYMENT_CLEANUP,
				}
				err = executeCommand("/bin/sh", args, "")
				if err != nil {
					return err
				}
			}
			if cleanupFlags.aws {

				res, err := writer.Prompt("Cleanup will remove all the AWS resources created by provison-infra. Do you want to continue?\nPress y to agree, n to disagree? [y/n]")
				if err != nil {
					return err
				}
				if strings.ToLower(res) == "n" {
					writer.Println("Cleanup execution cancelled.")
					return nil
				}
				archBytes, err := ioutil.ReadFile("/hab/a2_deploy_workspace/terraform/.tf_arch") // nosemgrep
				if err != nil {
					writer.Errorf("%s", err.Error())
					return err
				}
				var arch = strings.Trim(string(archBytes), "\n")
				writer.Println("Reference architecture type :" + arch)

				appendString := ""
				if infra.Outputs.BackupConfigS3.Value == "true" && cleanupFlags.force {
					appendString = appendString + `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;cp -r ../../.tf_arch .;cp -r ../../../a2ha.rb ..;terraform apply -var="destroy_bucket=true" -auto-approve;cd $i;done`
				} else if infra.Outputs.BackupConfigEFS.Value == "true" && !cleanupFlags.force {
					appendString = appendString + `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform state rm "module.efs[0].aws_efs_file_system.backups";cd $i;done`
				}

				writer.Println("Cleaning up all AWS provisioned resources.")

				// 'cleanupScripts' contains array of scripts
				// [Script to delete AWS reesources, Script to uninstall deployment workspace]
				var cleanupScripts = []string{fmt.Sprintf(AWS_PROVISION, appendString), DEPLOYMENT_CLEANUP}

				// Iteration 1: Removes AWS resource
				// Iteration 2: Uninstall automate-ha-deployment
				for i := 0; i < len(cleanupScripts); i++ {
					args := []string{
						"-c",
						cleanupScripts[i],
					}
					err = executeCommand("/bin/sh", args, "")
					if err != nil {
						return err
					}
				}
				writer.Success("Cleaning up completed.")
			}
		}
	} else {
		writer.Println("\nCleanup not executed.")
	}
	return nil
}

func executeCommandForArrayofIPs(sshUser string, sshPort string, sshKeyFile string, remoteIps []string, script string, servername string) error {
	for i := 0; i < len(remoteIps); i++ {
		writer.Println("Cleanup has started on " + servername + " node : " + remoteIps[i] + "\n")
		_, err := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sshKeyFile, remoteIps[i], script)
		if err != nil {
			writer.Error(err.Error())
			return err
		} else {
			writer.Success("Cleanup is completed on " + servername + " node : " + remoteIps[i] + "\n")
		}
	}
	return nil
}
