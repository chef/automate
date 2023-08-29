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

	TERRAFORM_CMD   = "terraform"
	AWS_DESTROY_DIR = "/hab/a2_deploy_workspace/terraform/destroy/aws/"
	TERRAFORM_INIT  = "init"

	AWS_PROVISION = `
	for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done
	%s
	for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy  -state=/hab/a2_deploy_workspace/terraform/destroy/aws/terraform.tfstate -auto-approve;cd $i;done
	`

	DEPLOYMENT_CLEANUP = `hab pkg uninstall chef/automate-ha-deployment`
	DESTROY_S3_BUCKET  = `HAB_LICENSE=accept-no-persist hab pkg exec core/aws-cli aws s3 rm s3://%s --recursive; hab pkg exec core/aws-cli aws s3 rb s3://%s`
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
		[ ! -L /hab ] && [ -d /hab ] && echo "not a symbolic, but it is directory...so deleting /hab" && sudo rm -rf /hab;
		[ -L /hab ] && echo "/hab is the symbolic link, so deleting the content of symbolic link" && sudo rm -rf /hab/*
		sudo rm -rf /var/automate-ha;
		sudo rm -rf /var/tmp/*.aib*;
		`

	BACKENDCLEANUP_COMMANDS = `
		sudo systemctl stop hab-sup;
		[ ! -L /hab ] && [ -d /hab ] && echo "not a symbolic, but it is directory...so deleting /hab" && sudo rm -rf /hab;
		[ -L /hab ] && echo "/hab is the symbolic link, so deleting the content of symbolic link" && sudo rm -rf /hab/*
		[ -f /bin/hab ] && sudo rm -rf /bin/hab;
		sudo rm -rf /var/automate-ha;
		sudo rm -rf /var/tmp/*.aib*;
		`
)

func runCleanupCmd(cmd *cobra.Command, args []string) error {

	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return err
	}
	if infra != nil {
		if isA2HARBFileExist() {
			sshconfig := &SSHConfig{}
			sshconfig.sshUser = infra.Outputs.SSHUser.Value
			sshconfig.sshKeyFile = infra.Outputs.SSHKeyFile.Value
			sshconfig.sshPort = infra.Outputs.SSHPort.Value
			sshUtil := NewSSHUtil(sshconfig)
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
				err = executeCommandForArrayofIPs(sshUtil, automateIps, FRONTENDCLEANUP_COMMANDS, "automate")
				if err != nil {
					return err
				}
				chefserverIps := infra.Outputs.ChefServerPrivateIps.Value
				err = executeCommandForArrayofIPs(sshUtil, chefserverIps, FRONTENDCLEANUP_COMMANDS, "chef-server")
				if err != nil {
					return err
				}
				postgresqlIps := infra.Outputs.PostgresqlPrivateIps.Value
				err = executeCommandForArrayofIPs(sshUtil, postgresqlIps, BACKENDCLEANUP_COMMANDS, "postgresql")
				if err != nil {
					return err
				}
				opensearchIps := infra.Outputs.OpensearchPrivateIps.Value

				err = executeCommandForArrayofIPs(sshUtil, opensearchIps, BACKENDCLEANUP_COMMANDS, "opensearch")
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

				backup_config, err := getTheValueFromA2HARB("backup_config")
				if err != nil {
					writer.Error("Error in getting backup_config")
					return err
				}

				if backup_config == "s3" && cleanupFlags.force {
					bucket_name, err := getTheValueFromA2HARB("s3_bucketName")
					if err != nil {
						writer.Error("Error in getting bucket_name")
						return err
					}
					writer.Body("BucketName :" + bucket_name)
					appendString = appendString + fmt.Sprintf(DESTROY_S3_BUCKET, bucket_name, bucket_name)
				} else if infra.Outputs.BackupConfigEFS.Value == "true" && !cleanupFlags.force {
					appendString = appendString + `for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform state rm "module.efs[0].aws_efs_file_system.backups";cd $i;done`
				}
				writer.Println("Cleaning up all AWS provisioned resources.")

				// Init terraform folder
				err = executeCommand(TERRAFORM_CMD, []string{TERRAFORM_INIT}, AWS_DESTROY_DIR)
				if err != nil {
					writer.Error("Terraform init failed")
					return err
				}
				// Modify tf state or remove bucket based in "--force" arg
				writer.Printf("%v", len(appendString) > 0)
				if len(appendString) > 0 {
					err = executeCommand("/bin/sh", []string{"-c", appendString}, "")
					if err != nil {
						writer.Errorf(err.Error())
						return err
					}
				}

				// Destroy AWS resources provisioned by terraform
				err = executeCommand(TERRAFORM_CMD, []string{"destroy", "-auto-approve"}, AWS_DESTROY_DIR)
				if err != nil {
					writer.Errorf("error while destroying infra, %v", err)
					return err
				}
				writer.Success("cleanup completed successfully. Run the following command to remove/uninstall deployment workspace\n hab pkg uninstall chef/automate-ha-deployment\n")
			}
		}
	} else {
		writer.Println("\nCleanup not executed.")
	}
	return nil
}

func executeCommandForArrayofIPs(sshUtil SSHUtil, remoteIps []string, script string, servername string) error {
	for i := 0; i < len(remoteIps); i++ {
		sshUtil.getSSHConfig().hostIP = remoteIps[i]
		writer.Println("Cleanup has started on " + servername + " node : " + remoteIps[i] + "\n")
		_, err := sshUtil.connectAndExecuteCommandOnRemote(script, true)
		if err != nil {
			writer.Error(err.Error())
			return err
		} else {
			writer.Success("Cleanup is completed on " + servername + " node : " + remoteIps[i] + "\n")
		}
	}
	return nil
}
