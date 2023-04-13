package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
	"time"

	dc "github.com/chef/automate/api/config/deployment"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

var automateHATerraformOutputFile = "/hab/a2_deploy_workspace/terraform/terraform.tfstate"
var automateHATerraformDestroyOutputFile = "/hab/a2_deploy_workspace/terraform/destroy/aws/terraform.tfstate"

var sshFlags = struct {
	hostname string
}{}

type AutomateHAInfraDetails struct {
	Version          int    `json:"version"`
	TerraformVersion string `json:"terraform_version"`
	Serial           int    `json:"serial"`
	Lineage          string `json:"lineage"`
	Outputs          struct {
		AutomateAdminPassword struct {
			Value string `json:"value"`
			Type  string `json:"type"`
		} `json:"automate_admin_password"`
		AutomateAdminUser struct {
			Value string `json:"value"`
			Type  string `json:"type"`
		} `json:"automate_admin_user"`
		AutomateDataCollectorToken struct {
			Value string `json:"value"`
			Type  string `json:"type"`
		} `json:"automate_data_collector_token"`
		AutomateFrontendUrls struct {
			Value string `json:"value"`
			Type  string `json:"type"`
		} `json:"automate_frontend_urls"`
		AutomatePrivateIps struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"automate_private_ips"`
		AutomatePublicIps struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"automate_public_ips"`
		AutomateSSH struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"automate_ssh"`
		AutomateURL struct {
			Value string `json:"value"`
			Type  string `json:"type"`
		} `json:"automate_url"`
		ChefServerPrivateIps struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"chef_server_private_ips"`
		ChefServerPublicIps struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"chef_server_public_ips"`
		ChefServerSSH struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"chef_server_ssh"`
		OpensearchPrivateIps struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"opensearch_private_ips"`
		OpensearchPublicIps struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"opensearch_public_ips"`
		OpensearchSSH struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"opensearch_ssh"`
		OpsDashboardAddresses struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"ops_dashboard_addresses"`
		PostgresqlPrivateIps struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"postgresql_private_ips"`
		PostgresqlPublicIps struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"postgresql_public_ips"`
		PostgresqlSSH struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"postgresql_ssh"`
		SSHKeyFile struct {
			Value string `json:"value"`
			Type  string `json:"type"`
		} `json:"ssh_key_file"`
		SSHUser struct {
			Value string `json:"value"`
			Type  string `json:"type"`
		} `json:"ssh_user"`
		SSHPort struct {
			Value string `json:"value"`
			Type  string `json:"type"`
		} `json:"ssh_port"`
		BackupConfigEFS struct {
			Value string `json:"value"`
			Type  string `json:"type"`
		} `json:"backup_config_efs"`
		BackupConfigS3 struct {
			Value string `json:"value"`
			Type  string `json:"type"`
		} `json:"backup_config_s3"`
	} `json:"outputs"`
	Resources []struct {
		Module    string `json:"module,omitempty"`
		Mode      string `json:"mode"`
		Type      string `json:"type"`
		Name      string `json:"name"`
		Provider  string `json:"provider"`
		Instances []struct {
			SchemaVersion int `json:"schema_version"`
			Attributes    struct {
				Architecture        string `json:"architecture"`
				Arn                 string `json:"arn"`
				BlockDeviceMappings []struct {
					DeviceName string `json:"device_name"`
					Ebs        struct {
						DeleteOnTermination string `json:"delete_on_termination"`
						Encrypted           string `json:"encrypted"`
						Iops                string `json:"iops"`
						SnapshotID          string `json:"snapshot_id"`
						Throughput          string `json:"throughput"`
						VolumeSize          string `json:"volume_size"`
						VolumeType          string `json:"volume_type"`
					} `json:"ebs"`
					NoDevice    string `json:"no_device"`
					VirtualName string `json:"virtual_name"`
				} `json:"block_device_mappings"`
				CreationDate    time.Time   `json:"creation_date"`
				Description     string      `json:"description"`
				EnaSupport      bool        `json:"ena_support"`
				ExecutableUsers interface{} `json:"executable_users"`
				Filter          []struct {
					Name   string   `json:"name"`
					Values []string `json:"values"`
				} `json:"filter"`
				Hypervisor      string        `json:"hypervisor"`
				ID              string        `json:"id"`
				ImageID         string        `json:"image_id"`
				ImageLocation   string        `json:"image_location"`
				ImageOwnerAlias interface{}   `json:"image_owner_alias"`
				ImageType       string        `json:"image_type"`
				KernelID        interface{}   `json:"kernel_id"`
				MostRecent      bool          `json:"most_recent"`
				Name            string        `json:"name"`
				NameRegex       interface{}   `json:"name_regex"`
				OwnerID         string        `json:"owner_id"`
				Owners          []string      `json:"owners"`
				Platform        interface{}   `json:"platform"`
				PlatformDetails string        `json:"platform_details"`
				ProductCodes    []interface{} `json:"product_codes"`
				Public          bool          `json:"public"`
				RamdiskID       interface{}   `json:"ramdisk_id"`
				RootDeviceName  string        `json:"root_device_name"`
				RootDeviceType  string        `json:"root_device_type"`
				RootSnapshotID  string        `json:"root_snapshot_id"`
				SriovNetSupport string        `json:"sriov_net_support"`
				State           string        `json:"state"`
				StateReason     struct {
					Code    string `json:"code"`
					Message string `json:"message"`
				} `json:"state_reason"`
				Tags struct {
				} `json:"tags"`
				UsageOperation     string `json:"usage_operation"`
				VirtualizationType string `json:"virtualization_type"`
			} `json:"attributes"`
		} `json:"instances"`
		Each string `json:"each,omitempty"`
	} `json:"resources"`
}

func init() {
	sshCommand.PersistentFlags().StringVarP(
		&sshFlags.hostname,
		"hostname",
		"H",
		"",
		"Automate ha server name to ssh")
	RootCmd.AddCommand(sshCommand)
}

var sshCommand = &cobra.Command{
	Use:   "ssh",
	Short: "SSH into Automate HA servers",
	Long:  "SSH into Automate HA servers",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
		docs.Compatibility:       docs.CompatiblewithHA,
	},
	RunE: runSshCommand,
}

func runSshCommand(_ *cobra.Command, _ []string) error {
	if !isA2HARBFileExist() {
		return errors.New(AUTOMATE_HA_INVALID_BASTION)
	}

	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return err
	}
	sshStrings, err := getIPOfRequestedServers(sshFlags.hostname, infra)
	if err != nil {
		return err
	}
	idx, err := writer.Prompt(strings.Join(sshStrings, "\n") + "\n press  1  to " + strconv.Itoa(len(sshStrings)))
	if err != nil {
		return err
	}
	i, err := strconv.Atoi(idx)
	if err != nil {
		return err
	}
	if i < 1 || i > len(sshStrings) {
		return errors.New("invalid input it should be between 1  to " + strconv.Itoa(len(sshStrings)))
	}
	var sshString string = sshStrings[i-1]
	sshTokens := strings.Split(sshString, " ")
	return executeShellCommand(sshTokens[0], sshTokens[1:], "")
}

func getAutomateHAInfraDetails() (*AutomateHAInfraDetails, error) {

	infraConfigFile, err := FileContainingAutomateHAInfraDetails()
	if err != nil {
		return nil, err
	}

	return getAutomateHAInfraDetailHelper(infraConfigFile)
}

func getAutomateHAInfraDetailHelper(infraConfigFilePath string) (*AutomateHAInfraDetails, error) {
	automateHAInfraDetails := &AutomateHAInfraDetails{}

	file, err := os.Open(infraConfigFilePath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	buf := new(bytes.Buffer)
	_, err = buf.ReadFrom(file)
	if err != nil {
		return nil, err
	}

	content := buf.String()
	if content == "" {
		return nil, errors.New("the file is empty")
	}

	err = json.Unmarshal([]byte(content), automateHAInfraDetails)
	if err != nil {
		return nil, err
	}

	extractPortAndSshUserFromAutomateSSHCommand(automateHAInfraDetails)
	return automateHAInfraDetails, nil
}

func FileContainingAutomateHAInfraDetails() (string, error) {
	if _, err := os.Stat(automateHATerraformOutputFile); errors.Is(err, nil) {
		return automateHATerraformOutputFile, nil
	}

	if _, err := os.Stat(automateHATerraformDestroyOutputFile); errors.Is(err, nil) {
		return automateHATerraformDestroyOutputFile, nil
	}

	return "", errors.New("Automate Ha infra confile file not exist")
}

func extractPortAndSshUserFromAutomateSSHCommand(automateHAInfraDetails *AutomateHAInfraDetails) {
	if automateHAInfraDetails == nil || len(automateHAInfraDetails.Outputs.AutomateSSH.Value) == 0 {
		return
	}

	automateSSH := automateHAInfraDetails.Outputs.AutomateSSH.Value[0]
	lastSpaceIndex := strings.LastIndex(automateSSH, " ")

	if len(strings.TrimSpace(automateHAInfraDetails.Outputs.SSHPort.Value)) == 0 {
		port := getSSHPortFromAutomateSSH(automateSSH, lastSpaceIndex)
		automateHAInfraDetails.Outputs.SSHPort.Value = port
	}

	if len(strings.TrimSpace(automateHAInfraDetails.Outputs.SSHUser.Value)) == 0 {
		sshUser := strings.TrimSpace(automateSSH[lastSpaceIndex:strings.LastIndex(automateSSH, "@")])
		automateHAInfraDetails.Outputs.SSHUser.Value = sshUser
	}

}

func getSSHPortFromAutomateSSH(automateSSH string, lastSpaceIndex int) string {
	port := "22"
	if strings.Contains(automateSSH, "-p") {
		port = strings.TrimSpace(automateSSH[strings.LastIndex(automateSSH, "-p")+2 : lastSpaceIndex])
	}
	return port
}

func getIPOfRequestedServers(servername string, d *AutomateHAInfraDetails) ([]string, error) {
	isManagedServices := isManagedServicesOn()
	switch strings.ToLower(servername) {
	case "automate", "a2":
		return d.Outputs.AutomateSSH.Value, nil
	case "chef_server", "cs":
		return d.Outputs.ChefServerSSH.Value, nil
	case "postgresql", "pg":
		if isManagedServices {
			return d.Outputs.PostgresqlSSH.Value, status.Annotate(errors.New("can not ssh managed service"), status.InvalidCommandArgsError)
		}
		return d.Outputs.PostgresqlSSH.Value, nil
	case "opensearch", "os":
		if isManagedServices {
			return d.Outputs.OpensearchSSH.Value, errors.New("can not ssh managed service")
		}
		return d.Outputs.OpensearchSSH.Value, nil
	default:
		return nil, errors.New("invalid hostname possible values should be any one of automate/a2, chef_server/cs, postgresql/pg or opensearch/os")
	}
}

func getPostgresOrOpenSearchExistingLogConfig(remoteType string) (*dc.AutomateConfig, error) {
	var log dc.AutomateConfig
	var fileName string
	if remoteType == "postgresql" {
		fileName = postgresLogConfig
	} else {
		fileName = opensearchConfig
	}
	if checkIfFileExist(fileName) {
		contents, err := ioutil.ReadFile(fileName) // nosemgrep
		if err != nil {
			return nil, err
		}
		destString := string(contents)
		log1, err := decodeLogConfig(destString)
		log = *log1
		if err != nil {
			return &log, err
		}
	}
	return &log, nil

}
