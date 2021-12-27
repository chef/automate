package main

import (
	"encoding/json"
	"errors"
	"io/ioutil"
	"strconv"
	"strings"
	"time"

	"github.com/spf13/cobra"
)

const automateHATerraformOutputFile = "/hab/a2_deploy_workspace/terraform/terraform.tfstate"
const automateHATerraformDestroyOutputFile = "/hab/a2_deploy_workspace/terraform/destroy/aws/terraform.tfstate"

var sshFlags = struct {
	hostname string
}{}

type AutomteHAInfraDetails struct {
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
		ElasticsearchPrivateIps struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"elasticsearch_private_ips"`
		ElasticsearchPublicIps struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"elasticsearch_public_ips"`
		ElasticsearchSSH struct {
			Value []string `json:"value"`
			Type  []string `json:"type"`
		} `json:"elasticsearch_ssh"`
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
	sshCommand.PersistentFlags().StringVar(
		&sshFlags.hostname,
		"hostname",
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
	},
	RunE: runSshCommand,
}

func runSshCommand(cmd *cobra.Command, args []string) error {
	if !isA2HARBFileExist() {
		return errors.New(AUTOMATE_HA_INVALID_BASTION)
	}
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return err
	}
	sshStrings, err := getIPOfRequestedServers(sshFlags.hostname, infra)
	if err != nil {
		return nil
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

func getAutomateHAInfraDetails() (*AutomteHAInfraDetails, error) {
	if checkIfFileExist(automateHATerraformOutputFile) {
		automateHAInfraDetails := &AutomteHAInfraDetails{}
		contents, err := ioutil.ReadFile(automateHATerraformOutputFile)
		if err != nil {
			return nil, err
		}
		err = json.Unmarshal(contents, automateHAInfraDetails)
		if err != nil {
			return nil, err
		}
		return automateHAInfraDetails, nil
	} else if checkIfFileExist(automateHATerraformDestroyOutputFile) {
		automateHAInfraDetails := &AutomteHAInfraDetails{}
		contents, err := ioutil.ReadFile(automateHATerraformDestroyOutputFile)
		if err != nil {
			return nil, err
		}
		err = json.Unmarshal(contents, automateHAInfraDetails)
		if err != nil {
			return nil, err
		}
		return automateHAInfraDetails, nil
	} else {
		writer.Error("Automate Ha infra confile file not exits.")
		return nil, nil
	}
}

func getIPOfRequestedServers(servername string, d *AutomteHAInfraDetails) ([]string, error) {
	switch servername {
	case "automate":
		return d.Outputs.AutomateSSH.Value, nil
	case "chef_server":
		return d.Outputs.ChefServerSSH.Value, nil
	case "postgresql":
		return d.Outputs.PostgresqlSSH.Value, nil
	case "elasticsearch":
		return d.Outputs.ElasticsearchSSH.Value, nil
	default:
		return nil, errors.New("invalid hostname possible values should be any one of automate, chef_server, postgresql or elasticsearch")
	}
}
