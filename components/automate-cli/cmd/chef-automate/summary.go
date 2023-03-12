package main

import (
	"container/list"
	"encoding/json"
	"fmt"
	"os/exec"
	"strconv"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/stringutils"
	"github.com/jedib0t/go-pretty/v5/table"
	"github.com/spf13/cobra"
)

type StatusSummary interface {
	Run() error
	FEDisplay()
	BEDisplay()
}

type Summary struct {
	writer                *cli.Writer
	fileutils             fileutils.FileUtils
	feStatus              FeStatus
	beStatus              BeStatus
	timeout               int64
	spinnerTimeout        time.Duration
	infra                 *AutomteHAInfraDetails
	statusSummaryCmdFlags *StatusSummaryCmdFlags
}

type A2haHabitatAutoTfvars struct {
	HabSupHttpGatewayAuthToken string `json:"hab_sup_http_gateway_auth_token"`
	HabSupRingKey              string `json:"hab_sup_ring_key"`
}


// Display implements StatusSummary
func (ss *Summary) FEDisplay() {
	if len(ss.feStatus) != 0 {
		fmt.Println("Frontend Services")
		t := table.NewWriter()
		tTemp := table.Table{}
		tTemp.Render()
		for _, status := range ss.feStatus {
			t.AppendRow(table.Row{status.serviceName, status.ipAddress, status.status, status.Opensearch})
		}
		t.AppendHeader(table.Row{"Name", "IP Address", "Status", "Opensearch"})
		fmt.Println(t.Render())
	}
}

// Display implements StatusSummary
func (ss *Summary) BEDisplay() {
	if len(ss.beStatus) != 0 {
		fmt.Println("Backend Services")
		t := table.NewWriter()
		tTemp := table.Table{}
		tTemp.Render()
		for _, status := range ss.beStatus {
			t.AppendRow(table.Row{status.serviceName, status.ipAddress, status.health, status.process, status.upTime, status.role})
		}
		t.AppendHeader(table.Row{"Name", "IP Address", "Health", "Process", "Uptime", "Role"})
		fmt.Println(t.Render())
	}
}

// Run implements StatusSummary
func (ss *Summary) Run() error {
	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := ss.CheckIPAddresses()
	if errList != nil && errList.Len() > 0 {
		return status.Wrap(getSingleErrorFromList(errList), status.ConfigError, "IP address validation failed")
	}
	sshUtil := ss.setSSHConfig()
	err := ss.connectToFeNode(sshUtil, automateIps, "Automate", "FE")
	if err != nil {
		return err
	}
	err = ss.connectToFeNode(sshUtil, chefServerIps, "Chef Server", "FE")
	if err != nil {
		return err
	}
	err = ss.connectToBeNode(sshUtil, opensearchIps, "Open Search", "BE")
	if err != nil {
		return err
	}
	err = ss.connectToBeNode(sshUtil, postgresqlIps, "Postgresql", "BE")
	if err != nil {
		return err
	}
	return nil
}

func (ss *Summary) setSSHConfig() SSHUtil {
	sshconfig := &SSHConfig{}
	sshconfig.sshUser = ss.infra.Outputs.SSHUser.Value
	sshconfig.sshKeyFile = ss.infra.Outputs.SSHKeyFile.Value
	sshconfig.sshPort = ss.infra.Outputs.SSHPort.Value
	sshUtil := NewSSHUtil(sshconfig)
	return sshUtil
}

func (ss *Summary) connectToBeNode(sshUtil SSHUtil, serviceIps []string, serviceName, serviceType string) error {
	script := ""
	err := ss.executeCommandForArrayofIPs(sshUtil, serviceIps, script, serviceName, serviceType)
	if err != nil {
		return err
	}

	return nil
}

func (ss *Summary) connectToFeNode(sshUtil SSHUtil, serviceIps []string, serviceName, serviceType string) error {
	script := GenerateOriginalAutomateCLICommand(
		&cobra.Command{
			Use: "chef-automate",
		}, []string{
			"status",
			"-t",
			"150",
		})

	err := ss.executeCommandForArrayofIPs(sshUtil, serviceIps, script, serviceName, serviceType)
	if err != nil {
		return err
	}
	return nil
}

func (ss *Summary) executeCommandForArrayofIPs(sshUtil SSHUtil, remoteIps []string, script, serviceName, serviceType string) error {
	getConfigJsonString := convTfvarToJson(terraformPath + "/a2ha_habitat.auto.tfvars")
	for i := 0; i < len(remoteIps); i++ {
		sshUtil.getSSHConfig().hostIP = remoteIps[i]
		if serviceType == "FE" {
			status := ss.getFEStatus(sshUtil, script, remoteIps[i], serviceName)
			ss.feStatus = append(ss.feStatus, status)
		}
		if serviceType == "BE" {
			authToken, err := ss.readA2haHabitatAutoTfvarsAuthToken(getConfigJsonString)
			if err != nil {
				return err
			}
			status, err := ss.getBEStatus(sshUtil, remoteIps[i], authToken, serviceName)
			if err != nil {
				return err
			}
			ss.beStatus = append(ss.beStatus, status)
		}

	}
	return nil
}

func (ss *Summary) readA2haHabitatAutoTfvarsAuthToken(getConfigJsonString string) (string, error) {
	config := A2haHabitatAutoTfvars{}
	err := json.Unmarshal([]byte(getConfigJsonString), &config)
	if err != nil {
		return "", err
	}
	authToken := config.HabSupHttpGatewayAuthToken

	return authToken, nil
}
func (ss *Summary) getBEStatus(sshUtil SSHUtil, ip string, authToken, serviceType string) (BeStatusValue, error) {
	// curl --location --request GET 'https://localhost:9631/services/automate-ha-opensearch/default' --header 'Authorization: Bearer zeLm+WL26/S2DRpGL9z2d8G+Q8HM0Ht0Jdq5ytqg3ik=' -k
	service := ""
	if serviceType == "Open Search" {
		service = "automate-ha-opensearch"
	} else if serviceType == "Postgresql" {
		service = "automate-ha-postgresql"
	}
	args := []string{
		"-s",
		fmt.Sprintf("https://localhost:9631/services/%s/default", service),
		"--header",
		fmt.Sprintf("'Authorization: Bearer %s'", authToken),
		"-k",
	}
	defaultServiceDetails, err := executeCmd(sshUtil, "curl", args)
	if err != nil {
		return BeStatusValue{}, err
	}
	// memeber_id
	memeber_id := defaultServiceDetails["sys"].(map[string]interface{})["member_id"]
	serviceState := defaultServiceDetails["process"].(map[string]interface{})["state"].(string)
	servicePid := fmt.Sprint(defaultServiceDetails["process"].(map[string]interface{})["pid"])
	startingTime := defaultServiceDetails["process"].(map[string]interface{})["state_entered"].(float64)
	startingTime = float64(time.Now().UTC().Unix()) - startingTime
	// curl --location --request GET 'https://localhost:9631/services/automate-ha-opensearch/default/health' --header 'Authorization: Bearer zeLm+WL26/S2DRpGL9z2d8G+Q8HM0Ht0Jdq5ytqg3ik=' -k
	args = []string{
		"-s",
		fmt.Sprintf("https://localhost:9631/services/%s/default/health", service),
		"--header",
		fmt.Sprintf("'Authorization: Bearer %s'", authToken),
		"-k",
	}
	ServiceHealth, err := executeCmd(sshUtil, "curl", args)
	if err != nil {
		return BeStatusValue{}, err
	}

	health := fmt.Sprint(ServiceHealth["status"])

	// /census
	args = []string{
		"-s",
		"https://localhost:9631/census",
		"--header",
		fmt.Sprintf("'Authorization: Bearer %s'", authToken),
		"-k",
	}
	censusData, err := executeCmd(sshUtil, "curl", args)
	if err != nil {
		return BeStatusValue{}, err
	}

	populationData := censusData["census_groups"].(map[string]interface{})[service+".default"].(map[string]interface{})["population"].(map[string]interface{})[fmt.Sprintf("%s", memeber_id)]
	getrole, ok := populationData.(map[string]interface{})
	role := "Unknown"
	if ok {
		isleader, _ := strconv.ParseBool(fmt.Sprint(getrole["leader"]))
		isfollower, _ := strconv.ParseBool(fmt.Sprint(getrole["follower"]))
		if isleader {
			role = "Leader"
		} else if isfollower {
			role = "Follower"
		}
	}

	t := time.Unix(int64(startingTime), 0)
	duration := t.Sub(time.Unix(0, 0))

	// Format the duration as "148h 43m 31s"
	formatted := fmt.Sprintf("%dd %dh %dm %ds", int(duration.Hours())/24, int(duration.Hours())%24, int(duration.Minutes())%60, int(duration.Seconds())%60)

	return BeStatusValue{
		serviceName: serviceType,
		ipAddress:   ip,
		health:      health,
		process:     fmt.Sprintf("%s (pid: %s)", serviceState, servicePid),
		upTime:      formatted,
		role:        role,
	}, nil
}
func (ss *Summary) getFEStatus(sshUtil SSHUtil, script string, ip string, serviceType string) FeStatusValue {
	entry := FeStatusValue{}
	osStatus := ss.opensearchStatusInFE(sshUtil)
	_, err := sshUtil.connectAndExecuteCommandOnRemote(script, true)
	entry.ipAddress = ip
	entry.serviceName = serviceType
	entry.ipAddress = ip
	entry.Opensearch = osStatus
	if err != nil {
		_, ok := err.(*exec.ExitError)
		if ok {
			entry.status = "ERROR"
		} else {
			entry.status = "WARN"
		}
	} else {
		entry.status = "OK"
	}
	return entry
}
func (ss *Summary) opensearchStatusInFE(sshUtil SSHUtil) string {
	arg := []string{
		"-s",
		"http://localhost:10144/_cluster/health",
	}
	curlOSStatusScript := GenerateOriginalAutomateCLICommand(
		&cobra.Command{
			Use: "curl",
		}, arg)
	osStatusJson, _ := sshUtil.connectAndExecuteCommandOnRemote(curlOSStatusScript, true)

	body := []byte(osStatusJson)
	esHealthData := make(map[string]json.RawMessage)

	err := json.Unmarshal(body, &esHealthData)
	if err != nil {
		return "Unknown"
	}

	return fmt.Sprintf("%s Active: %s", esHealthData["status"], esHealthData["active_shards_percent_as_number"])
}

func executeCmd(sshUtil SSHUtil, cmd string, args []string) (map[string]interface{}, error) {
	script := GenerateOriginalAutomateCLICommand(&cobra.Command{
		Use: "curl",
	}, args)
	output, err := sshUtil.connectAndExecuteCommandOnRemote(script, true)
	if err != nil {
		return nil, err
	}

	var body map[string]interface{}
	err = json.Unmarshal([]byte(output), &body)
	if err != nil {
		return nil, err
	}
	return body, nil
}

// ValidateIPAddresses implements StatusSummary
func (ss *Summary) CheckIPAddresses() ([]string, []string, []string, []string, *list.List) {
	errorList := list.New()
	var automateIps, chefServerIps, opensearchIps, postgresqlIps []string
	if ss.statusSummaryCmdFlags.automateIp != "" ||
		ss.statusSummaryCmdFlags.chefServerIp != "" ||
		ss.statusSummaryCmdFlags.opensearchIp != "" ||
		ss.statusSummaryCmdFlags.postgresqlIp != "" {
		automateIpsFromcmd, chefServerIpsFromcmd, opensearchIpsFromcmd, postgresqlIpsFromcmd := splitIPCSV(
			ss.statusSummaryCmdFlags.automateIp,
			ss.statusSummaryCmdFlags.chefServerIp,
			ss.statusSummaryCmdFlags.opensearchIp,
			ss.statusSummaryCmdFlags.postgresqlIp,
		)
		if len(automateIpsFromcmd) != 0 {
			datafound, dataNotFound, err := validateIPAddresses(automateIpsFromcmd, ss.infra.Outputs.AutomatePrivateIps.Value, "Automate")
			automateIps = datafound
			if len(dataNotFound) != 0 {
				errorList.PushBack(fmt.Sprintf("List of IpAddress not found %s", dataNotFound))
			}
			if err != nil && err.Len() > 0 {
				errorList.PushBack(fmt.Sprintf(getSingleErrorFromList(err).Error(), "IP address validation failed"))
			}
		}
		if len(chefServerIpsFromcmd) != 0 {
			datafound, dataNotFound, err := validateIPAddresses(chefServerIpsFromcmd, ss.infra.Outputs.ChefServerPrivateIps.Value, "chef-server")
			chefServerIps = datafound
			if len(dataNotFound) != 0 {
				errorList.PushBack(fmt.Sprintf("List of IpAddress not found %s", dataNotFound))
			}
			if err != nil && err.Len() > 0 {
				errorList.PushBack(fmt.Sprintf(getSingleErrorFromList(err).Error(), "IP address validation failed"))
			}
		}
		if len(opensearchIpsFromcmd) != 0 {
			datafound, dataNotFound, err := validateIPAddresses(opensearchIpsFromcmd, ss.infra.Outputs.OpensearchPrivateIps.Value, "open search")
			if len(dataNotFound) != 0 {
				errorList.PushBack(fmt.Sprintf("List of IpAddress not found %s", dataNotFound))
			}
			opensearchIps = datafound
			if err != nil && err.Len() > 0 {
				errorList.PushBack(fmt.Sprintf(getSingleErrorFromList(err).Error(), "IP address validation failed"))
			}
		}
		if len(postgresqlIpsFromcmd) != 0 {
			datafound, dataNotFound, err := validateIPAddresses(postgresqlIpsFromcmd, ss.infra.Outputs.PostgresqlPrivateIps.Value, "postgres")
			opensearchIps = datafound
			if len(dataNotFound) != 0 {
				errorList.PushBack(fmt.Sprintf("List of IpAddress not found %s", dataNotFound))
			}
			if err != nil && err.Len() > 0 {
				errorList.PushBack(fmt.Sprintf(getSingleErrorFromList(err).Error(), "IP address validation failed"))
			}
		}
	} else {
		if ss.statusSummaryCmdFlags.isAutomate {
			automateIps = ss.infra.Outputs.AutomatePrivateIps.Value
		} else if ss.statusSummaryCmdFlags.isChefServer {
			chefServerIps = ss.infra.Outputs.ChefServerPrivateIps.Value
		} else if ss.statusSummaryCmdFlags.isOpenSearch {
			opensearchIps = ss.infra.Outputs.OpensearchPrivateIps.Value
		} else if ss.statusSummaryCmdFlags.isPostgresql {
			postgresqlIps = ss.infra.Outputs.PostgresqlPrivateIps.Value
		} else {
			automateIps = ss.infra.Outputs.AutomatePrivateIps.Value
			chefServerIps = ss.infra.Outputs.ChefServerPrivateIps.Value
			opensearchIps = ss.infra.Outputs.OpensearchPrivateIps.Value
			postgresqlIps = ss.infra.Outputs.PostgresqlPrivateIps.Value
		}
	}
	return automateIps, chefServerIps, opensearchIps, postgresqlIps, errorList
}

func validateIPAddresses(IpsFromcmd, ips []string, errorPrefix string) ([]string, []string, *list.List) {
	errorList := list.New()
	var ipFound, ipNotFound []string

	for _, ip := range IpsFromcmd {
		err := checkIPAddress(ip)
		if err != nil {
			errorList.PushBack(fmt.Sprintf("Incorrect %s IP address format for ip %s", errorPrefix, ip))

		}
		if stringutils.SliceContains(ips, ip) {
			ipFound = append(ipFound, ip)
		} else {
			ipNotFound = append(ipNotFound, ip)
		}
	}
	return ipFound, ipNotFound, errorList
}

func NewStatusSummary(writer *cli.Writer, infra *AutomteHAInfraDetails, fileutils fileutils.FileUtils, feStatus FeStatus, beStatus BeStatus, timeout int64, spinnerTimeout time.Duration, flags *StatusSummaryCmdFlags) StatusSummary {
	return &Summary{
		writer:                writer,
		fileutils:             fileutils,
		feStatus:              feStatus,
		beStatus:              beStatus,
		timeout:               timeout,
		spinnerTimeout:        spinnerTimeout,
		infra:                 infra,
		statusSummaryCmdFlags: flags,
	}
}
