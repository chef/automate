package main

import (
	"container/list"
	"encoding/json"
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/lib/stringutils"
	"github.com/jedib0t/go-pretty/v5/table"
	"github.com/spf13/cobra"
)

var (
	habUrl                = "https://localhost:9631"
	clusterOSHealth       = "http://localhost:10144/_cluster/health"
	a2haHabitatAutoTfvars = terraformPath + "/a2ha_habitat.auto.tfvars"
	nowFunc               = time.Now
)

const (
	ipAddressError    = "IP address validation failed"
	curlHeaderFlag    = "--header"
	curlAuthorization = "'Authorization: Bearer %s'"
)

type StatusSummary interface {
	Prepare(sshUtil SSHUtil) error
	ShowFEStatus() string
	ShowBEStatus() string
}

type Summary struct {
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

// Display Frontend Status
func (ss *Summary) ShowFEStatus() string {
	if len(ss.feStatus) != 0 {
		fmt.Println("Frontend Services")
		t := table.NewWriter()
		tTemp := table.Table{}
		tTemp.Render()
		for _, status := range ss.feStatus {
			t.AppendRow(table.Row{status.serviceName, status.ipAddress, status.status, status.Opensearch})
		}
		t.AppendHeader(table.Row{"Name", "IP Address", "Status", "Opensearch"})
		return t.Render()
	}
	return ""
}

// Display Backend Status
func (ss *Summary) ShowBEStatus() string {
	if len(ss.beStatus) != 0 {
		fmt.Println("Backend Services")
		t := table.NewWriter()
		tTemp := table.Table{}
		tTemp.Render()
		for _, status := range ss.beStatus {
			t.AppendRow(table.Row{status.serviceName, status.ipAddress, status.health, status.process, status.upTime, status.role})
		}
		t.AppendHeader(table.Row{"Name", "IP Address", "Health", "Process", "Uptime", "Role"})
		return t.Render()
	}
	return ""
}

// Run Status summary
func (ss *Summary) Prepare(sshUtil SSHUtil) error {
	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := ss.checkIPAddresses()
	if errList != nil && errList.Len() > 0 {
		return status.Wrap(getSingleErrorFromList(errList), status.ConfigError, ipAddressError)
	}
	err := ss.prepareFEScript(sshUtil, automateIps, "Automate", "FE")
	if err != nil {
		return err
	}
	err = ss.prepareFEScript(sshUtil, chefServerIps, "Chef Server", "FE")
	if err != nil {
		return err
	}
	err = ss.prepareBEScript(sshUtil, opensearchIps, "Open Search", "BE")
	if err != nil {
		return err
	}
	err = ss.prepareBEScript(sshUtil, postgresqlIps, "Postgresql", "BE")
	if err != nil {
		return err
	}
	return nil
}

// get sshConfig
func (ss *Summary) getSSHConfig() SSHUtil {
	sshconfig := &SSHConfig{}
	sshconfig.sshUser = ss.infra.Outputs.SSHUser.Value
	sshconfig.sshKeyFile = ss.infra.Outputs.SSHKeyFile.Value
	sshconfig.sshPort = ss.infra.Outputs.SSHPort.Value
	sshUtil := NewSSHUtil(sshconfig)
	return sshUtil
}

func (ss *Summary) prepareBEScript(sshUtil SSHUtil, serviceIps []string, serviceName, serviceType string) error {
	script := ""
	err := ss.executeCommandForArrayofIPs(sshUtil, serviceIps, script, serviceName, serviceType)
	if err != nil {
		return err
	}

	return nil
}

func (ss *Summary) prepareFEScript(sshUtil SSHUtil, serviceIps []string, serviceName, serviceType string) error {
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
	getConfigJsonString := convTfvarToJson(a2haHabitatAutoTfvars)
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
	service := ""
	if serviceType == "Open Search" {
		service = "automate-ha-opensearch"
	} else if serviceType == "Postgresql" {
		service = "automate-ha-postgresql"
	}
	args := []string{
		"-s",
		fmt.Sprintf("%s/services/%s/default", habUrl, service),
		curlHeaderFlag,
		fmt.Sprintf(curlAuthorization, authToken),
		"-k",
	}
	defaultServiceDetails, err := executeCmd(sshUtil, "curl", args)
	if err != nil {
		return BeStatusValue{}, err
	}
	memeberId := defaultServiceDetails["sys"].(map[string]interface{})["member_id"]
	serviceState := defaultServiceDetails["process"].(map[string]interface{})["state"].(string)
	servicePid := fmt.Sprint(defaultServiceDetails["process"].(map[string]interface{})["pid"])
	startingTime := defaultServiceDetails["process"].(map[string]interface{})["state_entered"].(float64)
	startingTime = float64(nowFunc().UTC().Unix()) - startingTime
	args = []string{
		"-s",
		fmt.Sprintf("%s/services/%s/default/health", habUrl, service),
		curlHeaderFlag,
		fmt.Sprintf(curlAuthorization, authToken),
		"-k",
	}
	ServiceHealth, err := executeCmd(sshUtil, "curl", args)
	if err != nil {
		return BeStatusValue{}, err
	}

	health := fmt.Sprint(ServiceHealth["status"])

	args = []string{
		"-s",
		fmt.Sprintf("%s/census", habUrl),
		curlHeaderFlag,
		fmt.Sprintf(curlAuthorization, authToken),
		"-k",
	}
	censusData, err := executeCmd(sshUtil, "curl", args)
	if err != nil {
		return BeStatusValue{}, err
	}

	populationData := censusData["census_groups"].(map[string]interface{})[service+".default"].(map[string]interface{})["population"].(map[string]interface{})[fmt.Sprintf("%s", memeberId)]
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

	// Format the duration as "1d 148h 43m 31s"
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
	osStatus := ss.opensearchStatusInFE(sshUtil)
	_, err := sshUtil.connectAndExecuteCommandOnRemote(script, true)
	status := "OK"
	if err != nil {
		_, ok := err.(*exec.ExitError)
		if ok {
			status = "ERROR"
		} else {
			status = "WARN"
		}
	}
	return FeStatusValue{
		serviceName: serviceType,
		ipAddress:   ip,
		status:      status,
		Opensearch:  osStatus,
	}
}

func (ss *Summary) opensearchStatusInFE(sshUtil SSHUtil) string {
	arg := []string{
		"-s",
		clusterOSHealth,
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

	return fmt.Sprintf("%s (Active: %s)", esHealthData["status"], esHealthData["active_shards_percent_as_number"])
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

func (ss *Summary) checkIPAddresses() ([]string, []string, []string, []string, *list.List) {
	errorList := list.New()
	var automateIps, chefServerIps, opensearchIps, postgresqlIps []string
	if ss.statusSummaryCmdFlags.node != "" {
		automateIps, chefServerIps, opensearchIps, postgresqlIps, errorList = ss.getIpAddressesFromFlag(errorList)
	} else {
		if ss.statusSummaryCmdFlags.isAutomate {
			automateIps = ss.infra.Outputs.AutomatePrivateIps.Value
		}
		if ss.statusSummaryCmdFlags.isChefServer {
			chefServerIps = ss.infra.Outputs.ChefServerPrivateIps.Value
		}
		if ss.statusSummaryCmdFlags.isOpenSearch {
			opensearchIps = ss.infra.Outputs.OpensearchPrivateIps.Value
		}
		if ss.statusSummaryCmdFlags.isPostgresql {
			postgresqlIps = ss.infra.Outputs.PostgresqlPrivateIps.Value
		}
		if !ss.statusSummaryCmdFlags.isAutomate &&
			!ss.statusSummaryCmdFlags.isChefServer &&
			!ss.statusSummaryCmdFlags.isOpenSearch &&
			!ss.statusSummaryCmdFlags.isPostgresql {
			automateIps = ss.infra.Outputs.AutomatePrivateIps.Value
			chefServerIps = ss.infra.Outputs.ChefServerPrivateIps.Value
			opensearchIps = ss.infra.Outputs.OpensearchPrivateIps.Value
			postgresqlIps = ss.infra.Outputs.PostgresqlPrivateIps.Value
		}
	}
	return automateIps, chefServerIps, opensearchIps, postgresqlIps, errorList
}

func (ss *Summary) getIpAddressesFromFlag(errorList *list.List) ([]string, []string, []string, []string, *list.List) {
	var automateIps, chefServerIps, opensearchIps, postgresqlIps, nodes []string
	if ss.statusSummaryCmdFlags.isAutomate ||
		ss.statusSummaryCmdFlags.isChefServer ||
		ss.statusSummaryCmdFlags.isOpenSearch ||
		ss.statusSummaryCmdFlags.isPostgresql {
		nodes = ss.splitIP(
			ss.statusSummaryCmdFlags.node,
		)
		if len(nodes) != 0 {
			if ss.statusSummaryCmdFlags.isAutomate {
				datafound, dataNotFound, err := validateIPAddresses(nodes, ss.infra.Outputs.AutomatePrivateIps.Value, "Automate")
				automateIps = datafound
				nodes = dataNotFound
				if err != nil && err.Len() > 0 {
					errorList.PushBack(fmt.Sprintf(getSingleErrorFromList(err).Error(), ipAddressError))
				}
			}
			if ss.statusSummaryCmdFlags.isChefServer {
				datafound, dataNotFound, err := validateIPAddresses(nodes, ss.infra.Outputs.ChefServerPrivateIps.Value, "chef-server")
				chefServerIps = datafound
				nodes = dataNotFound
				if err != nil && err.Len() > 0 {
					errorList.PushBack(fmt.Sprintf(getSingleErrorFromList(err).Error(), ipAddressError))
				}
			}
			if ss.statusSummaryCmdFlags.isOpenSearch {
				datafound, dataNotFound, err := validateIPAddresses(nodes, ss.infra.Outputs.OpensearchPrivateIps.Value, "open search")
				opensearchIps = datafound
				nodes = dataNotFound

				if err != nil && err.Len() > 0 {
					errorList.PushBack(fmt.Sprintf(getSingleErrorFromList(err).Error(), ipAddressError))
				}
			}
			if ss.statusSummaryCmdFlags.isPostgresql {
				datafound, dataNotFound, err := validateIPAddresses(nodes, ss.infra.Outputs.PostgresqlPrivateIps.Value, "postgres")
				postgresqlIps = datafound
				nodes = dataNotFound
				if err != nil && err.Len() > 0 {
					errorList.PushBack(fmt.Sprintf(getSingleErrorFromList(err).Error(), ipAddressError))
				}
			}

			if len(nodes) != 0 {
				errorList.PushBack(fmt.Sprintf("List of  ip address not found %s", nodes))
			}
		}

	} else {
		errorList.PushBack("Please Provide service flag")
	}
	return automateIps, chefServerIps, opensearchIps, postgresqlIps, errorList

}

func (ss *Summary) splitIP(node string) (nodes []string) {
	nodes = strings.Split(node, ",")
	nodes = trimSliceSpace(nodes)
	return
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

func getCurrentTime() time.Time {
	return nowFunc()
}

func NewStatusSummary(infra *AutomteHAInfraDetails, feStatus FeStatus, beStatus BeStatus, timeout int64, spinnerTimeout time.Duration, flags *StatusSummaryCmdFlags) StatusSummary {
	return &Summary{
		feStatus:              feStatus,
		beStatus:              beStatus,
		timeout:               timeout,
		spinnerTimeout:        spinnerTimeout,
		infra:                 infra,
		statusSummaryCmdFlags: flags,
	}
}
