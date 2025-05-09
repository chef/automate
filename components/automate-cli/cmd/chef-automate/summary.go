package main

import (
	"container/list"
	"encoding/json"
	"fmt"
	"regexp"
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
	ipAddressError              = " IP address validation failed"
	curlHeaderFlag              = "--header"
	curlAuthorization           = "'Authorization: Bearer %s'"
	initialServiceState         = "down"
	initialServicePid           = "0"
	initialHealth               = "ERROR"
	initialFormattedDuration    = "0d 0h 0m 0s"
	initialRole                 = "Unknown"
	initialLag                  = "NA"
	defaultServiceDetails       = "DefaultServiceDetails"
	defaultServiceHealthDetails = "DefaultServiceHealthDetails"
	censusDetails               = "CensusDetails"
	opensearchName              = "opensearch"
	automateName                = "automate"
	chefServerName              = "chef-server"
	postgresqlName              = "postgresql"
)

type StatusSummary interface {
	Prepare() error
	ShowFEStatus() string
	ShowBEStatus() string
	GetPGLeaderNode() (string, string)
	GetPGMaxLagAmongFollowers() int64
	GetPGFollwerNodes() map[string]string
}

type Summary struct {
	feStatus              FeStatus
	beStatus              BeStatus
	timeout               int64
	spinnerTimeout        time.Duration
	infra                 *AutomateHAInfraDetails
	statusSummaryCmdFlags *StatusSummaryCmdFlags
	remoteCmdExecutor     RemoteCmdExecutor
}

type A2haHabitatAutoTfvars struct {
	HabSupHttpGatewayAuthToken string `json:"hab_sup_http_gateway_auth_token"`
	HabSupRingKey              string `json:"hab_sup_ring_key"`
}

func NewStatusSummary(infra *AutomateHAInfraDetails, feStatus FeStatus, beStatus BeStatus, timeout int64, spinnerTimeout time.Duration, flags *StatusSummaryCmdFlags, remoteCmdExecutor RemoteCmdExecutor) StatusSummary {
	return &Summary{
		feStatus:              feStatus,
		beStatus:              beStatus,
		timeout:               timeout,
		spinnerTimeout:        spinnerTimeout,
		infra:                 infra,
		statusSummaryCmdFlags: flags,
		remoteCmdExecutor:     remoteCmdExecutor,
	}
}

// Run Status summary
func (ss *Summary) Prepare() error {
	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := ss.getIPAddressesFromFlagOrInfra()
	if errList != nil && errList.Len() > 0 {
		return status.Wrap(getSingleErrorFromList(errList), status.InvalidCommandArgsError, ipAddressError)
	}

	if len(automateIps) != 0 {
		automateNodeMap := ss.automateNodeMap(automateIps)
		err := ss.prepareFEScript(automateIps, automateNodeMap, automateName, "FE")
		if err != nil {
			return err
		}
	}

	if len(chefServerIps) != 0 {
		chefServerNodeMap := ss.chefServerNodeMap(chefServerIps)
		err := ss.prepareFEScript(chefServerIps, chefServerNodeMap, chefServerName, "FE")
		if err != nil {
			return err
		}
	}

	if len(opensearchIps) != 0 {
		openSearchNodeMap := ss.openSearchNodeMap(opensearchIps)
		err := ss.prepareBEScript(opensearchIps, openSearchNodeMap, opensearchName, "BE")
		if err != nil {
			return err
		}
	}

	if len(postgresqlIps) != 0 {
		postgresqlNodeMap := ss.postgresqlNodeMap(postgresqlIps)
		err := ss.prepareBEScript(postgresqlIps, postgresqlNodeMap, postgresqlName, "BE")
		if err != nil {
			return err
		}
	}
	return nil
}

func (ss *Summary) postgresqlNodeMap(postgresqlIps []string) *NodeTypeAndCmd {
	postgresqlNodeMap := &NodeTypeAndCmd{
		Frontend:   &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Automate:   &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		ChefServer: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Postgresql: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      "",
				NodeIps:                  postgresqlIps,
				Single:                   false,
				NodeType:                 true,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Infra: ss.infra,
	}
	return postgresqlNodeMap
}

func (ss *Summary) openSearchNodeMap(opensearchIps []string) *NodeTypeAndCmd {
	openSearchNodeMap := &NodeTypeAndCmd{
		Frontend:   &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Automate:   &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		ChefServer: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Opensearch: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      "",
				NodeIps:                  opensearchIps,
				Single:                   false,
				NodeType:                 true,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Infra:      ss.infra,
	}
	return openSearchNodeMap
}

func (ss *Summary) chefServerNodeMap(chefServerIps []string) *NodeTypeAndCmd {
	chefServerNodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Automate: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		ChefServer: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      "",
				NodeIps:                  chefServerIps,
				Single:                   false,
				NodeType:                 true,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Infra:      ss.infra,
	}
	return chefServerNodeMap
}

func (ss *Summary) automateNodeMap(automateIps []string) *NodeTypeAndCmd {
	automateNodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Automate: &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      "",
				NodeIps:                  automateIps,
				Single:                   false,
				NodeType:                 true,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true,
			},
		},
		Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Infra:      ss.infra,
	}
	return automateNodeMap
}

// returns automateIps, chefServerIps, opensearchIps, postgresqlIps, errList
func (ss *Summary) getIPAddressesFromFlagOrInfra() ([]string, []string, []string, []string, *list.List) {
	errorList := list.New()
	if ss.statusSummaryCmdFlags.node != "" {
		automateIps, chefServerIps, opensearchIps, postgresqlIps, errorList := ss.getIpAddressesFromFlag(errorList)
		return automateIps, chefServerIps, opensearchIps, postgresqlIps, errorList
	}
	return ss.getIpAddressesFromInfra(errorList)
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
				automateIps, nodes, errorList = ss.validateIPAddresses(errorList, nodes, automateName, ipAddressError)
			}
			if ss.statusSummaryCmdFlags.isChefServer {
				chefServerIps, nodes, errorList = ss.validateIPAddresses(errorList, nodes, chefServerName, ipAddressError)
			}
			if ss.statusSummaryCmdFlags.isOpenSearch {
				opensearchIps, nodes, errorList = ss.validateIPAddresses(errorList, nodes, opensearchName, ipAddressError)
			}
			if ss.statusSummaryCmdFlags.isPostgresql {
				postgresqlIps, nodes, errorList = ss.validateIPAddresses(errorList, nodes, postgresqlName, ipAddressError)
			}
			if len(nodes) != 0 && errorList.Len() == 0 {
				if ss.statusSummaryCmdFlags.isAutomate ||
					ss.statusSummaryCmdFlags.isChefServer ||
					ss.statusSummaryCmdFlags.isOpenSearch ||
					ss.statusSummaryCmdFlags.isPostgresql {
					services := []string{}
					if ss.statusSummaryCmdFlags.isAutomate {
						services = append(services, "Automate")
					}
					if ss.statusSummaryCmdFlags.isChefServer {
						services = append(services, "Chef server")
					}
					if ss.statusSummaryCmdFlags.isOpenSearch {
						services = append(services, "Opensearch")
					}
					if ss.statusSummaryCmdFlags.isPostgresql {
						services = append(services, "PostgreSQL")
					}
					errorList.PushBack(fmt.Sprintf("List of  ip address not found %s does not match any node for %s services", nodes, strings.Join(services[:], ", ")))
				} else {
					errorList.PushBack(fmt.Sprintf("List of  ip address not found %s does not match any node for Automate, PostgreSQL, Opensearch and ChefServer services", nodes))
				}
			}
		}
		return automateIps, chefServerIps, opensearchIps, postgresqlIps, errorList
	}

	errorList.PushBack("Please Provide service flag")
	return automateIps, chefServerIps, opensearchIps, postgresqlIps, errorList

}

func (ss *Summary) getIpAddressesFromInfra(errorList *list.List) ([]string, []string, []string, []string, *list.List) {
	var automateIps, chefServerIps, opensearchIps, postgresqlIps []string

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
	return automateIps, chefServerIps, opensearchIps, postgresqlIps, errorList
}

func (ss *Summary) prepareBEScript(serviceIps []string, nodeMap *NodeTypeAndCmd, serviceName, serviceType string) error {
	getConfigJsonString := convTfvarToJson(a2haHabitatAutoTfvars)
	authToken, err := ss.readA2haHabitatAutoTfvarsAuthToken(getConfigJsonString)
	if err != nil {
		return err
	}

	// default service details
	defaultServiceDetailsCmd := GenerateOriginalAutomateCLICommand(
		&cobra.Command{
			Use: "curl",
		}, []string{
			"-s",
			fmt.Sprintf("%s/services/automate-ha-%s/default", habUrl, serviceName),
			curlHeaderFlag,
			fmt.Sprintf(curlAuthorization, authToken),
			"-k",
		})

	// default service health details
	defaultServiceHealthDetailsCmd := GenerateOriginalAutomateCLICommand(
		&cobra.Command{
			Use: "curl",
		}, []string{
			"-s",
			fmt.Sprintf("%s/services/automate-ha-%s/default/health", habUrl, serviceName),
			curlHeaderFlag,
			fmt.Sprintf(curlAuthorization, authToken),
			"-k",
		})

	// census details
	censusDetailsCmd := GenerateOriginalAutomateCLICommand(
		&cobra.Command{
			Use: "curl",
		}, []string{
			"-s",
			fmt.Sprintf("%s/census", habUrl),
			curlHeaderFlag,
			fmt.Sprintf(curlAuthorization, authToken),
			"-k",
		})

	script := map[string]string{
		defaultServiceDetails:       defaultServiceDetailsCmd,
		defaultServiceHealthDetails: defaultServiceHealthDetailsCmd,
		censusDetails:               censusDetailsCmd,
	}

	if serviceName == opensearchName {
		nodeMap.Opensearch.CmdInputs.MutipleCmdWithArgs = script
	}

	if serviceName == postgresqlName {
		nodeMap.Postgresql.CmdInputs.MutipleCmdWithArgs = script
	}

	beOutput, err := ss.remoteCmdExecutor.ExecuteWithNodeMap(nodeMap)
	if err != nil {
		return err
	}

	for ip, res := range beOutput {
		status := ss.getBEStatus(res, ip, authToken, serviceName)
		ss.beStatus = append(ss.beStatus, status)
	}

	return nil
}

func (ss *Summary) prepareFEScript(serviceIps []string, nodeMap *NodeTypeAndCmd, serviceName, serviceType string) error {

	// Status Script
	status := GenerateOriginalAutomateCLICommand(
		&cobra.Command{
			Use: "chef-automate",
		}, []string{
			"status",
			"-t",
			"150",
		})

	// OsStatus Script
	osStatus := GenerateOriginalAutomateCLICommand(
		&cobra.Command{
			Use: "curl",
		}, []string{
			"-s",
			clusterOSHealth,
		})

	maintStatus := GenerateOriginalAutomateCLICommand(
		&cobra.Command{
			Use: "chef-automate",
		}, []string{
			"config",
			"show",
			"|",
			"grep",
			"maintenance_mode",
		})

	script := map[string]string{
		"Status":      status,
		"OsStatus":    osStatus,
		"MaintStatus": maintStatus,
	}

	if serviceName == automateName {
		nodeMap.Automate.CmdInputs.MutipleCmdWithArgs = script
	}

	if serviceName == chefServerName {
		nodeMap.ChefServer.CmdInputs.MutipleCmdWithArgs = script
	}

	feOutput, err := ss.remoteCmdExecutor.ExecuteWithNodeMap(nodeMap)
	if err != nil {
		return err
	}

	for ip, res := range feOutput {
		data := ss.getFEStatus(ip, res, serviceName)
		ss.feStatus = append(ss.feStatus, data)
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

func (ss *Summary) getBEStatus(outputs []*CmdResult, ip string, authToken, serviceName string) BeStatusValue {
	var memeberId, role string
	var err error
	cmdResMap := map[string]*CmdResult{}
	serviceState := initialServiceState
	servicePid := initialServicePid
	formattedDuration := initialFormattedDuration
	health := initialHealth
	defaultBeStatusValue := BeStatusValue{
		serviceName: serviceName,
		ipAddress:   ip,
		health:      initialHealth,
		process:     fmt.Sprintf("%s (pid: %s)", initialServiceState, initialServicePid),
		upTime:      initialFormattedDuration,
		role:        initialRole,
		lag:         initialLag,
	}

	for _, output := range outputs {
		cmdResMap[output.ScriptName] = output
	}

	memeberId, serviceState, servicePid, formattedDuration, err = ss.getBEDefaultServiceDetails(cmdResMap[defaultServiceDetails].Output)
	if cmdResMap[defaultServiceDetails].Error != nil || err != nil {
		return defaultBeStatusValue
	}
	health, err = ss.getBEServiceHealth(cmdResMap[defaultServiceHealthDetails].Output)
	if cmdResMap[defaultServiceHealthDetails].Error != nil || err != nil {
		return defaultBeStatusValue
	}
	role, err = ss.getBECensus(cmdResMap[censusDetails].Output, serviceName, memeberId)
	if cmdResMap[censusDetails].Error != nil || err != nil {
		defaultBeStatusValue.role = role
		return defaultBeStatusValue
	}

	lag, err := ss.getFollowerLag(cmdResMap[defaultServiceHealthDetails].Output, serviceName, role)
	if err != nil {
		lag = "Error"
	}

	return BeStatusValue{
		serviceName: serviceName,
		ipAddress:   ip,
		health:      health,
		process:     fmt.Sprintf("%s (pid: %s)", serviceState, servicePid),
		upTime:      formattedDuration,
		role:        role,
		lag:         lag,
	}
}

func (ss *Summary) getBEDefaultServiceDetails(output string) (string, string, string, string, error) {
	defaultServiceDetails, err := parseStringInToMapStringInterface(output)
	if err != nil {
		return "", initialServiceState, initialServicePid, initialFormattedDuration, err
	}
	memeberId := defaultServiceDetails["sys"].(map[string]interface{})["member_id"].(string)
	serviceState := defaultServiceDetails["process"].(map[string]interface{})["state"].(string)
	var servicePid string
	if defaultServiceDetails["process"].(map[string]interface{})["pid"] == nil {
		servicePid = "0"
	} else {
		servicePid = fmt.Sprintf("%d", int(defaultServiceDetails["process"].(map[string]interface{})["pid"].(float64)))
	}
	startingTime := defaultServiceDetails["process"].(map[string]interface{})["state_entered"].(float64)
	startingTime = float64(nowFunc().UTC().Unix()) - startingTime

	t := time.Unix(int64(startingTime), 0)
	duration := t.Sub(time.Unix(0, 0))

	// Format the duration as "1d 148h 43m 31s"
	formattedDuration := fmt.Sprintf("%dd %dh %dm %ds", int(duration.Hours())/24, int(duration.Hours())%24, int(duration.Minutes())%60, int(duration.Seconds())%60)

	return memeberId, serviceState, servicePid, formattedDuration, nil
}

func (ss *Summary) getBEServiceHealth(output string) (string, error) {
	ServiceHealth, err := parseStringInToMapStringInterface(output)
	if err != nil {
		return initialHealth, err
	}
	health := fmt.Sprint(ServiceHealth["status"])
	return health, nil
}

func (ss *Summary) getBECensus(output, service, memeberId string) (string, error) {
	role := initialRole
	if service == opensearchName {
		role = "OS node"
	}

	censusData, err := parseStringInToMapStringInterface(output)
	if err != nil {
		return "Unknown", err
	}

	populationData := censusData["census_groups"].(map[string]interface{})["automate-ha-"+service+".default"].(map[string]interface{})["population"].(map[string]interface{})[memeberId]
	getrole, ok := populationData.(map[string]interface{})
	if ok {
		isleader, _ := strconv.ParseBool(fmt.Sprint(getrole["leader"]))
		isfollower, _ := strconv.ParseBool(fmt.Sprint(getrole["follower"]))
		if isleader {
			role = "Leader"
		}
		if isfollower {
			role = "Follower"
		}
	}
	return role, nil
}

func (ss *Summary) getFollowerLag(output, service, role string) (string, error) {
	lag := initialLag
	outputMap, err := parseStringInToMapStringInterface(output)
	if err != nil {
		return lag, err
	}

	if service == postgresql {
		if role == "Follower" {
			// Define the regular expression pattern to match the substring
			pattern := `(\d+ bytes and \d+ seconds)`

			// Compile the regular expression pattern
			regExp := regexp.MustCompile(pattern)

			// Find the first matching substring of the stdout string
			lag = regExp.FindString(outputMap["stdout"].(string))

			return lag, nil
		}
	}
	return lag, nil
}

func (ss *Summary) getFEStatus(ip string, outputs []*CmdResult, serviceType string) FeStatusValue {
	var osStatus, status, ms string

	for _, output := range outputs {
		switch output.ScriptName {
		case "OsStatus":
			osStatus = ss.opensearchStatusInFE(output)
		case "Status":
			status = "OK"
			if output.Error != nil {
				if strings.Contains(output.Error.Error(), "97") {
					status = "WARN"
				} else {
					status = "ERROR"
				}
			}
		case "MaintStatus":
			ms = "false"
			parts := strings.Split(output.Output, "=")
			if len(parts) == 2 {
				val := strings.TrimSpace(parts[1])
				if val == "true" {
					ms = val
				}
			}
		}
	}

	feStatusValue := FeStatusValue{
		serviceName: serviceType,
		ipAddress:   ip,
		status:      status,
		Opensearch:  osStatus,
		MaintStatus: ms,
	}
	return feStatusValue
}

func (ss *Summary) opensearchStatusInFE(osStatusOutput *CmdResult) string {

	if osStatusOutput.Error != nil {
		return "Unknown"
	}

	body := []byte(osStatusOutput.Output)
	esHealthData := make(map[string]json.RawMessage)

	err := json.Unmarshal(body, &esHealthData)
	if err != nil {
		return "Unknown"
	}

	return fmt.Sprintf("%s (Active: %s)", esHealthData["status"], esHealthData["active_shards_percent_as_number"])
}

func parseStringInToMapStringInterface(output string) (map[string]interface{}, error) {
	var body map[string]interface{}
	err := json.Unmarshal([]byte(output), &body)
	if err != nil {
		return body, err
	}
	return body, nil
}

func (ss *Summary) splitIP(node string) (nodes []string) {
	nodes = strings.Split(node, ",")
	nodes = trimSliceSpace(nodes)
	return
}

func (ss *Summary) validateIPAddresses(errorList *list.List, IpsFromcmd []string, nodeType, errorMessage string) ([]string, []string, *list.List) {
	ips := ss.getNodeIPs(nodeType)
	var ipFound, ipNotFound []string

	for _, ip := range IpsFromcmd {
		err := checkIPAddress(ip)
		if err != nil {
			errorList.PushBack("Incorrect " + nodeType + " IP, " + ip + errorMessage)
		}
		if stringutils.SliceContains(ips, ip) {
			ipFound = append(ipFound, ip)
		} else {
			ipNotFound = append(ipNotFound, ip)
		}
	}
	return ipFound, ipNotFound, errorList
}
func (ss *Summary) getNodeIPs(NodeType string) []string {
	switch NodeType {
	case automateName:
		return ss.infra.Outputs.AutomatePrivateIps.Value
	case chefServerName:
		return ss.infra.Outputs.ChefServerPrivateIps.Value
	case opensearchName:
		return ss.infra.Outputs.OpensearchPrivateIps.Value
	case postgresqlName:
		return ss.infra.Outputs.PostgresqlPrivateIps.Value
	}
	return []string{}
}

// Display Frontend Status
func (ss *Summary) ShowFEStatus() string {
	if len(ss.feStatus) != 0 {
		writer.Println("Frontend Services")
		t := table.NewWriter()
		tTemp := table.Table{}
		tTemp.Render()
		for _, status := range ss.feStatus {
			t.AppendRow(table.Row{status.serviceName, status.ipAddress, status.status, status.Opensearch, status.MaintStatus})
		}
		t.AppendHeader(table.Row{"Name", "IP Address", "Status", "Opensearch", "Maintenance Mode"})
		return t.Render()
	}
	return ""
}

// Display Backend Status
func (ss *Summary) ShowBEStatus() string {
	if len(ss.beStatus) != 0 {
		writer.Println("Backend Services")
		t := table.NewWriter()
		tTemp := table.Table{}
		tTemp.Render()
		for _, status := range ss.beStatus {
			t.AppendRow(table.Row{status.serviceName, status.ipAddress, status.health, status.process, status.lag, status.upTime, status.role})
		}
		t.AppendHeader(table.Row{"Name", "IP Address", "Health", "Process", "Lag", "Uptime", "Role"})
		return t.Render()
	}
	return ""
}
func (ss *Summary) GetPGLeaderNode() (string, string) {
	if len(ss.beStatus) != 0 {
		for _, status := range ss.beStatus {
			if status.role == "Leader" && status.serviceName == "postgresql" {
				return status.ipAddress, status.health
			}
		}
	}
	return "", ""
}

func (ss *Summary) GetPGFollwerNodes() map[string]string {
	m := make(map[string]string)
	if len(ss.beStatus) != 0 {
		for _, status := range ss.beStatus {
			if status.role != "Leader" && status.serviceName == "postgresql" {
				m[status.ipAddress] = status.health
			}
		}
	}
	return m
}

func (ss *Summary) GetPGMaxLagAmongFollowers() int64 {
	var maxLag int64 = 0
	if len(ss.beStatus) != 0 {
		for _, status := range ss.beStatus {
			if status.role == "Follower" && status.serviceName == "postgresql" {
				re := regexp.MustCompile("[0-9]+")
				lagsNumberics := re.FindAllString(status.lag, -1)
				if len(lagsNumberics) > 0 {
					lag, _ := strconv.ParseInt(lagsNumberics[0], 10, 64)
					if lag > maxLag {
						maxLag = lag
					}
				}
			}
		}
	}
	return maxLag
}
