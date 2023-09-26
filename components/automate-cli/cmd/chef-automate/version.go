// Copyright © 2017 Chef Software

package main

import (
	"container/list"
	"context"
	"errors"
	"fmt"
	"os"
	"regexp"
	"strings"

	"github.com/pelletier/go-toml"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/statusservice"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/automate/lib/version"
	"github.com/fatih/color"
)

var versionCmd = &cobra.Command{
	Use:   "version",
	Short: "Show CLI version",
	Long:  "Show the CLI version.",
	RunE:  runVersionCmd,
	Annotations: map[string]string{
		NoRequireRootAnnotation: NoRequireRootAnnotation,
		docs.Tag:                docs.BastionHost,
	},
}

type versionResult struct {
	ClientVersion   string `json:"client_version"`
	ClientGitSHA    string `json:"client_git_sha"`
	ManifestVersion string `json:"manifest_version"`
	ManifestGitSHA  string `json:"manifest_git_sha"`
}

var verbose bool

var VersionCommandFlags = struct {
	node         string
	verbose      bool
	isAutomate   bool
	isChefServer bool
	isOpenSearch bool
	isPostgresql bool
}{}

func runVersionCmd(cmd *cobra.Command, args []string) error {
	writer.Printf("Version: %s\n", "2")
	// Check for bastion
	if isA2HARBFileExist() {
		logrus.Debug("Running command on bastion")
		writer.Println(color.New(color.Bold).Add(color.Underline).Sprint(BASTION_NAME))
		writer.Println("\n")
		printClientVersion()
		err := runCommandOnBastion(args)

		if err != nil {
			writer.Println("Error while running version command on Bastion. Please check logs for more info")
			writer.Println("-----------------------------------------")
		}
		return nil
	}
	printClientVersion()
	return printServerVersion()
}

func printClientVersion() {
	// The client version is built in and hence always available.
	if verbose {
		writer.Title("CLI")
		writer.Bodyf("CLI Build: %s", version.BuildTime)
		writer.Bodyf("Git SHA: %s", version.GitSHA)
	} else {
		writer.Bodyf("CLI Build: %s", version.BuildTime)
	}
}

func printServerVersion() error {
	if os.Geteuid() != 0 {

		return status.New(
			status.MustBeRootError,
			"Server version cannot be listed because this command was not run as root. Re-run this command as root to see full server version information.",
		)
	}

	// Check for bastion
	if isA2HARBFileExist() {
		writer.Bodyf("Server Build : For getting chef-automate version, Please login to chef-automate and use command chef-automate version ")
		return nil
	}

	// Connect to the server to get the server version.
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}

	response, err := connection.ManifestVersion(context.Background(), &api.ManifestVersionRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request for Chef Automate package manifest failed",
		)
	}

	if verbose {
		writer.Title("Server")
		writer.Bodyf("Server Build: %s", response.BuildTimestamp)
		writer.Bodyf("Git SHA: %s", response.BuildSha)
		err := printUpgradeStatus()
		if err != nil {
			return err
		}
	} else {
		// get a server version
		writer.Bodyf("Server Build: %s", response.BuildTimestamp)
	}
	status.GlobalResult = versionResult{
		ClientVersion:   version.BuildTime,
		ClientGitSHA:    version.GitSHA,
		ManifestVersion: response.BuildTimestamp,
		ManifestGitSHA:  response.BuildSha,
	}
	return nil
}

func printUpgradeStatus() error {
	// Copy-pasta'ed from upgrade command.
	conn, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}
	resp, err := conn.UpgradeStatus(context.Background(), &api.UpgradeStatusRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to get upgrade status failed",
		)
	}

	upgradeStatus := ""

	switch resp.State {
	case api.UpgradeStatusResponse_IDLE:
		upgradeStatus = "up-to-date"
	case api.UpgradeStatusResponse_UPGRADING:
		upgradeStatus = "upgrading"
	case api.UpgradeStatusResponse_UNKNOWN:
		// I don't think we can get here without hitting the err != nil above first
		upgradeStatus = "could not be determined!"
	}
	writer.Bodyf("Upgrade status: %s (Run `chef-automate upgrade status` for more detail)\n", upgradeStatus)
	return nil
}

func init() {
	versionCmd.Flags().BoolVarP(&VersionCommandFlags.verbose, "verbose", "v", false, "Show additional version information")

	versionCmd.PersistentFlags().BoolVar(&VersionCommandFlags.isAutomate, "a2", false, "Get only automate Status")
	versionCmd.PersistentFlags().SetAnnotation("automate", docs.Compatibility, []string{docs.CompatiblewithHA})

	versionCmd.PersistentFlags().BoolVar(&VersionCommandFlags.isChefServer, "cs", false, "Get only chef server Status")
	versionCmd.PersistentFlags().SetAnnotation("chef-server", docs.Compatibility, []string{docs.CompatiblewithHA})

	versionCmd.PersistentFlags().BoolVar(&VersionCommandFlags.isOpenSearch, "os", false, "Get only opensearch Status")
	versionCmd.PersistentFlags().SetAnnotation("opensearch", docs.Compatibility, []string{docs.CompatiblewithHA})

	versionCmd.PersistentFlags().BoolVar(&VersionCommandFlags.isPostgresql, "pg", false, "Get only postgresql Status")
	versionCmd.PersistentFlags().SetAnnotation("postgresql", docs.Compatibility, []string{docs.CompatiblewithHA})

	versionCmd.PersistentFlags().StringVar(&VersionCommandFlags.node, "node", "", "Node Ip address. While using this flag, pass the node type as well. Example : chef-automate version --node 192.0.0.1 --cs")
	versionCmd.PersistentFlags().SetAnnotation("node", docs.Compatibility, []string{docs.CompatiblewithHA})
	RootCmd.AddCommand(versionCmd)
}

func runCommandOnBastion(args []string) error {

	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		logrus.Errorf("Error while getting HA Infra details :: %s", err)
		return err
	}
	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := getIPAddressesFromFlagOrInfra(infra)
	if errList != nil && errList.Len() > 0 {
		logrus.Errorf("Error while getting IP addresses :: %s", getSingleErrorFromList(errList))
		return getSingleErrorFromList(errList)
	}

	sshUtil := NewSSHUtil(&SSHConfig{})
	cmdExecutor := NewRemoteCmdExecutorWithoutNodeMap(sshUtil, writer)

	writer.Println("-----------------------------------------")

	if len(automateIps) != 0 {

		versions, err := getChefAutomateVersion(automateIps, infra, cmdExecutor)
		if err != nil {
			logrus.Errorf("Error while getting Automate Version :: %s", err)
			return err
		}
		writer.Println(color.New(color.Bold).Add(color.Underline).Sprint(AUTOMATE_NAME))
		writer.Println("\n")

		for ip, version := range versions {
			writer.Printf("Node IP : %s\n", ip)
			writer.Println(version)
		}
		writer.Println("-----------------------------------------")
	}

	if len(chefServerIps) != 0 {
		versions, err := getInfraServerVersion(chefServerIps, infra, cmdExecutor)
		if err != nil {
			logrus.Errorf("Error while getting Infra server Version :: %s", err)
			return err
		}
		writer.Println(color.New(color.Bold).Add(color.Underline).Sprint(CHEF_SERVER_NAME))
		writer.Println("\n")
		for ip, version := range versions {
			writer.Printf("Node IP : %s\n", ip)

			writer.Printf("Version : %s\n\n", version)
		}
		writer.Println("-----------------------------------------")
	}

	isManaged := isManagedServicesOn()

	if len(opensearchIps) != 0 {
		versions, err := getOpensearchVersion(opensearchIps, infra, isManaged, cmdExecutor)
		if err != nil {
			logrus.Errorf("Error while getting Opensearch Version :: %s", err)
			return err
		}
		writer.Println(color.New(color.Bold).Add(color.Underline).Sprint(OPENSEARCH_NAME))
		writer.Println("\n")
		for ip, version := range versions {
			writer.Printf("Node IP : %s\n", ip)

			writer.Printf("Version : %s\n\n", version)
		}
		writer.Println("-----------------------------------------")

	}

	if len(postgresqlIps) != 0 {
		versions, err := getPostgresqlVersion(postgresqlIps, infra, isManaged, cmdExecutor)
		if err != nil {
			logrus.Errorf("Error while getting Postgresql Version :: %s", err)
			return err
		}
		writer.Println(color.New(color.Bold).Add(color.Underline).Sprint(POSTGRESQL_NAME))
		writer.Println("\n")

		for ip, version := range versions {
			writer.Printf("Node IP : %s\n", ip)

			writer.Printf("Version : %s\n\n", version)
		}

		writer.Println("-----------------------------------------")

	}

	return nil
}

func getChefAutomateVersion(automateIps []string, infra *AutomateHAInfraDetails, cmdExecuter RemoteCmdExecutor) (map[string]string, error) {
	automateCmd := A2VERSIONCMD
	if VersionCommandFlags.verbose {
		automateCmd = A2VERSIONVERBOSE
	}
	nodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Automate: &Cmd{CmdInputs: &CmdInputs{
			Cmd:                      automateCmd,
			NodeIps:                  automateIps,
			NodeType:                 true,
			SkipPrintOutput:          true,
			HideSSHConnectionMessage: true}},
		ChefServer: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Infra:      infra,
	}

	cmdresult, err := cmdExecuter.ExecuteWithNodeMap(nodeMap)

	if err != nil {
		logrus.Error("ERROR", err)
		return nil, err
	}
	versionMap := make(map[string]string)
	for ip, result := range cmdresult {
		versionMap[ip] = result[0].Output
	}
	return versionMap, nil
}

func getInfraServerVersion(chefServerIps []string, infra *AutomateHAInfraDetails, cmdExecuter RemoteCmdExecutor) (map[string]string, error) {

	nodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Automate: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		ChefServer: &Cmd{CmdInputs: &CmdInputs{
			Cmd:                      CSVERSIONCMD,
			NodeIps:                  chefServerIps,
			NodeType:                 true,
			SkipPrintOutput:          true,
			HideSSHConnectionMessage: true}},
		Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Infra:      infra,
	}

	cmdresult, err := cmdExecuter.ExecuteWithNodeMap(nodeMap)

	if err != nil {
		logrus.Error("ERROR", err)
		return nil, err
	}

	versionMap := make(map[string]string)
	for ip, result := range cmdresult {

		v, err := extractVersion(result[0].Output, VERSIONREGEX)
		if err != nil {
			logrus.Error("ERROR", err)
			return nil, err
		}
		versionMap[ip] = v
	}
	return versionMap, nil
}

func getOpensearchVersion(opensearchIps []string, infra *AutomateHAInfraDetails, isManagedServicesOn bool, cmdExecuter RemoteCmdExecutor) (map[string]string, error) {
	versionMap := make(map[string]string)
	if isManagedServicesOn {
		automateIps := infra.Outputs.AutomatePrivateIps.Value
		nodeMap := &NodeTypeAndCmd{
			Frontend: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Automate: &Cmd{CmdInputs: &CmdInputs{
				Cmd:                      OSGETINFOCURLCMD,
				NodeIps:                  []string{automateIps[0]},
				NodeType:                 true,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true}},
			ChefServer: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Infra:      infra,
		}

		cmdresult, err := cmdExecuter.ExecuteWithNodeMap(nodeMap)

		if err != nil {
			logrus.Error("ERROR", err)
			return nil, err
		}

		var v string
		var err1 error
		for _, result := range cmdresult {
			v, err1 = extractVersion(result[0].Output, OSVERSIONREGEX)
			if err1 != nil {
				logrus.Error("ERROR", err)
				return nil, err1
			}
		}
		for _, ip := range opensearchIps {
			versionMap[ip] = v

		}

		return versionMap, nil
	} else {
		nodeMap := &NodeTypeAndCmd{
			Frontend:   &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Automate:   &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			ChefServer: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Opensearch: &Cmd{CmdInputs: &CmdInputs{
				Cmd:                      HABSVCSTATUS,
				NodeIps:                  opensearchIps,
				NodeType:                 true,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true}},
			Infra: infra,
		}

		cmdresult, err := cmdExecuter.ExecuteWithNodeMap(nodeMap)

		if err != nil {
			logrus.Error("ERROR", err)
			return nil, err
		}
		for ip, result := range cmdresult {
			servicedetails, err := statusservice.ParseHabSvcStatus(result[0].Output)
			if err != nil {
				fmt.Print("ERROR", err)
				return nil, err
			}
			osPkg := filterPackage(servicedetails, "automate-ha-opensearch")

			if osPkg == nil {
				logrus.Error("No os pkg found")
				return nil, errors.New("No os pkg found")
			}

			v, err := extractVersion(osPkg.Version, VERSIONREGEX)
			if err != nil {
				logrus.Error("ERROR", err)
				return nil, err
			}
			versionMap[ip] = v

		}

		return versionMap, nil

	}
}

func getPostgresqlVersion(postgresqlIps []string, infra *AutomateHAInfraDetails, isManagedServicesOn bool, cmdExecuter RemoteCmdExecutor) (map[string]string, error) {
	versionMap := make(map[string]string)

	if isManagedServicesOn {
		sshconfig := &SSHConfig{}
		sshconfig.sshUser = infra.Outputs.SSHUser.Value
		sshconfig.sshKeyFile = infra.Outputs.SSHKeyFile.Value
		sshconfig.sshPort = infra.Outputs.SSHPort.Value
		sshconfig.hostIP = infra.Outputs.AutomatePrivateIps.Value[0]
		sshUtil := NewSSHUtil(sshconfig)
		su, sp := getPgAuth(sshUtil)
		if su == "" || sp == "" {
			return nil, errors.New("Couldn't get Super user password and name from config")
		}
		automateIps := infra.Outputs.AutomatePrivateIps.Value
		pgCommand := fmt.Sprintf(PGGETVERSIONCURLCMD, sp, PGCOREPKG, su)
		nodeMap := &NodeTypeAndCmd{
			Frontend: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Automate: &Cmd{CmdInputs: &CmdInputs{
				Cmd:                      pgCommand,
				NodeIps:                  []string{automateIps[0]},
				NodeType:                 true,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true}},
			ChefServer: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Infra:      infra,
		}

		cmdresult, err := cmdExecuter.ExecuteWithNodeMap(nodeMap)

		if err != nil {
			logrus.Error("ERROR", err)
			return nil, err
		}

		var v string
		var err1 error
		for _, result := range cmdresult {
			v, err1 = extractVersion(result[0].Output, PGVERSIONREGEX)
			if err1 != nil {
				logrus.Error("ERROR", err)
				return nil, err1
			}
		}

		for _, ip := range postgresqlIps {
			versionMap[ip] = v
		}

		return versionMap, nil
	} else {
		nodeMap := &NodeTypeAndCmd{
			Frontend:   &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Automate:   &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			ChefServer: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Postgresql: &Cmd{CmdInputs: &CmdInputs{
				Cmd:                      HABSVCSTATUS,
				NodeIps:                  postgresqlIps,
				NodeType:                 true,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true}},
			Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Infra:      infra,
		}

		cmdresult, err := cmdExecuter.ExecuteWithNodeMap(nodeMap)

		if err != nil {
			logrus.Error("ERROR", err)
			return nil, err
		}
		for ip, result := range cmdresult {
			servicedetails, err := statusservice.ParseHabSvcStatus(result[0].Output)
			if err != nil {
				fmt.Print("ERROR", err)
				return nil, err
			}
			pgPkg := filterPackage(servicedetails, "automate-ha-postgresql")

			if pgPkg == nil {
				logrus.Error("No pg pkg found")
				return nil, errors.New("No pg pkg found")
			}
			v, err := extractVersion(pgPkg.Version, VERSIONREGEX)
			if err != nil {
				logrus.Error("ERROR", err)
				return nil, err
			}
			versionMap[ip] = v

		}

		return versionMap, nil

	}
}

func getPgAuth(sshUtil SSHUtil) (string, string) {
	output, err := sshUtil.connectAndExecuteCommandOnRemote(CONFIGSHOW, true)
	if err != nil {
		logrus.Error("Error in config show", err)
		return "", ""
	}
	config, err := toml.Load(output)
	if err != nil {
		logrus.Error("Error in parsing config", err)
		return "", ""
	}
	// retrieve data directly
	superUser := config.Get("global.v1.external.postgresql.auth.password.superuser").(*toml.Tree)
	if superUser != nil {
		userName := superUser.Get("username").(string)
		password := superUser.Get("password").(string)
		return userName, password
	}

	return "", ""
}

func filterPackage(services *[]models.ServiceDetails, packageName string) *models.ServiceDetails {
	for _, service := range *services {
		if strings.HasPrefix(service.ServiceName, packageName) {
			return &service
		}
	}
	return nil
}

func getIPAddressesFromFlagOrInfra(infra *AutomateHAInfraDetails) ([]string, []string, []string, []string, *list.List) {
	errorList := list.New()
	if VersionCommandFlags.node != "" {
		automateIps, chefServerIps, opensearchIps, postgresqlIps, errorList := getIpAddressesFromFlag(errorList, infra)
		return automateIps, chefServerIps, opensearchIps, postgresqlIps, errorList
	}
	return getIpAddressesFromInfra(errorList, infra)
}

func getIpAddressesFromFlag(errorList *list.List, infra *AutomateHAInfraDetails) ([]string, []string, []string, []string, *list.List) {
	var automateIps, chefServerIps, opensearchIps, postgresqlIps, nodes []string
	if VersionCommandFlags.isAutomate ||
		VersionCommandFlags.isChefServer ||
		VersionCommandFlags.isOpenSearch ||
		VersionCommandFlags.isPostgresql {
		nodes = splitIP(
			VersionCommandFlags.node,
		)
		if len(nodes) != 0 {
			if VersionCommandFlags.isAutomate {
				automateIps, nodes, errorList = validateIPAddresses(errorList, nodes, automateName, ipAddressError, infra)
			}
			if VersionCommandFlags.isChefServer {
				chefServerIps, nodes, errorList = validateIPAddresses(errorList, nodes, chefServerName, ipAddressError, infra)
			}
			if VersionCommandFlags.isOpenSearch {
				opensearchIps, nodes, errorList = validateIPAddresses(errorList, nodes, opensearchName, ipAddressError, infra)
			}
			if VersionCommandFlags.isPostgresql {
				postgresqlIps, nodes, errorList = validateIPAddresses(errorList, nodes, postgresqlName, ipAddressError, infra)
			}
			if len(nodes) != 0 && errorList.Len() == 0 {
				if VersionCommandFlags.isAutomate ||
					VersionCommandFlags.isChefServer ||
					VersionCommandFlags.isOpenSearch ||
					VersionCommandFlags.isPostgresql {
					services := []string{}
					if VersionCommandFlags.isAutomate {
						services = append(services, AUTOMATE_NAME)
					}
					if VersionCommandFlags.isChefServer {
						services = append(services, CHEF_SERVER_NAME)
					}
					if VersionCommandFlags.isOpenSearch {
						services = append(services, OPENSEARCH_NAME)
					}
					if VersionCommandFlags.isPostgresql {
						services = append(services, POSTGRESQL_NAME)
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

func getIpAddressesFromInfra(errorList *list.List, infra *AutomateHAInfraDetails) ([]string, []string, []string, []string, *list.List) {
	var automateIps, chefServerIps, opensearchIps, postgresqlIps []string

	if VersionCommandFlags.isAutomate {
		automateIps = infra.Outputs.AutomatePrivateIps.Value
	}
	if VersionCommandFlags.isChefServer {
		chefServerIps = infra.Outputs.ChefServerPrivateIps.Value
	}
	if VersionCommandFlags.isOpenSearch {
		opensearchIps = infra.Outputs.OpensearchPrivateIps.Value
	}
	if VersionCommandFlags.isPostgresql {
		postgresqlIps = infra.Outputs.PostgresqlPrivateIps.Value
	}
	if !VersionCommandFlags.isAutomate &&
		!VersionCommandFlags.isChefServer &&
		!VersionCommandFlags.isOpenSearch &&
		!VersionCommandFlags.isPostgresql {
		automateIps = infra.Outputs.AutomatePrivateIps.Value
		chefServerIps = infra.Outputs.ChefServerPrivateIps.Value
		opensearchIps = infra.Outputs.OpensearchPrivateIps.Value
		postgresqlIps = infra.Outputs.PostgresqlPrivateIps.Value
	}
	return automateIps, chefServerIps, opensearchIps, postgresqlIps, errorList
}

func splitIP(node string) (nodes []string) {
	nodes = strings.Split(node, ",")
	nodes = trimSliceSpace(nodes)
	return
}

func validateIPAddresses(errorList *list.List, IpsFromcmd []string, nodeType, errorMessage string, infra *AutomateHAInfraDetails) ([]string, []string, *list.List) {
	ips, err := getNodeIPs(false, "", infra, nodeType)
	if err != nil {
		errorList.PushBack("Error while getting node ips" + err.Error())
	}
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

func extractVersion(input string, pattern string) (string, error) {

	regex, err := regexp.Compile(pattern)
	if err != nil {
		return "", err
	}

	// Find the match
	match := regex.FindStringSubmatch(input)
	if len(match) < 2 {
		return "", fmt.Errorf("no version string found")
	}

	// Extract the version string
	version := match[1]
	return version, nil
}
