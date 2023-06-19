// Copyright © 2017 Chef Software

package main

import (
	"container/list"
	"context"
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
	"github.com/ttacon/chalk"
)

var versionCmd = &cobra.Command{
	Use:   "version",
	Short: "Show CLI version",
	Long:  "Show the CLI version.",
	RunE:  runVersionCmd,
	Annotations: map[string]string{
		NoRequireRootAnnotation: NoRequireRootAnnotation,
		docs.Tag:                docs.FrontEnd,
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
	// Check for bastion
	if isA2HARBFileExist() {
		logrus.Debug("Running command on bastion")
		runCommandOnBastion(args)
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

	versionCmd.PersistentFlags().StringVar(&VersionCommandFlags.node, "node", "", "Node Ip address")
	versionCmd.PersistentFlags().SetAnnotation("node", docs.Compatibility, []string{docs.CompatiblewithHA})
	RootCmd.AddCommand(versionCmd)
}

func runCommandOnBastion(args []string) error {

	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return err
	}
	automateIps, chefServerIps, opensearchIps, postgresqlIps, errList := getIPAddressesFromFlagOrInfra(infra)
	if errList != nil && errList.Len() > 0 {
		return status.Wrap(getSingleErrorFromList(errList), status.InvalidCommandArgsError, ipAddressError)
	}

	if len(automateIps) != 0 {
		err := getChefAutomateVersion(automateIps, infra)
		if err != nil {
			return err
		}
	}

	if len(chefServerIps) != 0 {
		err := getInfraServerVersion(chefServerIps, infra)
		if err != nil {
			return err
		}
	}

	if len(opensearchIps) != 0 {
		err := getOpensearchVersion(opensearchIps, infra)
		if err != nil {
			return err
		}
	}

	if len(postgresqlIps) != 0 {
		err := getPostgresqlVersion(postgresqlIps, infra)
		if err != nil {
			return err
		}
	}

	return nil
}

func getChefAutomateVersion(automateIps []string, infra *AutomateHAInfraDetails) error {
	nodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Automate: &Cmd{CmdInputs: &CmdInputs{
			Cmd:                      "sudo chef-automate version",
			NodeIps:                  automateIps,
			NodeType:                 true,
			SkipPrintOutput:          true,
			HideSSHConnectionMessage: true}},
		ChefServer: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Infra:      infra,
	}

	sshUtil := NewSSHUtil(&SSHConfig{})
	cmdUtil := NewRemoteCmdExecutor(nodeMap, sshUtil, writer)

	cmdresult, err := cmdUtil.Execute()

	if err != nil {
		logrus.Error("ERROR", err)
		return err
	}

	writer.Println("-----------------------------------------")
	writer.Println(chalk.Bold.TextStyle(chalk.Underline.TextStyle("Automate")))
	writer.Println("\n")

	for ip, result := range cmdresult {
		writer.Printf("Node IP : %s\n", ip)
		writer.Println(result[0].Output)

	}
	return nil
}

func getInfraServerVersion(chefServerIps []string, infra *AutomateHAInfraDetails) error {

	nodeMap := &NodeTypeAndCmd{
		Frontend: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Automate: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		ChefServer: &Cmd{CmdInputs: &CmdInputs{
			Cmd:                      "sudo chef-server-ctl version",
			NodeIps:                  chefServerIps,
			NodeType:                 true,
			SkipPrintOutput:          true,
			HideSSHConnectionMessage: true}},
		Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
		Infra:      infra,
	}

	sshUtil := NewSSHUtil(&SSHConfig{})
	cmdUtil := NewRemoteCmdExecutor(nodeMap, sshUtil, writer)

	cmdresult, err := cmdUtil.Execute()

	if err != nil {
		logrus.Error("ERROR", err)
		return err
	}
	writer.Println("-----------------------------------------")
	writer.Println(chalk.Bold.TextStyle(chalk.Underline.TextStyle("Chef Server")))
	writer.Println("\n")

	for ip, result := range cmdresult {

		v, err := extractVersion(result[0].Output, `(\d+\.\d+\.\d+)`)
		if err != nil {
			logrus.Error("ERROR", err)
			return err
		}
		writer.Printf("Node IP : %s\n", ip)

		writer.Printf("Version : %s\n\n", v)
	}
	return nil
}

func getOpensearchVersion(opensearchIps []string, infra *AutomateHAInfraDetails) error {
	if isManagedServicesOn() {
		automateIps := infra.Outputs.AutomatePrivateIps.Value
		nodeMap := &NodeTypeAndCmd{
			Frontend: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Automate: &Cmd{CmdInputs: &CmdInputs{
				Cmd:                      "curl -XGET http://localhost:10144",
				NodeIps:                  []string{automateIps[0]},
				NodeType:                 true,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true}},
			ChefServer: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Infra:      infra,
		}

		sshUtil := NewSSHUtil(&SSHConfig{})
		cmdUtil := NewRemoteCmdExecutor(nodeMap, sshUtil, writer)

		cmdresult, err := cmdUtil.Execute()

		if err != nil {
			logrus.Error("ERROR", err)
			return err
		}

		writer.Println("-----------------------------------------")
		writer.Println(chalk.Bold.TextStyle(chalk.Underline.TextStyle("Opensearch")))
		writer.Println("\n")

		var v string
		var err1 error
		for _, result := range cmdresult {
			v, err1 = extractVersion(result[0].Output, `(\d+\.\d+\.\d+)`)
			if err1 != nil {
				logrus.Error("ERROR", err)
				return err1
			}
		}

		for _, ip := range opensearchIps {
			writer.Printf("Node IP : %s\n", ip)

			writer.Printf("Version : %s\n\n", v)

		}

		return nil
	} else {
		nodeMap := &NodeTypeAndCmd{
			Frontend:   &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Automate:   &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			ChefServer: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Postgresql: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Opensearch: &Cmd{CmdInputs: &CmdInputs{
				Cmd:                      "echo yes |sudo hab svc status",
				NodeIps:                  opensearchIps,
				NodeType:                 true,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true}},
			Infra: infra,
		}

		sshUtil := NewSSHUtil(&SSHConfig{})
		cmdUtil := NewRemoteCmdExecutor(nodeMap, sshUtil, writer)

		cmdresult, err := cmdUtil.Execute()

		if err != nil {
			logrus.Error("ERROR", err)
			return err
		}
		writer.Println("-----------------------------------------")
		writer.Println(chalk.Bold.TextStyle(chalk.Underline.TextStyle("Opensearch")))
		writer.Println("\n")
		for ip, result := range cmdresult {
			servicedetails, err := statusservice.ParseHabSvcStatus(result[0].Output)
			if err != nil {
				fmt.Print("ERROR", err)
				return err
			}
			osPkg := filterPackage(servicedetails, "automate-ha-opensearch")

			if osPkg == nil {
				fmt.Print("No os pkg found")
				return nil
			}

			v, err := extractVersion(osPkg.Version, `(\d+\.\d+\.\d+)`)
			if err != nil {
				logrus.Error("ERROR", err)
				return err
			}
			writer.Printf("Node IP : %s\n", ip)

			writer.Printf("Version : %s\n\n", v)

		}

		return nil

	}
}

func getPostgresqlVersion(postgresqlIps []string, infra *AutomateHAInfraDetails) error {

	if isManagedServicesOn() {
		su, sp := getPgAuth(infra)
		automateIps := infra.Outputs.AutomatePrivateIps.Value
		pgCommand := fmt.Sprintf("PGPASSWORD=%s  hab pkg exec core/postgresql13  psql -U %s -h localhost -p 10145 -d postgres --dbname postgres -tAc 'SELECT version()'", sp, su)
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

		sshUtil := NewSSHUtil(&SSHConfig{})
		cmdUtil := NewRemoteCmdExecutor(nodeMap, sshUtil, writer)

		cmdresult, err := cmdUtil.Execute()

		if err != nil {
			logrus.Error("ERROR", err)
			return err
		}
		writer.Println("-----------------------------------------")
		writer.Println(chalk.Bold.TextStyle(chalk.Underline.TextStyle("Postgresql")))
		writer.Println("\n")

		var v string
		var err1 error
		for _, result := range cmdresult {
			v, err1 = extractVersion(result[0].Output, `PostgreSQL (\d+\.\d+)`)
			if err1 != nil {
				logrus.Error("ERROR", err)
				return err1
			}
		}

		for _, ip := range postgresqlIps {
			writer.Printf("Node IP : %s\n", ip)

			writer.Printf("Version : %s\n\n", v)
		}

		return nil
	} else {
		nodeMap := &NodeTypeAndCmd{
			Frontend:   &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Automate:   &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			ChefServer: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Postgresql: &Cmd{CmdInputs: &CmdInputs{
				Cmd:                      "echo yes |sudo hab svc status",
				NodeIps:                  postgresqlIps,
				NodeType:                 true,
				SkipPrintOutput:          true,
				HideSSHConnectionMessage: true}},
			Opensearch: &Cmd{CmdInputs: &CmdInputs{NodeType: false}},
			Infra:      infra,
		}

		sshUtil := NewSSHUtil(&SSHConfig{})
		cmdUtil := NewRemoteCmdExecutor(nodeMap, sshUtil, writer)

		cmdresult, err := cmdUtil.Execute()

		if err != nil {
			logrus.Error("ERROR", err)
			return err
		}
		writer.Println("-----------------------------------------")
		writer.Println(chalk.Bold.TextStyle(chalk.Underline.TextStyle("Postgresql")))
		writer.Println("\n")
		for ip, result := range cmdresult {
			servicedetails, err := statusservice.ParseHabSvcStatus(result[0].Output)
			if err != nil {
				fmt.Print("ERROR", err)
				return err
			}
			pgPkg := filterPackage(servicedetails, "automate-ha-postgresql")

			if pgPkg == nil {
				fmt.Print("No os pkg found")
				return nil
			}
			v, err := extractVersion(pgPkg.Version, `(\d+\.\d+\.\d+)`)
			if err != nil {
				logrus.Error("ERROR", err)
				return err
			}
			writer.Printf("Node IP : %s\n", ip)

			writer.Printf("Version : %s\n\n", v)

		}

		return nil

	}
}

func getPgAuth(infra *AutomateHAInfraDetails) (string, string) {
	sshconfig := &SSHConfig{}
	sshconfig.sshUser = infra.Outputs.SSHUser.Value
	sshconfig.sshKeyFile = infra.Outputs.SSHKeyFile.Value
	sshconfig.sshPort = infra.Outputs.SSHPort.Value
	sshconfig.hostIP = infra.Outputs.AutomatePrivateIps.Value[0]
	sshUtil := NewSSHUtil(sshconfig)
	output, err := sshUtil.connectAndExecuteCommandOnRemote("sudo chef-automate config show", true)
	if err != nil {
		logrus.Error("Error in config show", err)
	}
	config, _ := toml.Load(output)
	// retrieve data directly
	superUser := config.Get("global.v1.external.postgresql.auth.password.superuser").(*toml.Tree)
	userName := superUser.Get("username").(string)
	password := superUser.Get("password").(string)

	return userName, password
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
						services = append(services, "Automate")
					}
					if VersionCommandFlags.isChefServer {
						services = append(services, "Chef server")
					}
					if VersionCommandFlags.isOpenSearch {
						services = append(services, "Opensearch")
					}
					if VersionCommandFlags.isPostgresql {
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
		errorList.PushBack("Error while getting node ips")
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
