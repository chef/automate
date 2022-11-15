package main

import (
	"fmt"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/lib/stringutils"
	"github.com/spf13/cobra"
)

var addDeleteNodeHACmdFlags = struct {
	automateIp   string
	chefServerIp string
	opensearchIp string
	postgresqlIp string
	autoAccept   bool
	filepath     string
}{}

var existingIps struct {
	config          ExistingInfraConfigToml
	automateCerByIp []struct {
		IP         string `toml:"ip"`
		PrivateKey string `toml:"private_key"`
		PublicKey  string `toml:"public_key"`
	} `toml:"certs_by_ip"`
	chefServerCerByIp []struct {
		IP         string `toml:"ip"`
		PrivateKey string `toml:"private_key"`
		PublicKey  string `toml:"public_key"`
	} `toml:"certs_by_ip"`
	opensearchCertsByIP []struct {
		IP         string `toml:"ip"`
		PrivateKey string `toml:"private_key"`
		PublicKey  string `toml:"public_key"`
		NodesDn    string `toml:"nodes_dn"`
	} `toml:"certs_by_ip"`
	postgresqlCertsByIP []struct {
		IP         string `toml:"ip"`
		PrivateKey string `toml:"private_key"`
		PublicKey  string `toml:"public_key"`
	} `toml:"certs_by_ip"`
	automate        []string
	chefServer      []string
	opensearch      []string
	postgresql      []string
	automateCount   int
	chefServerCount int
	opensearchCount int
	postgresqlCount int
}

var nodeCmd = &cobra.Command{
	Use:    "node COMMAND",
	Short:  "This command is used to add or delete HA nodes",
	Hidden: false,
}

func addNodeHACmd() *cobra.Command {
	var addNodeHACmd = &cobra.Command{
		Use:   "add",
		Short: "Add new node in HA",
		Long:  `Add new node in HA`,
		RunE:  runAddNodeHACmd,
	}
	addNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.automateIp, "automate-ip", "", "a2-ip")
	addNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.chefServerIp, "chef-server-ip", "", "cs-ip")
	addNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.opensearchIp, "open-search-ip", "", "os-ip")
	addNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.postgresqlIp, "postgresql-ip", "", "pg-ip")
	addNodeHACmd.PersistentFlags().BoolVarP(&addDeleteNodeHACmdFlags.autoAccept, "", "y", false, "auto-accept")
	addNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.filepath, "filepath", "", "file-path")

	return addNodeHACmd
}

func deleteNodeHACmd() *cobra.Command {
	var deleteNodeHACmd = &cobra.Command{
		Use:   "delete",
		Short: "Delete existing node in HA",
		Long:  `Delete existing node in HA`,
		RunE:  runDeleteNodeHACmd,
	}
	deleteNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.automateIp, "automate-ip", "", "a2-ip")
	deleteNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.chefServerIp, "chef-server-ip", "", "cs-ip")
	deleteNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.opensearchIp, "open-search-ip", "", "os-ip")
	deleteNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.postgresqlIp, "postgresql-ip", "", "pg-ip")
	deleteNodeHACmd.PersistentFlags().BoolVarP(&addDeleteNodeHACmdFlags.autoAccept, "", "y", false, "auto-accept")
	deleteNodeHACmd.PersistentFlags().StringVar(&addDeleteNodeHACmdFlags.filepath, "filepath", "", "file-path")

	return deleteNodeHACmd
}

func runAddNodeHACmd(c *cobra.Command, args []string) error {
	var configPath = ""
	if len(args) > 0 {
		configPath = args[0]
	}
	var deployer, derr = getDeployer(configPath)
	if derr != nil {
		return status.Wrap(derr, status.ConfigError, invalidConfig)
	}
	if deployer != nil {

		// check deployment type AWS or ExistingInfra
		deployerType, err := getModeFromConfig(configPath)
		if err != nil {
			return err
		}
		if deployerType == EXISTING_INFRA_MODE {
			existingIps.config, err = readConfig(addDeleteNodeHACmdFlags.filepath)
			if err != nil {
				return err
			}
			existingIps.automate = existingIps.config.ExistingInfra.Config.AutomatePrivateIps
			existingIps.chefServer = existingIps.config.ExistingInfra.Config.ChefServerPrivateIps
			existingIps.opensearch = existingIps.config.ExistingInfra.Config.OpensearchPrivateIps
			existingIps.postgresql = existingIps.config.ExistingInfra.Config.PostgresqlPrivateIps
			existingIps.automateCerByIp = existingIps.config.Automate.Config.CertsByIP
			existingIps.chefServerCerByIp = existingIps.config.ChefServer.Config.CertsByIP
			existingIps.opensearchCertsByIP = existingIps.config.Opensearch.Config.CertsByIP
			existingIps.postgresqlCertsByIP = existingIps.config.Postgresql.Config.CertsByIP
		}
		// else if deployerType == AWS_MODE {
		// 	// aws := newAwsDeployemnt(configPath)
		// }

		// fmt.Println(existingIps.automateCerByIp)
		// fmt.Println(deployerType)

		// Split the ip address
		automateIpList := strings.Split(addDeleteNodeHACmdFlags.automateIp, ",")
		chefServerIpList := strings.Split(addDeleteNodeHACmdFlags.chefServerIp, ",")
		opensearchIpList := strings.Split(addDeleteNodeHACmdFlags.opensearchIp, ",")
		postgresqlPList := strings.Split(addDeleteNodeHACmdFlags.postgresqlIp, ",")

		if addDeleteNodeHACmdFlags.automateIp != "" {
			found, err := checkIpExistAndValidateIps(automateIpList, "automate")
			if err != nil {
				return err
			}
			fmt.Println(found)
		}

		if addDeleteNodeHACmdFlags.chefServerIp != "" {
			found, err := checkIpExistAndValidateIps(chefServerIpList, "chef-server")
			if err != nil {
				return err
			}
			fmt.Println(found)
		}

		if addDeleteNodeHACmdFlags.opensearchIp != "" {
			found, err := checkIpExistAndValidateIps(opensearchIpList, "opensearch")
			if err != nil {
				return err
			}
			fmt.Println(found)
		}

		if addDeleteNodeHACmdFlags.postgresqlIp != "" {
			found, err := checkIpExistAndValidateIps(postgresqlPList, "postgresql")
			if err != nil {
				return err
			}
			fmt.Println(found)
		}

		if addDeleteNodeHACmdFlags.automateIp != "" ||
			addDeleteNodeHACmdFlags.chefServerIp != "" ||
			addDeleteNodeHACmdFlags.opensearchIp != "" ||
			addDeleteNodeHACmdFlags.postgresqlIp != "" {
			err := promptCheckList(
				"do you want to continue ? [y/n]",
			)
			if err != nil {
				return err
			}

		}
	}
	return nil
}

func runDeleteNodeHACmd(c *cobra.Command, args []string) error {
	var configPath = ""
	if len(args) > 0 {
		configPath = args[0]
	}
	var deployer, derr = getDeployer(configPath)
	if derr != nil {
		return status.Wrap(derr, status.ConfigError, invalidConfig)
	}
	if deployer != nil {

		// check deployment type AWS or ExistingInfra
		deployerType, err := getModeFromConfig(configPath)
		if err != nil {
			return err
		}
		if deployerType == EXISTING_INFRA_MODE {
		}

	}
	return nil
}

func checkIpExistAndValidateIps(ips []string, arrayType string) (bool, error) {
	for _, ip := range ips {
		if stringutils.SliceContains(existingIps.automate, ip) ||
			stringutils.SliceContains(existingIps.chefServer, ip) ||
			stringutils.SliceContains(existingIps.opensearch, ip) ||
			stringutils.SliceContains(existingIps.postgresql, ip) {
			return true, nil
		}
		// check ip address is valid or not
		err := checkIPAddress(ip)
		if err != nil {
			return true, err
		}
		fmt.Println(len(existingIps.config.Automate.Config.CertsByIP), "hello")
		fmt.Println(existingIps.config.Automate.Config.EnableCustomCerts, "hello11")
		if arrayType == "automate" {
			existingIps.automate = append(existingIps.automate, ip)
			if existingIps.config.Automate.Config.EnableCustomCerts &&
				len(existingIps.config.Automate.Config.CertsByIP) > 0 {
			}
		}
		if arrayType == "chef-server" {
			existingIps.chefServer = append(existingIps.chefServer, ip)
		}
		if arrayType == "opensearch" {
			existingIps.opensearch = append(existingIps.opensearch, ip)
		}
		if arrayType == "postgresql" {
			existingIps.postgresql = append(existingIps.postgresql, ip)
		}
	}
	return false, nil
}
func init() {
	nodeCmd.AddCommand(addNodeHACmd())
	nodeCmd.AddCommand(deleteNodeHACmd())
	RootCmd.AddCommand(nodeCmd)
}
