package main

import (
	"bytes"
	"reflect"
	"regexp"
	"strings"
	"time"

	"fmt"
	"os"
	"path/filepath"

	"github.com/fatih/color"
	ptoml "github.com/pelletier/go-toml"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/lib/config/genconfig"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/pmt"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/toml"
	"github.com/imdario/mergo"
)

var configCmdFlags = struct {
	overwriteFile bool
	timeout       int64
	waitTimeout   int
	acceptMLSA    bool

	automate         bool
	chef_server      bool
	frontend         bool
	opensearch       bool
	postgresql       bool
	getAppliedConfig bool
	file             string
}{}

const (
	postgresql       = "postgresql"
	opensearch_const = "opensearch"
	PRODUCT_WARNING  = `Ignored 'products' from [deployment.v1.svc]`
	CERT_WARNING     = `Ignored values from [%s]
	Please use 'cert-rotate' command to rotate certificate. 
	For more information, run: 'chef-automate cert-rotate --help'`
	ERROR_SELF_MANAGED_CONFIG_SHOW  = "Showing the configuration for externally configured %s is not supported."
	ERROR_SELF_MANAGED_CONFIG_PATCH = "Patching the configuration for externally configured %s is not supported."
	ERROR_SELF_MANAGED_CONFIG_SET   = "Setting the configuration for externally configured %s is not supported."
	patching                        = "Config Patch"
	setting                         = "Config Set"
	waitTimeout                     = "wait-timeout"
	automateHaPath                  = "/hab/var/automate-ha"
)

var configValid = "Config file must be a valid %s config"

func init() {
	configCmd.AddCommand(showConfigCmd)
	configCmd.AddCommand(patchConfigCmd)
	configCmd.AddCommand(setConfigCmd)
	configCmd.AddCommand(genConfigCmd)

	//config gen flags
	genConfigCmd.Flags().BoolVarP(&configCmdFlags.overwriteFile, "overwrite", "O", false, "Overwrite existing config.toml")
	configCmd.AddCommand(ocIdShowAppCmd)

	//config show flags
	showConfigCmd.Flags().BoolVarP(&configCmdFlags.overwriteFile, "overwrite", "O", false, "Overwrite existing config.toml [Standalone]")
	showConfigCmd.PersistentFlags().SetAnnotation("overwrite", docs.Compatibility, []string{docs.CompatiblewithStandalone})
	showConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.automate, "automate", "a", false, "Shows configurations from Automate node(HA)")
	showConfigCmd.PersistentFlags().SetAnnotation("automate", docs.Compatibility, []string{docs.CompatiblewithHA})
	showConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.chef_server, "chef_server", "c", false, "Shows configurations from Chef-server node(HA)")
	showConfigCmd.PersistentFlags().SetAnnotation("chef_server", docs.Compatibility, []string{docs.CompatiblewithHA})
	showConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.postgresql, "postgresql", "p", false, "Shows configurations from PostgresQL node")
	showConfigCmd.PersistentFlags().SetAnnotation("postgresql", docs.Compatibility, []string{docs.CompatiblewithHA})
	showConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.opensearch, "opensearch", "o", false, "Shows configurations from OpenSearch node")
	showConfigCmd.PersistentFlags().SetAnnotation("opensearch", docs.Compatibility, []string{docs.CompatiblewithHA})
	showConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.automate, "a2", false, "Shows configurations from Automate node(HA)[DUPLICATE]")
	showConfigCmd.PersistentFlags().SetAnnotation("a2", docs.Compatibility, []string{docs.CompatiblewithHA})
	showConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.chef_server, "cs", false, "Shows configurations from Chef-server node(HA)[DUPLICATE]")
	showConfigCmd.PersistentFlags().SetAnnotation("cs", docs.Compatibility, []string{docs.CompatiblewithHA})
	showConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.postgresql, "pg", false, "Shows configurations from PostgresQL node[DUPLICATE]")
	showConfigCmd.PersistentFlags().SetAnnotation("pg", docs.Compatibility, []string{docs.CompatiblewithHA})
	showConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.opensearch, "os", false, "Shows configurations from OpenSearch node[DUPLICATE]")
	showConfigCmd.PersistentFlags().SetAnnotation("os", docs.Compatibility, []string{docs.CompatiblewithHA})
	showConfigCmd.PersistentFlags().IntVar(&configCmdFlags.waitTimeout, waitTimeout, DEFAULT_TIMEOUT, "This flag sets the operation timeout duration (in seconds) for each individual node during the config show process")
	showConfigCmd.PersistentFlags().SetAnnotation(waitTimeout, docs.Compatibility, []string{docs.CompatiblewithHA})

	// config patch flags
	patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.frontend, "frontend", "f", false, "Patch toml configuration to the all frontend nodes")
	patchConfigCmd.PersistentFlags().SetAnnotation("frontend", docs.Compatibility, []string{docs.CompatiblewithHA})
	patchConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.frontend, "fe", false, "Patch toml configuration to the all frontend nodes[DUPLICATE]")
	patchConfigCmd.PersistentFlags().SetAnnotation("fe", docs.Compatibility, []string{docs.CompatiblewithHA})
	patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.automate, "automate", "a", false, "Patch toml configuration to the automate node")
	patchConfigCmd.PersistentFlags().SetAnnotation("automate", docs.Compatibility, []string{docs.CompatiblewithHA})
	patchConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.automate, "a2", false, "Patch toml configuration to the automate node[DUPLICATE]")
	patchConfigCmd.PersistentFlags().SetAnnotation("a2", docs.Compatibility, []string{docs.CompatiblewithHA})
	patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.chef_server, "chef_server", "c", false, "Patch toml configuration to the chef_server node")
	patchConfigCmd.PersistentFlags().SetAnnotation("chef_server", docs.Compatibility, []string{docs.CompatiblewithHA})
	patchConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.chef_server, "cs", false, "Patch toml configuration to the chef_server node[DUPLICATE]")
	patchConfigCmd.PersistentFlags().SetAnnotation("cs", docs.Compatibility, []string{docs.CompatiblewithHA})
	patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.opensearch, "opensearch", "o", false, "Patch toml configuration to the opensearch node")
	patchConfigCmd.PersistentFlags().SetAnnotation("opensearch", docs.Compatibility, []string{docs.CompatiblewithHA})
	patchConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.opensearch, "os", false, "Patch toml configuration to the opensearch node[DUPLICATE]")
	patchConfigCmd.PersistentFlags().SetAnnotation("os", docs.Compatibility, []string{docs.CompatiblewithHA})
	patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.postgresql, "postgresql", "p", false, "Patch toml configuration to the postgresql node")
	patchConfigCmd.PersistentFlags().SetAnnotation("postgresql", docs.Compatibility, []string{docs.CompatiblewithHA})
	patchConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.postgresql, "pg", false, "Patch toml configuration to the postgresql node[DUPLICATE]")
	patchConfigCmd.PersistentFlags().SetAnnotation("pg", docs.Compatibility, []string{docs.CompatiblewithHA})
	patchConfigCmd.PersistentFlags().IntVar(&configCmdFlags.waitTimeout, waitTimeout, DEFAULT_TIMEOUT, "This flag sets the operation timeout duration (in seconds) for each individual node during the config patch process")
	patchConfigCmd.PersistentFlags().SetAnnotation(waitTimeout, docs.Compatibility, []string{docs.CompatiblewithHA})

	// config set flags
	setConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.automate, "automate", "a", false, "Set toml configuration to the automate node")
	setConfigCmd.PersistentFlags().SetAnnotation("automate", docs.Compatibility, []string{docs.CompatiblewithHA})
	setConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.automate, "a2", false, "Set toml configuration to the automate node[DUPLICATE]")
	setConfigCmd.PersistentFlags().SetAnnotation("a2", docs.Compatibility, []string{docs.CompatiblewithHA})
	setConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.chef_server, "chef_server", "c", false, "Set toml configuration to the chef_server node")
	setConfigCmd.PersistentFlags().SetAnnotation("chef_server", docs.Compatibility, []string{docs.CompatiblewithHA})
	setConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.chef_server, "cs", false, "Set toml configuration to the chef_server node[DUPLICATE]")
	setConfigCmd.PersistentFlags().SetAnnotation("cs", docs.Compatibility, []string{docs.CompatiblewithHA})
	setConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.opensearch, "opensearch", "o", false, "Set toml configuration to the opensearch node")
	setConfigCmd.PersistentFlags().SetAnnotation("opensearch", docs.Compatibility, []string{docs.CompatiblewithHA})
	setConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.opensearch, "os", false, "Set toml configuration to the opensearch node[DUPLICATE]")
	setConfigCmd.PersistentFlags().SetAnnotation("os", docs.Compatibility, []string{docs.CompatiblewithHA})
	setConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.postgresql, "postgresql", "p", false, "Set toml configuration to the postgresql node")
	setConfigCmd.PersistentFlags().SetAnnotation("postgresql", docs.Compatibility, []string{docs.CompatiblewithHA})
	setConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.postgresql, "pg", false, "Set toml configuration to the postgresql node[DUPLICATE]")
	setConfigCmd.PersistentFlags().SetAnnotation("pg", docs.Compatibility, []string{docs.CompatiblewithHA})

	//config oc-id-show-app flags
	ocIdShowAppCmd.PersistentFlags().IntVar(&configCmdFlags.waitTimeout, waitTimeout, DEFAULT_TIMEOUT, "This flag sets the operation timeout duration (in seconds) for each individual node during the config oc-id-show-app process")
	ocIdShowAppCmd.PersistentFlags().SetAnnotation(waitTimeout, docs.Compatibility, []string{docs.CompatiblewithStandalone, docs.CompatiblewithHA})

	configCmd.PersistentFlags().BoolVarP(&configCmdFlags.acceptMLSA, "auto-approve", "y", false, "Do not prompt for confirmation; accept defaults and continue")
	configCmd.PersistentFlags().Int64VarP(&configCmdFlags.timeout, "timeout", "t", 10, "Request timeout in seconds")
	configCmd.PersistentFlags().SetAnnotation("timeout", docs.Compatibility, []string{docs.CompatiblewithStandalone})

	RootCmd.AddCommand(configCmd)
}

var configCmd = &cobra.Command{
	Use:   "config COMMAND",
	Short: "Chef Automate configuration",
}

var showConfigCmd = &cobra.Command{
	Use:   "show [/path/to/write/config.toml]",
	Short: "show the Chef Automate configuration",
	Long:  "Show the Chef Automate configuration. When given a filepath, the output will be written to the file instead of printed to STDOUT",
	RunE:  runShowCmd,
	Args:  cobra.RangeArgs(0, 2),
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

var genConfigCmd = &cobra.Command{
	Use:   "gen [/path/to/write/config.toml]",
	Short: "generate the Automate HA configuration",
	Long:  "Prompt based Config Generation command. It will output the config in the provided file, if file path is not provided then it will print on STDOUT.",
	RunE:  runGenCmd,
	Args:  cobra.RangeArgs(0, 2),
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

var patchConfigCmd = &cobra.Command{
	Use:   "patch path/to/config.toml",
	Short: "patch the Chef Automate configuration", Long: "Apply a partial Chef Automate configuration to the deployment. It will take the partial configuration, merge it with the existing configuration, and apply and required changes.",
	RunE: runPatchCommand,
	Args: cobra.ExactArgs(1),
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

var setConfigCmd = &cobra.Command{
	Use:   "set path/to/config.toml",
	Short: "set the Chef Automate configuration",
	Long:  "Set the Chef Automate configuration for the deployment. It will replace the Chef Automate configuration with the given configuration and apply any required changes.",
	RunE:  runSetCommand,
	Args:  cobra.ExactArgs(1),
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

func runGenCmd(cmd *cobra.Command, args []string) error {
	fsu := fileutils.NewFileSystemUtils()
	p := pmt.PromptFactory(os.Stdin, os.Stdout, fsu)
	g := genconfig.GenConfigImpFactory(p)

	err := g.GenConfigWithPrompts()
	if err != nil {
		return status.Wrap(
			err,
			status.FailedToGenConfig,
			"Failed to generate config with given input values to the prompts.",
		)
	}
	t, err := g.Toml()
	if err != nil {
		return status.Wrap(
			err,
			status.MarshalError,
			"Marshaling configuration to TOML failed",
		)
	}

	// Handle writing to a file if a path was given
	if len(args) > 0 && args[0] != "" {
		outFile, err := filepath.Abs(args[0])
		if err != nil {
			return status.Annotate(err, status.FileAccessError)
		}

		err = checkConfigGenFileExist(outFile, fsu)
		if err != nil {
			return err
		}
		err = fsu.WriteFile(outFile, t, 0644)
		if err != nil {
			return status.Wrap(err, status.FileAccessError, fmt.Sprint("Failed to write to file: ", outFile))
		}
		fmt.Println("\nConfig generated successfully!")
		fmt.Println("\nUse cat " + outFile + " command to see if the file is created")
		fmt.Println("After checking the config file, you can follow next steps:")
		fmt.Println("\ta. For AWS deployment, Provision the infra with " +
			color.New(color.Bold).Add(color.Italic).Sprint("`chef-automate provision-infra`") +
			" command and then proceed to deployment with " +
			color.New(color.Bold).Add(color.Italic).Sprint("`chef-automate deploy`") + " command")
		fmt.Println("\tb. For On-premise deployment, Proceed to deployment with " + color.New(color.Bold).Add(color.Italic).Sprint("`chef-automate deploy`") + " command")
		fmt.Println("Refer to `https://docs.chef.io/automate/cli/` for more information on related flags")
	} else {
		writer.Println("")
		writer.Println(string(t))
	}

	status.GlobalResult = t
	return nil
}

func checkConfigGenFileExist(outFile string, fsu fileutils.FileUtils) error {
	if _, err := fsu.Stat(outFile); err == nil {
		if configCmdFlags.overwriteFile {
			return nil
		}
		p := pmt.PromptFactory(os.Stdin, os.Stdout, fsu)
		ok, err := p.Confirm(fmt.Sprintf("%s file already exists. Do you wish to overwrite it?", outFile), "yes", "no")
		if err != nil {
			return status.Wrap(err, status.PromptFailed, err.Error())
		}
		if !ok {
			err = errors.New("failed to confirm overwrite")
			return status.Annotate(err, status.FileAccessError)
		}
	}
	return nil
}

var ocIdShowAppCmd = &cobra.Command{
	Use:   "oc-id-show-app",
	Short: "Get the details of the oauth applications registered with OC-ID",
	Long:  "Get the details of the oauth applications registered with OC-ID",
	RunE:  runOcIdShowAppCommand,
	Annotations: map[string]string{
		docs.Tag: docs.BastionHost,
	},
}

func runShowCmd(cmd *cobra.Command, args []string) error {

	if isA2HARBFileExist() {

		if isManagedServicesOn() {
			err := errorOnSelfManaged(configCmdFlags.postgresql, configCmdFlags.opensearch)
			if err != nil {
				return status.Annotate(err, status.InvalidCommandArgsError)
			}
		}

		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}

		if configCmdFlags.waitTimeout < DEFAULT_TIMEOUT {
			return errors.Errorf("The operation timeout duration for each individual node during the config show process should be set to a value greater than %v seconds.", DEFAULT_TIMEOUT)
		}

		var outFrontend string
		var outBackend string
		var outFiles []string
		if len(args) > 0 && args[0] != "" {
			outFrontend = args[0]
			outBackend = "> " + args[0]
			outFiles = []string{outFrontend}
		}
		frontendCommand := fmt.Sprintf(GET_FRONTEND_CONFIG, outFrontend)
		postgresqlCommand := fmt.Sprintf(GET_BACKEND_CONFIG, "postgresql", outBackend)
		opensearchCommand := fmt.Sprintf(GET_BACKEND_CONFIG, "opensearch", outBackend)

		frontend := &Cmd{
			CmdInputs: &CmdInputs{
				NodeType: false,
			},
		}

		automate := &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:         frontendCommand,
				WaitTimeout: configCmdFlags.waitTimeout,
				Single:      false,
				InputFiles:  []string{},
				Outputfiles: outFiles,
				NodeType:    configCmdFlags.automate,
			},
		}

		chefServer := &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:         frontendCommand,
				WaitTimeout: configCmdFlags.waitTimeout,
				Single:      false,
				InputFiles:  []string{},
				Outputfiles: outFiles,
				NodeType:    configCmdFlags.chef_server,
			},
		}

		postgresql := &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:         postgresqlCommand,
				WaitTimeout: configCmdFlags.waitTimeout,
				Single:      false,
				InputFiles:  []string{},
				Outputfiles: outFiles,
				NodeType:    configCmdFlags.postgresql,
			},
		}

		opensearch := &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:         opensearchCommand,
				WaitTimeout: configCmdFlags.waitTimeout,
				Single:      false,
				InputFiles:  []string{},
				Outputfiles: outFiles,
				NodeType:    configCmdFlags.opensearch,
			},
		}

		nodeMap := &NodeTypeAndCmd{
			Frontend:   frontend,
			Automate:   automate,
			ChefServer: chefServer,
			Postgresql: postgresql,
			Opensearch: opensearch,
			Infra:      infra,
		}
		sshUtil := NewSSHUtil(&SSHConfig{})
		cmdUtil := NewRemoteCmdExecutor(nodeMap, sshUtil, writer)

		if configCmdFlags.overwriteFile {
			writer.Errorln("Overwrite flag is not supported in HA")
		}

		if configCmdFlags.automate || configCmdFlags.chef_server || configCmdFlags.postgresql || configCmdFlags.opensearch {
			_, err = cmdUtil.Execute()
		} else {
			writer.Println(cmd.UsageString())
		}

		return err
	}
	res, err := client.GetAutomateConfig(configCmdFlags.timeout)
	if err != nil {
		return err
	}

	// TODO: should this be done server side?
	res.Config.Redact()

	// Handle writing to a file if a path was given
	if len(args) > 0 && args[0] != "" {
		outFile, err := filepath.Abs(args[0])
		if err != nil {
			return status.Annotate(err, status.FileAccessError)
		}

		if _, err := os.Stat(outFile); err == nil {
			if !configCmdFlags.overwriteFile {
				ok, err := writer.Confirm(fmt.Sprintf("%s file already exists. Do you wish to overwrite it?", outFile))
				if err != nil || !ok {
					if !ok {
						err = errors.New("failed to confirm overwrite")
					}

					return status.Annotate(err, status.FileAccessError)
				}
			}
		}

		err = res.Config.MarshalToTOMLFile(outFile, 0644)
		if err != nil {
			return status.Wrap(
				err,
				status.MarshalError,
				"Marshaling configuration to config.toml failed",
			)
		}

		writer.Successf("Chef Automate configuration file written to: %s", outFile)
		return nil
	}

	t, err := res.Config.MarshalToTOML()
	if err != nil {
		return status.Wrap(
			err,
			status.MarshalError,
			"Marshaling configuration to TOML failed",
		)
	}
	status.GlobalResult = res.Config

	writer.Println(string(t))
	return nil
}

func errorOnSelfManaged(isPostgresql, isOpenSearch bool) error {

	if isPostgresql {
		return errors.Errorf(ERROR_SELF_MANAGED_CONFIG_SHOW, "Postgresql")
	}
	if isOpenSearch {
		return errors.Errorf(ERROR_SELF_MANAGED_CONFIG_SHOW, "OpenSearch")
	}
	return nil
}

func runPatchCommand(cmd *cobra.Command, args []string) error {
	/*
		incase of a2ha mode of deployment, config file will be copied to /hab/a2_deploy_workspace/configs/automate.toml file
		then automate cluster ctl deploy will patch the config to automate
	*/

	if isA2HARBFileExist() {
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		timestamp := time.Now().Format("20060102150405")
		if configCmdFlags.waitTimeout < DEFAULT_TIMEOUT {
			return errors.Errorf("The operation timeout duration for each individual node during the config patch process should be set to a value greater than %v seconds.", DEFAULT_TIMEOUT)
		}
		isLoggerConfig, err := checkIfRequestedConfigHasCentrailisedLogging(args)
		if err != nil {
			return err
		}

		sshconfig := &SSHConfig{}
		sshconfig.sshUser = infra.Outputs.SSHUser.Value
		sshconfig.sshKeyFile = infra.Outputs.SSHKeyFile.Value
		sshconfig.sshPort = infra.Outputs.SSHPort.Value
		sshUtil := NewSSHUtil(sshconfig)

		if (configCmdFlags.opensearch || configCmdFlags.postgresql) && isLoggerConfig {
			err := patchCentralisedLoggingForPostgresql(args, sshUtil, infra)
			if err != nil {
				return err
			}
			inputfile, err := RemoveLogConfig(args[0])
			if err != nil {
				return err
			}
			args[0] = inputfile
			file, err := os.Open(args[0])
			if err != nil {
				return err
			}
			defer file.Close()
			buf := new(bytes.Buffer)
			_, err = buf.ReadFrom(file)
			if err != nil {
				return err
			}

			content := buf.String()
			if content == "" {
				return nil
			}
		}

		configFile := args[0]
		configFileName := stringutils.GetFileName(configFile)
		frontendPrefix := "frontend" + "_" + timestamp + "_"
		frontendCmd := fmt.Sprintf(FRONTEND_COMMAND, PATCH, frontendPrefix+configFileName, DATE_FORMAT)
		frontend := &Cmd{
			PreExec: prePatchCheckForFrontendNodes,
			CmdInputs: &CmdInputs{
				Cmd:                      frontendCmd,
				WaitTimeout:              configCmdFlags.waitTimeout,
				Single:                   false,
				Args:                     args,
				InputFiles:               []string{configFile},
				ErrorCheckEnableInOutput: true,
				NodeType:                 configCmdFlags.frontend,
				InputFilesPrefix:         frontendPrefix,
			},
		}
		automatePrefix := "automate" + "_" + timestamp + "_"
		automateCmd := fmt.Sprintf(FRONTEND_COMMAND, PATCH, automatePrefix+configFileName, DATE_FORMAT)
		automate := &Cmd{
			PreExec: prePatchCheckForFrontendNodes,
			CmdInputs: &CmdInputs{
				Cmd:                      automateCmd,
				WaitTimeout:              configCmdFlags.waitTimeout,
				Single:                   false,
				Args:                     args,
				InputFiles:               []string{configFile},
				ErrorCheckEnableInOutput: true,
				NodeType:                 configCmdFlags.automate,
				InputFilesPrefix:         automatePrefix,
			},
		}
		chefserverPrefix := "chef_server" + "_" + timestamp + "_"
		chefServerCmd := fmt.Sprintf(FRONTEND_COMMAND, PATCH, chefserverPrefix+configFileName, DATE_FORMAT)
		chefServer := &Cmd{
			PreExec: prePatchCheckForFrontendNodes,
			CmdInputs: &CmdInputs{
				Cmd:                      chefServerCmd,
				WaitTimeout:              configCmdFlags.waitTimeout,
				Single:                   false,
				Args:                     args,
				InputFiles:               []string{configFile},
				ErrorCheckEnableInOutput: true,
				NodeType:                 configCmdFlags.chef_server,
				InputFilesPrefix:         chefserverPrefix,
			},
		}
		postgresqlPrefix := "postgresql" + "_" + timestamp + "_"
		postgresqlCmd := fmt.Sprintf(BACKEND_COMMAND, DATE_FORMAT, "postgresql", "%s", postgresqlPrefix+configFileName)
		postgresql := &Cmd{
			PreExec: prePatchCheckForPostgresqlNodes,
			CmdInputs: &CmdInputs{
				Cmd:                      postgresqlCmd,
				Args:                     args,
				WaitTimeout:              configCmdFlags.waitTimeout,
				Single:                   true,
				InputFiles:               []string{configFile},
				ErrorCheckEnableInOutput: true,
				NodeType:                 configCmdFlags.postgresql,
				InputFilesPrefix:         postgresqlPrefix,
			},
		}
		opensearchPrefix := "opensearch" + "_" + timestamp + "_"
		opensearchCmd := fmt.Sprintf(BACKEND_COMMAND, DATE_FORMAT, "opensearch", "%s", opensearchPrefix+configFileName)
		opensearch := &Cmd{
			PreExec: prePatchCheckForOpensearch,
			CmdInputs: &CmdInputs{
				Cmd:                      opensearchCmd,
				Args:                     args,
				WaitTimeout:              configCmdFlags.waitTimeout,
				Single:                   true,
				InputFiles:               []string{configFile},
				ErrorCheckEnableInOutput: true,
				NodeType:                 configCmdFlags.opensearch,
				InputFilesPrefix:         opensearchPrefix,
			},
		}

		nodeMap := &NodeTypeAndCmd{
			Frontend:   frontend,
			Automate:   automate,
			ChefServer: chefServer,
			Postgresql: postgresql,
			Opensearch: opensearch,
			Infra:      infra,
		}
		//sshUtil := NewSSHUtil(&SSHConfig{})
		cmdUtil := NewRemoteCmdExecutor(nodeMap, sshUtil, writer)

		if configCmdFlags.frontend || configCmdFlags.automate || configCmdFlags.chef_server || configCmdFlags.postgresql || configCmdFlags.opensearch {
			_, err := cmdUtil.Execute()

			if err != nil {
				return err
			}
		} else {
			writer.Println(cmd.UsageString())
		}

	} else {
		cfg, err := dc.LoadUserOverrideConfigFile(args[0])
		if err != nil {
			return status.Annotate(err, status.ConfigError)
		}

		// keep chefserver fqdn same as automate fqdn in case of standalone automate
		if !checkIfFileExist(automateHaPath) && cfg.GetGlobal().GetV1().GetChefServer() != nil && cfg.GetGlobal().GetV1().GetChefServer().GetFqdn() != nil {
			res, err := client.GetAutomateConfig(configCmdFlags.timeout)
			if err != nil {
				return err
			}

			if cfg.Global.V1.ChefServer.Fqdn.GetValue() != res.Config.Global.V1.Fqdn.GetValue() {
				cfg.Global.V1.ChefServer.Fqdn.Value = res.Config.Global.V1.Fqdn.GetValue()
				if cfg.Global.V1.ChefServer.RootCa != nil {
					cfg.Global.V1.ChefServer.RootCa.Value = ""
				}
			}
		}

		if err = client.PatchAutomateConfig(configCmdFlags.timeout, cfg, writer); err != nil {
			return err
		}
		writer.Success("Configuration patched")
	}

	return nil
}

// prePatchCheckForFrontendNodes patches the configuration for front end nodes in Automate HA
func prePatchCheckForFrontendNodes(inputs *CmdInputs, sshUtil SSHUtil, infra *AutomateHAInfraDetails, remoteService string, writer *cli.Writer) error {
	srcPath, err := parseAndRemoveRestrictedKeysFromSrcFile(inputs.Args[0])
	if err != nil {
		return err
	}

	inputs.InputFiles[0] = srcPath

	return nil
}

func patchCentralisedLoggingForPostgresql(args []string, sshUtil SSHUtil, infra *AutomateHAInfraDetails) error {
	var remoteService string
	if configCmdFlags.postgresql {
		remoteService = "postgresql"
	}
	if configCmdFlags.opensearch {
		remoteService = "opensearch"
	}
	// checking for log configuration
	fmt.Println("calling enableCentralizedLogConfigForHA...")
	err := enableCentralizedLogConfigForHA(args, remoteService, sshUtil, infra.Outputs.PostgresqlPrivateIps.Value)
	if err != nil {
		return err
	}
	return nil
}

// prePatchCheckForPostgresqlNodes patches the config for postgresql nodes in Automate HA
func prePatchCheckForPostgresqlNodes(inputs *CmdInputs, sshUtil SSHUtil, infra *AutomateHAInfraDetails, remoteService string, writer *cli.Writer) error {
	if isManagedServicesOn() {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_SELF_MANAGED_CONFIG_PATCH, "Postgresql")
	}

	if len(infra.Outputs.PostgresqlPrivateIps.Value) == 0 {
		writer.Error("Postgres IPs not found in the config. Please contact the support team")
		return nil
	}

	args := inputs.Args
	// isLoggerConfig, err := checkIfRequestedConfigHasCentrailisedLogging(args)
	// if err != nil {
	// 	return err
	// }
	// if isLoggerConfig {
	// 	//checking for log configuration
	// 	err := enableCentralizedLogConfigForHA(args, remoteService, sshUtil, infra.Outputs.PostgresqlPrivateIps.Value)
	// 	if err != nil {
	// 		return err
	// 	}
	// 	inputfile, err := RemoveLogConfig(args[0])
	// 	if err != nil {
	// 		return err
	// 	}
	// 	args[0] = inputfile
	// 	file, err := os.Open(args[0])
	// 	if err != nil {
	// 		return err
	// 	}
	// 	defer file.Close()
	// 	buf := new(bytes.Buffer)
	// 	_, err = buf.ReadFrom(file)
	// 	if err != nil {
	// 		return err
	// 	}

	// 	content := buf.String()
	// 	if content == "" {
	// 		return nil
	// 	}
	// }

	//checking database configuration
	existConfig, reqConfig, err := getExistingAndRequestedConfigForPostgres(args, infra, GET_APPLIED_CONFIG, sshUtil, inputs)
	if err != nil {
		return err
	}

	//Implementing the config if there is some change in the database configuration
	isConfigChangedDatabase := isConfigChanged(existConfig, reqConfig)

	if isConfigChangedDatabase {
		if reqConfig.Ssl != nil {
			writer.Warn(fmt.Sprintf(CERT_WARNING, "ssl"))
		}
		reqConfig.Ssl = nil

		tomlFile := args[0]
		tomlFilePath, err := createTomlFileFromConfig(&reqConfig, tomlFile)
		if err != nil {
			return err
		}

		inputs.InputFiles[0] = tomlFilePath
	} else {
		return errors.New("No changes in existing config on postgresql and provided config")
	}
	return nil
}

// prePatchCheckForOpensearch patches the config for open-search nodes in Automate HA
func prePatchCheckForOpensearch(inputs *CmdInputs, sshUtil SSHUtil, infra *AutomateHAInfraDetails, remoteService string, writer *cli.Writer) error {
	if isManagedServicesOn() {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_SELF_MANAGED_CONFIG_PATCH, "OpenSearch")
	}

	if len(infra.Outputs.OpensearchPrivateIps.Value) == 0 {
		writer.Error("OpenSearch IPs not found in the config. Please contact the support team")
		return nil
	}

	args := inputs.Args

	//checking for log configuration
	err := enableCentralizedLogConfigForHA(args, remoteService, sshUtil, infra.Outputs.OpensearchPrivateIps.Value)
	if err != nil {
		return err
	}

	//checking database configuration
	existConfig, reqConfig, err := getExistingAndRequestedConfigForOpenSearch(args, infra, GET_APPLIED_CONFIG, sshUtil)
	if err != nil {
		return err
	}

	//Implementing the config if there is some change in the database configuration
	isConfigChangedDatabase := isConfigChanged(existConfig, reqConfig)

	if isConfigChangedDatabase {
		if reqConfig.TLS != nil {
			writer.Warn(fmt.Sprintf(CERT_WARNING, "tls"))
		}
		reqConfig.TLS = nil

		tomlFile := args[0]
		tomlFilePath, err := createTomlFileFromConfig(&reqConfig, tomlFile)
		if err != nil {
			return err
		}

		inputs.InputFiles[0] = tomlFilePath
	} else {
		return errors.New("No changes in existing config on opensearch and provided config")
	}
	return nil
}

func runSetCommand(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		var err error

		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}

		sshUser := infra.Outputs.SSHUser.Value
		sskKeyFile := infra.Outputs.SSHKeyFile.Value
		sshPort := infra.Outputs.SSHPort.Value

		timestamp := time.Now().Format("20060102150405")

		sshConfig := &SSHConfig{
			sshUser:    sshUser,
			sshKeyFile: sskKeyFile,
			sshPort:    sshPort,
		}
		sshUtil := NewSSHUtil(sshConfig)

		if configCmdFlags.automate {
			frontendIps := infra.Outputs.AutomatePrivateIps.Value
			if len(frontendIps) == 0 {
				writer.Error("No automate IPs are found")
				os.Exit(1)
			}
			const remoteService string = "automate"
			err = setConfigForFrontEndNodes(args, sshUtil, frontendIps, remoteService, timestamp, writer)
		} else if configCmdFlags.chef_server {
			frontendIps := infra.Outputs.ChefServerPrivateIps.Value
			if len(frontendIps) == 0 {
				writer.Error("No chef-server IPs are found")
				os.Exit(1)
			}
			const remoteService string = "chef-server"
			err = setConfigForFrontEndNodes(args, sshUtil, frontendIps, remoteService, timestamp, writer)
		} else if configCmdFlags.postgresql {
			const remoteService string = "postgresql"
			err = setConfigForPostgresqlNodes(args, remoteService, sshUtil, infra, timestamp, writer)
		} else if configCmdFlags.opensearch {
			const remoteService string = "opensearch"
			err = setConfigForOpensearch(args, remoteService, sshUtil, infra, timestamp, writer)
		} else {
			writer.Println(cmd.UsageString())
		}
		if err != nil {
			return err
		}
	} else {
		cfg, err := dc.LoadUserOverrideConfigFile(args[0])
		if err != nil {
			return status.Annotate(err, status.ConfigError)
		}

		if err := client.SetAutomateConfig(configCmdFlags.timeout, cfg, writer); err != nil {
			return err
		}
		writer.Success("Configuration set")
	}
	return nil
}

func runOcIdShowAppCommand(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		infra, err := getAutomateHAInfraDetails()
		if err != nil {
			return err
		}
		if configCmdFlags.waitTimeout < DEFAULT_TIMEOUT {
			return errors.Errorf("The operation timeout duration for each individual node during the config oc-id-show-app process should be set to a value greater than %v seconds.", DEFAULT_TIMEOUT)
		}

		frontendCmd := fmt.Sprintf(CONF_PREFIX_FOR_SHOW_APPS_CMD, OCID_SHOW_APP)
		frontend := &Cmd{
			CmdInputs: &CmdInputs{
				Cmd:                      frontendCmd,
				WaitTimeout:              configCmdFlags.waitTimeout,
				Single:                   true,
				Args:                     args,
				ErrorCheckEnableInOutput: true,
				NodeType:                 true,
			},
		}

		nodeMap := &NodeTypeAndCmd{
			Frontend: frontend,
			Infra:    infra,
		}
		sshUtil := NewSSHUtil(&SSHConfig{})
		cmdUtil := NewRemoteCmdExecutor(nodeMap, sshUtil, writer)

		_, cmdExecErr := cmdUtil.Execute()

		if cmdExecErr != nil {
			return cmdExecErr
		}
	} else {
		oauthAppDetailsFilePath := "/hab/svc/automate-cs-ocid/config/registered_oauth_applications.yaml"
		content, err := os.ReadFile(oauthAppDetailsFilePath)
		if err != nil {
			printErr := "Could not find the file with the registered application details. Pls restart OC-ID to generate it."
			writer.Errorln(printErr)
			return err
		}
		registeredAppDetails := string(content)
		writer.Println(registeredAppDetails)
	}
	return nil
}

type ResultConfigSet struct {
	HostIP string
	Output string
	Error  error
}

// setConfigForFrontEndNodes set the configuration for front end nodes in Automate HA
func setConfigForFrontEndNodes(args []string, sshUtil SSHUtil, frontendIps []string, remoteService string, timestamp string, cliWriter *cli.Writer) error {
	resultChan := make(chan ResultConfigSet, len(frontendIps))
	configFile := remoteService + timestamp
	scriptCommands := fmt.Sprintf(FRONTEND_COMMAND, SET, configFile, DATE_FORMAT)
	originalSSHConfig := sshUtil.getSSHConfig()

	for _, hostIP := range frontendIps {
		newSSHConfig := &SSHConfig{
			sshUser:    originalSSHConfig.sshUser,
			sshPort:    originalSSHConfig.sshPort,
			sshKeyFile: originalSSHConfig.sshKeyFile,
			hostIP:     hostIP,
		}
		newSSHUtil := NewSSHUtil(newSSHConfig)

		printConnectionMessage(remoteService, hostIP, cliWriter)

		go func(args []string, configFile string, remoteService string, scriptCommands string, newSSHUtil SSHUtil, resultChan chan ResultConfigSet) {
			rc := ResultConfigSet{newSSHUtil.getSSHConfig().hostIP, "", nil}
			err := newSSHUtil.copyFileToRemote(args[0], configFile, false)
			if err != nil {
				rc.Error = err
				resultChan <- rc
				return
			}

			output, err := newSSHUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
			if err != nil {
				rc.Error = err
				resultChan <- rc
				return
			}

			err = checkOutputForError(output)
			if err != nil {
				rc.Error = err
				resultChan <- rc
				return
			}

			rc.Output = output
			resultChan <- rc
		}(args, configFile, remoteService, scriptCommands, newSSHUtil, resultChan)
	}

	for i := 0; i < len(frontendIps); i++ {
		result := <-resultChan

		if i == 0 {
			writer.StopSpinner()
			cliWriter.Println("=====================================================")
		}

		if result.Error != nil {
			printConfigErrorMessage(setting, remoteService, result.HostIP, cliWriter, result.Error.Error())
		} else {
			cliWriter.Printf("Output for Host IP %s : %s", result.HostIP, result.Output+"\n")
			printConfigSuccessMessage(setting, remoteService, result.HostIP, cliWriter)
		}

		if i < len(frontendIps)-1 {
			cliWriter.Println("=====================================================")
			writer.StartSpinner()
		}
	}

	close(resultChan)
	return nil
}

// setConfigForPostgresqlNodes set the configuration for postgresql nodes in Automate HA
func setConfigForPostgresqlNodes(args []string, remoteService string, sshUtil SSHUtil, infra *AutomateHAInfraDetails, timestamp string, writer *cli.Writer) error {
	if isManagedServicesOn() {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_SELF_MANAGED_CONFIG_SET, "Postgresql")
	}

	if len(infra.Outputs.PostgresqlPrivateIps.Value) == 0 {
		writer.Error("Postgres IPs not found in the config. Please contact the support team")
		return nil
	}

	//checking for log configuration
	err := enableCentralizedLogConfigForHA(args, remoteService, sshUtil, infra.Outputs.PostgresqlPrivateIps.Value)
	if err != nil {
		return err
	}

	//Getting Requested Config
	reqConfigInterface, err := getConfigInterfaceForPostgresqlOrOpenSearch(args, postgresql)
	if err != nil {
		return err
	}
	reqConfig := reqConfigInterface.(PostgresqlConfig)

	//Setting the config
	tomlFile := args[0] + timestamp
	tomlFilePath, err := createTomlFileFromConfig(&reqConfig, tomlFile)
	if err != nil {
		return err
	}

	return setConfigForPostgresqlAndOpensearch(remoteService, timestamp, sshUtil, infra.Outputs.PostgresqlPrivateIps.Value[0], tomlFilePath, writer)
}

// setConfigForOpensearch set the configuration for opensearch nodes in Automate HA
func setConfigForOpensearch(args []string, remoteService string, sshUtil SSHUtil, infra *AutomateHAInfraDetails, timestamp string, writer *cli.Writer) error {
	if isManagedServicesOn() {
		return status.Errorf(status.InvalidCommandArgsError, ERROR_SELF_MANAGED_CONFIG_SET, "OpenSearch")
	}

	if len(infra.Outputs.OpensearchPrivateIps.Value) == 0 {
		writer.Error("OpenSearch IPs not found in the config. Please contact the support team")
		return nil
	}

	//checking for log configuration
	err := enableCentralizedLogConfigForHA(args, remoteService, sshUtil, infra.Outputs.OpensearchPrivateIps.Value)
	if err != nil {
		return err
	}

	//Getting Requested Config
	reqConfigInterface, err := getConfigInterfaceForPostgresqlOrOpenSearch(args, opensearch_const)
	if err != nil {
		return err
	}
	reqConfig := reqConfigInterface.(OpensearchConfig)

	//Setting the config
	tomlFile := args[0] + timestamp
	tomlFilePath, err := createTomlFileFromConfig(&reqConfig, tomlFile)
	if err != nil {
		return err
	}

	return setConfigForPostgresqlAndOpensearch(remoteService, timestamp, sshUtil, infra.Outputs.OpensearchPrivateIps.Value[0], tomlFilePath, writer)
}

// setConfigForPostgresqlAndOpensearch set the configuration for postgresql and opensearch nodes in Automate HA
func setConfigForPostgresqlAndOpensearch(remoteService string, timestamp string, sshUtil SSHUtil, hostIP string, tomlFilePath string, writer *cli.Writer) error {
	scriptCommands := fmt.Sprintf(BACKEND_COMMAND, DATE_FORMAT, remoteService, "%s", remoteService+timestamp)

	sshUtil.getSSHConfig().hostIP = hostIP
	printConnectionMessage(remoteService, sshUtil.getSSHConfig().hostIP, writer)

	err := sshUtil.copyFileToRemote(tomlFilePath, remoteService+timestamp, true)
	if err != nil {
		writer.Errorf("%v", err)
		return err
	}

	output, err := sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
	if err != nil {
		writer.Errorf("%v", err)
		return err
	}

	err = checkOutputForError(output)
	if err != nil {
		return err
	}

	writer.Printf(output + "\n")
	printConfigSuccessMessage(setting, remoteService, sshUtil.getSSHConfig().hostIP, writer)

	return nil
}

func getMergedOpensearchInterface(rawOutput string, pemFilePath string, remoteService string) (interface{}, error) {

	var src OpensearchConfig
	if _, err := toml.Decode(cleanToml(rawOutput), &src); err != nil {
		return "", err
	}

	pemBytes, err := os.ReadFile(pemFilePath)
	if err != nil {
		return "", err
	}

	destString := string(pemBytes)
	var dest OpensearchConfig
	if _, err := toml.Decode(destString, &dest); err != nil {
		return "", errors.Errorf(configValid, remoteService)
	}

	mergo.Merge(&dest, src) //, mergo.WithOverride

	return dest, nil
}

func getMergedPostgresqlInterface(rawOutput string, pemFilePath string, remoteService string) (interface{}, error) {

	var src PostgresqlConfig
	if _, err := toml.Decode(cleanToml(rawOutput), &src); err != nil {
		return "", err
	}

	pemBytes, err := os.ReadFile(pemFilePath)
	if err != nil {
		return "", err
	}

	destString := string(pemBytes)
	var dest PostgresqlConfig
	if _, err := toml.Decode(destString, &dest); err != nil {
		return "", errors.Errorf(configValid, remoteService)
	}

	mergo.Merge(&dest, src) //, mergo.WithOverride

	return dest, nil
}

func getRemoteType(flag string, infra *AutomateHAInfraDetails) (string, string) {
	switch strings.ToLower(flag) {
	case "opensearch", "os", "o":
		return infra.Outputs.OpensearchPrivateIps.Value[0], "opensearch"
	case "postgresql", "pg", "p":
		return infra.Outputs.PostgresqlPrivateIps.Value[0], "postgresql"
	default:
		return "", ""
	}
}

func cleanToml(rawData string) string {
	re := regexp.MustCompile("(?im).*info:.*$")
	tomlOutput := re.ReplaceAllString(rawData, "")
	return tomlOutput
}

func getMergerTOMLPath(args []string, infra *AutomateHAInfraDetails, timestamp string, remoteType string, config string) (string, error) {
	sshconfig := &SSHConfig{}
	tomlFile := args[0] + timestamp
	sshconfig.sshUser = infra.Outputs.SSHUser.Value
	sshconfig.sshKeyFile = infra.Outputs.SSHKeyFile.Value
	sshconfig.sshPort = infra.Outputs.SSHPort.Value

	remoteIP, remoteService := getRemoteType(remoteType, infra)
	sshconfig.hostIP = remoteIP
	sshUtil := NewSSHUtil(sshconfig)
	scriptCommands := fmt.Sprintf(config, remoteService)
	rawOutput, err := sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
	if err != nil {
		return "", err
	}

	var (
		dest interface{}
		err1 error
	)
	if remoteService == "opensearch" {
		dest, err1 = getMergedOpensearchInterface(rawOutput, args[0], remoteService)
	} else {
		dest, err1 = getMergedPostgresqlInterface(rawOutput, args[0], remoteService)
	}
	if err1 != nil {
		return "", err1
	}

	f, err := os.Create(tomlFile)

	if err != nil {
		// failed to create/open the file
		writer.Bodyf("Failed to create/open the file, \n%v", err)
		return "", err
	}
	if err := toml.NewEncoder(f).Encode(dest); err != nil {
		// failed to encode
		writer.Bodyf("Failed to encode\n%v", err)
		return "", err
	}
	if err := f.Close(); err != nil {
		// failed to close the file
		writer.Bodyf("Failed to close the file\n%v", err)
		return "", err
	}

	return tomlFile, nil
}

// getConfigFromRemoteServer gets the config for remote server using the commands
func getConfigFromRemoteServer(infra *AutomateHAInfraDetails, remoteType string, config string, sshUtil SSHUtil) (string, error) {
	remoteIP, remoteService := getRemoteType(remoteType, infra)
	sshUtil.getSSHConfig().hostIP = remoteIP
	scriptCommands := fmt.Sprintf(config, remoteService)
	rawOutput, err := sshUtil.connectAndExecuteCommandOnRemote(scriptCommands, true)
	if err != nil {
		return "", err
	}
	return rawOutput, nil
}

// getDecodedConfig gets the decoded config from the input
func getDecodedConfig(input string, remoteService string) (interface{}, error) {
	if remoteService == postgresql {
		var src PostgresqlConfig
		if _, err := toml.Decode(cleanToml(input), &src); err != nil {
			return nil, err
		}
		return src, nil
	}
	if remoteService == opensearch_const {
		var src OpensearchConfig
		if _, err := toml.Decode(cleanToml(input), &src); err != nil {
			return nil, err
		}
		return src, nil
	}

	return nil, nil
}

// getConfigInterfaceForPostgresqlOrOpenSearch gets the requested config from the args provided for postgresql or opensearch
func getConfigInterfaceForPostgresqlOrOpenSearch(args []string, remoteService string) (interface{}, error) {
	pemBytes, err := os.ReadFile(args[0])
	if err != nil {
		return nil, err
	}
	destString := string(pemBytes)
	if remoteService == "postgresql" {
		var dest PostgresqlConfig
		if _, err := toml.Decode(destString, &dest); err != nil {
			return dest, errors.Errorf(configValid, remoteService)
		}
		return dest, nil
	} else {
		var dest OpensearchConfig
		if _, err := toml.Decode(destString, &dest); err != nil {
			return dest, errors.Errorf(configValid, remoteService)
		}
		return dest, nil
	}
}

// func checkConfigType(args []string, remoteService string) {
// 	pemBytes, err := os.ReadFile(args[0])
// 	if err != nil {
// 		return
// 	}
// 	var data interface{}
// 	if err:= toml.Unmarshal([]byte(pemBytes),&data); err != nil{
// 		fmt.Println("Error in parsing the toml : ",err)
// 		return
// 	}
// 	switch v := data.(type){
// 	case map[string]interface{}:
// 		typeName := reflect.TypeOf(v).Name()
// 		if _,exists := v["RedirectSysLog"]; exists {

// 		}
// 	default:
// 		return
// 	}

// }

// isConfigChanged checks if configuration is changed
func isConfigChanged(src interface{}, dest interface{}) bool {
	return !reflect.DeepEqual(src, dest)
}

// getExistingAndRequestedConfigForPostgres get user updating config and existing *applied* config for postgresql
func getExistingAndRequestedConfigForPostgres(args []string, infra *AutomateHAInfraDetails, config string, sshUtil SSHUtil, inputs *CmdInputs) (PostgresqlConfig, PostgresqlConfig, error) {

	//Getting Existing config from server
	var existingAppliedConfig PostgresqlConfig
	var reqConfig PostgresqlConfig
	var emptyConfig PostgresqlConfig

	// getting applied remote config from pg node
	srcInputString, err := getConfigFromRemoteServer(infra, postgresql, config, sshUtil)
	if err != nil {
		return existingAppliedConfig, reqConfig, errors.Wrapf(err, "Unable to get config from the server with error")
	}
	existingAppliedConfigInterface, err := getDecodedConfig(srcInputString, postgresql)
	if err != nil {
		return existingAppliedConfig, reqConfig, err
	}
	existingAppliedConfig = existingAppliedConfigInterface.(PostgresqlConfig)

	//Getting Requested Config
	reqConfigInterface, err := getConfigInterfaceForPostgresqlOrOpenSearch(args, postgresql)
	if err != nil {
		return existingAppliedConfig, reqConfig, err
	}
	reqConfig = reqConfigInterface.(PostgresqlConfig)
	if !isConfigChanged(emptyConfig, reqConfig) {
		return existingAppliedConfig, reqConfig, status.Annotate(errors.New("Incorrect Config"), status.ConfigError)
	}
	mergo.Merge(&reqConfig, existingAppliedConfig)
	return existingAppliedConfig, reqConfig, nil
}

func checkIfRequestedConfigHasCentrailisedLogging(args []string) (bool, error) {
	config, err := ptoml.LoadFile(args[0])
	if err != nil {
		writer.Println(err.Error())
		return false, err
	}
	isconfig := config.Get("global.v1.log").(*ptoml.Tree)
	fmt.Println("is config val ", isconfig)
	if isconfig != nil {
		val := isconfig.Get("redirect_sys_log").(bool)
		fmt.Println("final value ", val)
		if val == true {
			return true, nil
		}
	}
	return false, nil
}

// getExistingAndRequestedConfigForOpenSearch gets existed and requested config for opensearch
func getExistingAndRequestedConfigForOpenSearch(args []string, infra *AutomateHAInfraDetails, config string, sshUtil SSHUtil) (OpensearchConfig, OpensearchConfig, error) {
	//Getting Existing config from server
	var existingConfig OpensearchConfig
	var reqConfig OpensearchConfig
	var emptyConfig OpensearchConfig
	srcInputString, err := getConfigFromRemoteServer(infra, opensearch_const, config, sshUtil)
	if err != nil {
		return existingConfig, reqConfig, errors.Wrapf(err, "Unable to get config from the server with error")
	}
	existingConfigInterface, err := getDecodedConfig(srcInputString, opensearch_const)
	if err != nil {
		return existingConfig, reqConfig, err
	}
	existingConfig = existingConfigInterface.(OpensearchConfig)

	//Getting Requested Config
	reqConfigInterface, err := getConfigInterfaceForPostgresqlOrOpenSearch(args, opensearch_const)
	if err != nil {
		return existingConfig, reqConfig, err
	}
	reqConfig = reqConfigInterface.(OpensearchConfig)
	if !isConfigChanged(emptyConfig, reqConfig) {
		return existingConfig, reqConfig, status.Annotate(errors.New("Incorrect Config"), status.ConfigError)
	}
	mergo.Merge(&reqConfig, existingConfig)
	return existingConfig, reqConfig, nil
}

// createTomlFileFromConfig created a toml file where path and struct interface is provided
func createTomlFileFromConfig(config interface{}, tomlFile string) (string, error) {
	f, err := os.Create(tomlFile)

	if err != nil {
		// failed to create/open the file
		writer.Bodyf("Failed to create/open the file, \n%v", err)
		return "", err
	}
	if err := toml.NewEncoder(f).Encode(config); err != nil {
		// failed to encode
		writer.Bodyf("Failed to encode\n%v", err)
		return "", err
	}
	if err := f.Close(); err != nil {
		// failed to close the file
		writer.Bodyf("Failed to close the file\n%v", err)
		return "", err
	}

	return tomlFile, nil

}

func parseAndRemoveRestrictedKeysFromSrcFile(srcString string) (string, error) {

	tomlbyt, _ := os.ReadFile(srcString)
	destString := string(tomlbyt)
	var dest dc.AutomateConfig
	if _, err := toml.Decode(destString, &dest); err != nil {
		fmt.Println(err)
	}

	if dest.Deployment == nil ||
		dest.Deployment.V1 == nil ||
		dest.Deployment.V1.Svc == nil ||
		dest.Deployment.V1.Svc.Products == nil {
		return srcString, nil
	} else {
		// Following are the unsupported or restricted key to patch via bastion
		writer.Warn(PRODUCT_WARNING)
		dest.Deployment.V1.Svc.Products = nil

		srcString, err := createTomlFileFromConfig(dest, srcString)
		if err != nil {
			return "", err
		}
		return srcString, nil
	}
}

func RemoveLogConfig(srcString string) (string, error) {
	tomlbyt, _ := os.ReadFile(srcString)
	destString := string(tomlbyt)
	if configCmdFlags.postgresql {
		var dest PostgresqlConfig
		if _, err := toml.Decode(destString, &dest); err != nil {
			fmt.Println(err)
		}
		if dest.Global != nil && dest.Global.V1 != nil && dest.Global.V1.Log != nil {
			dest.Global.V1.Log = nil
			dest.Global.V1 = nil
			dest.Global = nil
		}
		srcString, err := createTomlFileFromConfig(dest, srcString)
		if err != nil {
			return "", err
		}
		return srcString, nil
	}
	if configCmdFlags.opensearch {
		var dest OpensearchConfig
		if _, err := toml.Decode(destString, &dest); err != nil {
			fmt.Println(err)
		}
		if dest.Global != nil && dest.Global.V1 != nil && dest.Global.V1.Log != nil {
			dest.Global.V1.Log = nil
			dest.Global.V1 = nil
			dest.Global = nil
		}
		srcString, err := createTomlFileFromConfig(dest, srcString)
		if err != nil {
			return "", err
		}
		return srcString, nil
	}
	return "", nil

}

// If the output contains the word "error" then return error
func checkOutputForError(output string) error {
	if strings.Contains(strings.ToUpper(strings.TrimSpace(output)), "ERROR") {
		return errors.New(output)
	}
	return nil
}

// printConnectionMessage prints the connection message
func printConnectionMessage(remoteService string, hostIP string, cliWriter *cli.Writer) {
	cliWriter.Println("Connecting to the " + remoteService + " node : " + hostIP)
	cliWriter.BufferWriter().Flush()
}

// printConfigErrorMessage prints the config error message
func printConfigErrorMessage(configType string, remoteService string, hostIP string, cliWriter *cli.Writer, err string) {
	cliWriter.Fail(configType + " failed on " + remoteService + " node : " + hostIP + " with error:\n" + err + "\n")
	cliWriter.BufferWriter().Flush()
}

// printConfigSuccessMessage prints the config success message
func printConfigSuccessMessage(configType string, remoteService string, hostIP string, cliWriter *cli.Writer) {
	cliWriter.Success(configType + " is completed on " + remoteService + " node : " + hostIP + "\n")
	cliWriter.BufferWriter().Flush()
}
