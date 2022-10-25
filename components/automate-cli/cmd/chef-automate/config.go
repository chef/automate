package main

import (
	"bytes"
	"regexp"
	"strings"
	"time"

	"fmt"
	"io/ioutil"
	"net"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"golang.org/x/crypto/ssh"
	"golang.org/x/crypto/ssh/knownhosts"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/toml"
	"github.com/imdario/mergo"
)

var configCmdFlags = struct {
	overwriteFile bool
	timeout       int64
	acceptMLSA    bool

	// automate    bool
	// chef_server bool
	frontend         bool
	opensearch       bool
	postgresql       bool
	getAppliedConfig bool
	file             string
}{}

const (
	FRONTEND_COMMANDS = `
	sudo chef-automate config patch /tmp/%s;
	export TIMESTAMP=$(date +'%s');
	sudo mv /etc/chef-automate/config.toml /etc/chef-automate/config.toml.$TIMESTAMP;
	sudo chef-automate config show > sudo /etc/chef-automate/config.toml`

	BACKEND_COMMAND = `
	export TIMESTAMP=$(date +"%s");
	mv /tmp/pg_config.toml /tmp/pg_config.toml.$TIMESTAMP;
	cat /tmp/%s >> /tmp/pg_config.toml;
	echo "yes" | sudo hab config apply automate-ha-%s.default  $(date '+%s') /tmp/pg_config.toml;  
	rm -rf /tmp/%s; 
	`

	GET_CONFIG = `
	source <(sudo cat /hab/sup/default/SystemdEnvironmentFile.sh);
	automate-backend-ctl show --svc=automate-ha-%s | tail -n +2
	`

	dateFormat = "%Y%m%d%H%M%S"
)

func init() {
	configCmd.AddCommand(showConfigCmd)
	configCmd.AddCommand(patchConfigCmd)
	configCmd.AddCommand(setConfigCmd)

	showConfigCmd.Flags().BoolVarP(&configCmdFlags.overwriteFile, "overwrite", "o", false, "Overwrite existing config.toml")

	// patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.automate, "automate", "a", false, "Patch toml configuration to the automate node")
	// patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.chef_server, "chef_server", "c", false, "Patch toml configuration to the chef_server node")
	patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.frontend, "frontend", "f", false, "Patch toml configuration to the all frontend nodes")
	patchConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.frontend, "fe", false, "Patch toml configuration to the all frontend nodes[DUPLICATE]")
	patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.opensearch, "opensearch", "o", false, "Patch toml configuration to the opensearch node")
	patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.postgresql, "postgresql", "p", false, "Patch toml configuration to the postgresql node")
	patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.getAppliedConfig, "get-config", "G", false, "Get applied config from Opensearch or Postgresql nodes")
	patchConfigCmd.PersistentFlags().StringVarP(&configCmdFlags.file, "file", "F", "output_config.toml", "File path to write the config (default \"output_config.toml\")")

	configCmd.PersistentFlags().BoolVarP(&configCmdFlags.acceptMLSA, "auto-approve", "y", false, "Do not prompt for confirmation; accept defaults and continue")

	configCmd.PersistentFlags().Int64VarP(&configCmdFlags.timeout, "timeout", "t", 10, "Request timeout in seconds")

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
	Args:  cobra.RangeArgs(0, 1),
}

var patchConfigCmd = &cobra.Command{
	Use:   "patch path/to/config.toml",
	Short: "patch the Chef Automate configuration", Long: "Apply a partial Chef Automate configuration to the deployment. It will take the partial configuration, merge it with the existing configuration, and apply and required changes.",
	RunE: runPatchCommand,
	Args: cobra.ExactArgs(1),
}

var setConfigCmd = &cobra.Command{
	Use:   "set path/to/config.toml",
	Short: "set the Chef Automate configuration",
	Long:  "Set the Chef Automate configuration for the deployment. It will replace the Chef Automate configuration with the given configuration and apply any required changes.",
	RunE:  runSetCommand,
	Args:  cobra.ExactArgs(1),
}

func runShowCmd(cmd *cobra.Command, args []string) error {
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
func runPatchCommand(cmd *cobra.Command, args []string) error {
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return err
	}

	sshUser := infra.Outputs.SSHUser.Value
	sskKeyFile := infra.Outputs.SSHKeyFile.Value
	sshPort := infra.Outputs.SSHPort.Value

	/*
		incase of a2ha mode of deployment, config file will be copied to /hab/a2_deploy_workspace/configs/automate.toml file
		then automate cluster ctl deploy will patch the config to automate
	*/

	/*
		// if isA2HARBFileExist() {
		// 	if !configCmdFlags.acceptMLSA {
		// 		response, err := writer.Prompt(`If you have created any new bundles using upgrade commands and not deployed it,
		// 		this command will deploy that new airgap bundle with patching of configuration.
		// 		Press y to agree, n to to disagree? [y/n]`)
		// 		if err != nil {
		// 			return err
		// 		}

		// 		if !strings.Contains(response, "y") {
		// 			return errors.New("canceled Patching")
		// 		}
		// 	}
		// 	input, err := ioutil.ReadFile(args[0])
		// 	if err != nil {
		// 		return nil
		// 	}
		// 	err = ioutil.WriteFile(AUTOMATE_HA_AUTOMATE_CONFIG_FILE, input, 0644)
		// 	if err != nil {
		// 		writer.Printf("error in patching automate config to automate HA")
		// 		return err
		// 	}
		// 	return executeDeployment(args)
		// }
	*/

	if isA2HARBFileExist() {

		timestamp := time.Now().Format("20060102150405")

		if configCmdFlags.getAppliedConfig {
			remoteIP, remoteService := getRemoteType(args[0], infra)
			scriptCommands := fmt.Sprintf(GET_CONFIG, remoteService)
			if len(remoteIP) > 0 {
				err, output := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, remoteIP, scriptCommands)
				if err != nil {
					writer.Errorf("%s", err)
					return err
				}
				createTomlFile(configCmdFlags.file, output)
			}
			return nil
		}

		if configCmdFlags.frontend {
			frontendIps := append(infra.Outputs.ChefServerPrivateIps.Value, infra.Outputs.AutomatePrivateIps.Value...)
			if len(frontendIps) == 0 {
				writer.Error("No frontend IPs are found")
				os.Exit(1)
			}
			// writer.Printf("IPs: " + strings.Join(frontendIps, "") + "Path :" + args[0])
			scriptCommands := fmt.Sprintf(FRONTEND_COMMANDS, args[0], dateFormat)
			for i := 0; i < len(frontendIps); i++ {
				copyFileToRemote(sskKeyFile, args[0], sshUser, frontendIps[i])
				_, output := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, frontendIps[i], scriptCommands)
				writer.Printf(output)
			}
		}
		if configCmdFlags.postgresql {
			const remoteService string = "postgresql"
			err, tomlFilePath := getMergerTOMLPath(args, infra, timestamp)
			if err != nil {
				return err
			}
			scriptCommands := fmt.Sprintf(BACKEND_COMMAND, dateFormat, args[0], remoteService, "%s", args[0])
			// writer.Body(scriptCommands)
			if len(infra.Outputs.PostgresqlPrivateIps.Value) > 0 {
				remoteIp := infra.Outputs.PostgresqlPrivateIps.Value[0]
				copyFileToRemote(sskKeyFile, tomlFilePath, sshUser, remoteIp)
				_, output := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, remoteIp, scriptCommands)
				writer.Printf(output)
			}
		}
		if configCmdFlags.opensearch {
			const remoteService string = "opensearch"
			err, tomlFilePath := getMergerTOMLPath(args, infra, timestamp)
			if err != nil {
				return err
			}
			scriptCommands := fmt.Sprintf(BACKEND_COMMAND, dateFormat, args[0], remoteService, "%s", args[0])
			// writer.Body(scriptCommands)
			if len(infra.Outputs.OpensearchPrivateIps.Value) > 0 {
				remoteIp := infra.Outputs.OpensearchPrivateIps.Value[0]
				copyFileToRemote(sskKeyFile, tomlFilePath, sshUser, remoteIp)
				_, output := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, remoteIp, scriptCommands)
				writer.Printf(output)
			}
		}
	} else {

		cfg, err := dc.LoadUserOverrideConfigFile(args[0])
		if err != nil {
			return status.Annotate(err, status.ConfigError)
		}
		if err = client.PatchAutomateConfig(configCmdFlags.timeout, cfg, writer); err != nil {
			return err
		}
	}

	writer.Success("Configuration patched")
	return nil
}

func ConnectAndExecuteCommandOnRemote(sshUser string, sshPort string, sshKeyFile string, hostIP string, remoteCommands string) (error, string) {

	pemBytes, err := ioutil.ReadFile(sshKeyFile)
	if err != nil {
		writer.Errorf("Unable to read private key: %v", err)
		return err, ""
	}
	signer, err := ssh.ParsePrivateKey(pemBytes)
	if err != nil {
		writer.Errorf("Parsing key failed: %v", err)
		return err, ""
	}
	var (
		keyErr *knownhosts.KeyError
	)

	// Client config
	config := &ssh.ClientConfig{
		User: sshUser,
		Auth: []ssh.AuthMethod{ssh.PublicKeys(signer)},
		HostKeyCallback: ssh.HostKeyCallback(func(host string, remote net.Addr, pubKey ssh.PublicKey) error {
			kh := checkKnownHosts()
			hErr := kh(host, remote, pubKey)
			// Reference: https://blog.golang.org/go1.13-errors
			// To understand what errors.As is.
			if errors.As(hErr, &keyErr) && len(keyErr.Want) > 0 {
				// Reference: https://www.godoc.org/golang.org/x/crypto/ssh/knownhosts#KeyError
				// if keyErr.Want slice is empty then host is unknown, if keyErr.Want is not empty
				// and if host is known then there is key mismatch the connection is then rejected.
				writer.Printf("WARNING: Given hostkeystring is not a key of %s, either a MiTM attack or %s has reconfigured the host pub key.", host, host)
				return keyErr
			} else if errors.As(hErr, &keyErr) && len(keyErr.Want) == 0 {
				// host key not found in known_hosts then give a warning and continue to connect.
				writer.Printf("WARNING: %s is not trusted, adding this key to known_hosts file.\n", host)
				// time.Sleep(2 * time.Second)
				return addHostKey(host, remote, pubKey)
			}
			writer.Printf("Pub key exists for %s.", host)
			return nil
		}),
	}

	// Open connection
	conn, err := ssh.Dial("tcp", hostIP+":"+sshPort, config)
	if conn == nil || err != nil {
		writer.Errorf("dial failed:%v", err)
		return err, ""
	}
	defer conn.Close()

	// Open session
	session, err := conn.NewSession()
	if err != nil {
		writer.Errorf("session failed:%v", err)
		return err, ""
	}
	var stdoutBuf bytes.Buffer
	session.Stdout = &stdoutBuf
	// err = session.Run("sudo rm -rf /tmp/" + path + "")

	writer.StartSpinner()
	err = session.Run(remoteCommands)

	writer.StopSpinner()
	if err != nil {
		writer.Errorf("Run failed:%v", err)
		return err, ""
	}
	defer session.Close()
	// writer.Printf("\n%s\n", stdoutBuf)
	return nil, stdoutBuf.String()
}

func runSetCommand(cmd *cobra.Command, args []string) error {
	cfg, err := dc.LoadUserOverrideConfigFile(args[0])
	if err != nil {
		return status.Annotate(err, status.ConfigError)
	}

	if err := client.SetAutomateConfig(configCmdFlags.timeout, cfg, writer); err != nil {
		return err
	}

	writer.Success("Configuration set")
	return nil
}

func copyFileToRemote(sshKeyFile string, tomlFilePath string, sshUser string, hostIP string) {
	cmd := "scp"
	exec_args := []string{"-i", sshKeyFile, "-r", tomlFilePath, sshUser + "@" + hostIP + ":/tmp/"}
	if err := exec.Command(cmd, exec_args...).Run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func createKnownHosts() {
	f, fErr := os.OpenFile(filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts"), os.O_CREATE, 0600)
	if fErr != nil {
		writer.Errorf("%v", fErr)
		return
	}
	f.Close()
}

func checkKnownHosts() ssh.HostKeyCallback {
	createKnownHosts()
	kh, e := knownhosts.New(filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts"))
	if e != nil {
		writer.Errorf("%v", e)
		return nil
	}
	return kh
}

func addHostKey(host string, remote net.Addr, pubKey ssh.PublicKey) error {
	// add host key if host is not found in known_hosts, error object is return, if nil then connection proceeds,
	// if not nil then connection stops.
	khFilePath := filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts")

	f, fErr := os.OpenFile(khFilePath, os.O_APPEND|os.O_WRONLY, 0600)
	if fErr != nil {
		return fErr
	}
	defer f.Close()

	knownHosts := knownhosts.Normalize(remote.String())
	_, fileErr := f.WriteString(knownhosts.Line([]string{"\n", knownHosts}, pubKey))
	return fileErr
}

type A struct {
	FirstName  *string `json:"first_name"`
	MiddleName *string `json:"mid_name"`
	LastName   *string `json:"last_name"`
}

func getRemoteType(flag string, infra *AutomteHAInfraDetails) (string, string) {
	switch strings.ToLower(flag) {
	case "opensearch", "os":
		return infra.Outputs.OpensearchPrivateIps.Value[0], "opensearch"

	case "postgresql", "pg":
		return infra.Outputs.PostgresqlPrivateIps.Value[0], "postgresql"
	default:
		return infra.Outputs.OpensearchPrivateIps.Value[0], "opensearch"
	}
}

func createTomlFile(file string, tomlOutput string) error {
	writer.Print(file)
	initConfigHAPath := file
	if _, err := os.Stat(initConfigHAPath); err == nil {
		writer.Printf("Skipping config initialization. Config already exists at %s\n", initConfigHAPath)
		return nil
	}

	err := ioutil.WriteFile(initConfigHAPath, []byte(tomlOutput), 0600)
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "Writing initial configuration failed")
	}
	writer.Printf("\nconfig initializatized in a generated file : %s\n", initConfigHAPath)
	return nil
}

func tomlToJson(rawData string) string {

	re := regexp.MustCompile("(?im).*info:.*$")
	tomlOutput := re.ReplaceAllString(rawData, "")
	// open := Open
	// toml.Decode(tomlOutput, &open)
	// dataByt, _ := json.Marshal(open)
	// fmt.Println(string(dataByt))
	// return string(dataByt)
	return tomlOutput
}

func getMergerTOMLPath(args []string, infra *AutomteHAInfraDetails, timestamp string) (error, string) {
	tomlFile := args[0] + timestamp
	sshUser := infra.Outputs.SSHUser.Value
	sskKeyFile := infra.Outputs.SSHKeyFile.Value
	sshPort := infra.Outputs.SSHPort.Value

	remoteIP, remoteService := getRemoteType(args[0], infra)
	scriptCommands := fmt.Sprintf(GET_CONFIG, remoteService)
	writer.Body(scriptCommands + "\n")
	err, rawOutput := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, remoteIP, scriptCommands)
	writer.Body("Output" + rawOutput + "\n")
	if err != nil {
		// writer.Errorf("%s", err)
		return err, ""
	}

	writer.Body(rawOutput)

	var src OpensearchConfig
	if _, err := toml.Decode(tomlToJson(rawOutput), &src); err != nil {
		// writer.Printf("%v", err)
		return err, ""
	}

	// fmt.Println("Src/Server Output: ", src)

	//  start from here
	pemBytes, err := ioutil.ReadFile(args[0])
	if err != nil {
		// writer.Printf("\n%v\n", err)
		return err, ""
	}

	destString := string(pemBytes)
	var dest OpensearchConfig
	if _, err := toml.Decode(destString, &dest); err != nil {
		// writer.Printf("%v", err)
		return err, ""
	}

	// writer.Printf("Dest/User Input: ", dest)
	mergo.Merge(&dest, src) //, mergo.WithOverride
	// writer.Printf("%v", dest)

	f, err := os.Create(tomlFile)

	if err != nil {
		// failed to create/open the file    log.Fatal(err)
		writer.Bodyf("Failed to create/open the file, \n%v", err)
		return err, ""
	}
	if err := toml.NewEncoder(f).Encode(dest); err != nil {
		// failed to encode    log.Fatal(err)
		writer.Bodyf("Failed to encode\n%v", err)
		return err, ""
	}
	if err := f.Close(); err != nil {
		// failed to close the file
		writer.Bodyf("Failed to close the file\n%v", err)
		return err, ""
	}

	return nil, tomlFile
}
