package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"golang.org/x/crypto/ssh"
	"golang.org/x/crypto/ssh/knownhosts"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

var configCmdFlags = struct {
	overwriteFile bool
	timeout       int64
	acceptMLSA    bool

	// automate    bool
	// chef_server bool
	frontend   bool
	opensearch bool
	postgresql bool
}{}

func init() {
	configCmd.AddCommand(showConfigCmd)
	configCmd.AddCommand(patchConfigCmd)
	configCmd.AddCommand(setConfigCmd)

	showConfigCmd.Flags().BoolVarP(&configCmdFlags.overwriteFile, "overwrite", "o", false, "Overwrite existing config.toml")

	// patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.automate, "automate", "a", false, "Patch toml configuration to the automate node")
	// patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.chef_server, "chef_server", "c", false, "Patch toml configuration to the chef_server node")
	patchConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.frontend, "frontend", false, "Patch toml configuration to the chef_server node")
	patchConfigCmd.PersistentFlags().BoolVar(&configCmdFlags.frontend, "fe", false, "Patch toml configuration to the chef_server node")
	patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.opensearch, "opensearch", "o", false, "Patch toml configuration to the opensearch node")
	patchConfigCmd.PersistentFlags().BoolVarP(&configCmdFlags.postgresql, "postgresql", "p", false, "Patch toml configuration to the postgresql node")

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

		if configCmdFlags.frontend {
			frontendIps := append(infra.Outputs.ChefServerPrivateIps.Value, infra.Outputs.AutomatePrivateIps.Value...)
			writer.Printf("IPs: " + strings.Join(frontendIps, "") + "Path :" + args[0])
			for i := 0; i < len(frontendIps); i++ {
				executePatchOnRemote(sshUser, sshPort, sskKeyFile, frontendIps[i], args[0], "fe")
			}
		}
		if configCmdFlags.postgresql {
			for i := 0; i < 1; i++ {
				executePatchOnRemote(sshUser, sshPort, sskKeyFile, infra.Outputs.PostgresqlPrivateIps.Value[i], args[0], "pg")
			}
		}
		if configCmdFlags.opensearch {
			for i := 0; i < 1; i++ {
				executePatchOnRemote(sshUser, sshPort, sskKeyFile, infra.Outputs.OpensearchPrivateIps.Value[i], args[0], "os")
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

func executePatchOnRemote(sshUser string, sshPort string, sshKeyFile string, ip string, path string, remoteType string) {

	pemBytes, err := ioutil.ReadFile(sshKeyFile)
	if err != nil {
		writer.Errorf("Unable to read private key: %v", err)
	}
	signer, err := ssh.ParsePrivateKey(pemBytes)
	if err != nil {
		writer.Errorf("Parsing key failed: %v", err)
	}
	// _, err := kh.New("/root/.ssh/known_hosts")
	if err != nil {
		writer.Errorf("could not create hostkeycallback function: ", err)
	}
	var (
		keyErr *knownhosts.KeyError
	)

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
				writer.Printf("WARNING: %s is not trusted, adding this key to known_hosts file.", host)
				return addHostKey(host, remote, pubKey)
			}
			writer.Printf("Pub key exists for %s.", host)
			return nil
		}),
	}

	// Open connection
	conn, err := ssh.Dial("tcp", ip+":"+sshPort, config)
	if conn == nil || err != nil {
		writer.Errorf("dial failed:%v", err)
		return
	}
	defer conn.Close()

	// Open session
	session, err := conn.NewSession()
	if err != nil {
		writer.Errorf("session failed:%v", err)
	}
	var stdoutBuf bytes.Buffer
	session.Stdout = &stdoutBuf
	// err = session.Run("sudo rm -rf /tmp/" + path + "")
	cmd := "scp"
	exec_args := []string{"-i", sshKeyFile, "-r", path, sshUser + "@" + ip + ":/tmp/"}
	if err := exec.Command(cmd, exec_args...).Run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	// writer.Printf("Executing patch command on IP: " + ip)
	writer.StartSpinner()
	if remoteType == "fe" {
		err = session.Run("sudo chef-automate config patch /tmp/" + path +
			"\n")
	} else if remoteType == "pg" {
		// err = session.Run("sudo chef-automate config patch  /tmp/" + path + "")
		err = session.Run("export HAB_LICENSE=accept-no-persist; echo \"yes\" | sudo hab config apply automate-ha-postgresql.default  $(date '+%s') /tmp/" + path + "\n")
	} else {
		err = session.Run("export HAB_LICENSE=accept-no-persist; echo \"yes\" | sudo hab config apply automate-ha-opensearch.default $(date '+%s') /tmp/" + path + "\n")
	}
	writer.StopSpinner()
	if err != nil {
		writer.Errorf("Run failed:%v", err)
	} else {
		writer.Success("SCP successful...\n")
	}
	defer session.Close()
	writer.Printf("\n%s\n", stdoutBuf)
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

func createKnownHosts() {
	f, fErr := os.OpenFile(filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts"), os.O_CREATE, 0600)
	if fErr != nil {
		writer.Errorf("%v", fErr)
	}
	f.Close()
}

func checkKnownHosts() ssh.HostKeyCallback {
	createKnownHosts()
	kh, e := knownhosts.New(filepath.Join(os.Getenv("HOME"), ".ssh", "known_hosts"))
	if e != nil {
		writer.Errorf("%v", e)
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
	_, fileErr := f.WriteString(knownhosts.Line([]string{knownHosts}, pubKey))
	return fileErr
}
