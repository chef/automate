package main

import (
	"errors"
	"io/ioutil"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

var promptCert = `
I agree to rotate Certificates
`
var errCert = `
Provide valid arguments
`

var sshFlag = struct {
	hostname bool
	//node     int64
}{}
var rootCaFlags = struct {
	rootCa      string
	privateCert string
	publicCert  string
	acceptCert  bool
}{}

var certRotation = &cobra.Command{
	Use:   "cert-rotate",
	Short: "Chef Automate rotate cert",
	Long:  "Chef Automate CLI command to rotate certificates",
	RunE:  certRotateCmd,
}

func init() {

	RootCmd.AddCommand(certRotation)
	certRotation.PersistentFlags().BoolVar(&sshFlag.hostname, "pg", false, "Automate ha server name to ssh")
	// we can add node name or node IpAddress as well.
	// certRotation.PersistentFlags().Int64Var(&sshFlag.node, "node", -1, "Automate ha node name to ssh")
	certRotation.PersistentFlags().StringVar(&rootCaFlags.rootCa, "root-ca", "", "Automate Root CA value")
	certRotation.PersistentFlags().StringVar(&rootCaFlags.privateCert, "private-cert", "", "Automate ha private certificate")
	certRotation.PersistentFlags().StringVar(&rootCaFlags.publicCert, "public-cert", "", "Automate ha public certificate")

}

func certRotateCmd(cmd *cobra.Command, args []string) error {
	if !isA2HARBFileExist() {
		return errors.New(AUTOMATE_HA_INVALID_BASTION)
	}
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return err
	}
	sshStrings, err := getIPOfRequestedServers(sshFlags.hostname, infra)
	if err != nil {
		return err
	}
	/*if sshFlag.node > 0 && sshFlag.node < int64(len(sshStrings)) {
	writer.Prompt(sshStrings[sshFlag.node-1])
	} else {
	return status.New(status.InvalidCommandArgsError, errCert)
	}*/
	idx, err := writer.Prompt(strings.Join(sshStrings, "\n"))
	if err != nil {
		return err
	}
	rootCaString, err := ioutil.ReadFile(rootCaFlags.rootCa)
	if err != nil {
		return errors.New("could not find root cert file")
	}

	if !rootCaFlags.acceptCert {
		agree, err := writer.Confirm(promptCert)
		if err != nil {
			return status.Wrap(err, status.InvalidCommandArgsError, errCert)
		}

		if !agree {
			return status.New(status.InvalidCommandArgsError, errCert)
		}
	}

	return nil
}

// if sshFlags.node < 1 || sshFlags.node > len(sshStrings) {
// 	return errors.New("invalid input it should be between 1  to ")
// }
