package main

import (
	"fmt"
	"io/ioutil"

	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

type certCmdFlagSet struct {
	hostname string
	filepath string
}

var certCmdFlags = certCmdFlagSet{}

func init() {

	certCmd := certCmd()

	certCmd.PersistentFlags().StringVarP(
		&certCmdFlags.hostname,
		"hostname",
		"n",
		"",
		"Hostname for the automate TLS certificate",
	)
	certCmd.PersistentFlags().StringVarP(
		&certCmdFlags.filepath,
		"file",
		"f",
		"",
		"File path to save automate TLS certifcate to.",
	)
	RootCmd.AddCommand(certCmd)
}

func certCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "external-cert COMMAND",
		Short: "Manage Chef Automate's external certificate",
		Long:  "Manage Chef Automate's external certificate authority. Used for establishing TLS/SSL communication with automate.",
	}

	certShow := &cobra.Command{
		Use:   "show",
		Short: "Show the external TLS/SSL certificates in Automate. Optionally, save the certificates to a file in the specified path.",
		RunE:  runCertShowCmd,
		Args:  cobra.MaximumNArgs(2),
	}

	cmd.AddCommand(certShow)

	return cmd
}

func runCertShowCmd(*cobra.Command, []string) error {
	res, err := client.GetAutomateConfig(int64(client.DefaultClientTimeout))
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connecting to deployment-service failed",
		)
	}
	tlsCreds := res.Config.GetGlobal().GetV1().GetFrontendTls()

	if certCmdFlags.hostname != "" && certCmdFlags.filepath != "" {
		for _, tlsCred := range tlsCreds {
			if tlsCred.GetServerName() == certCmdFlags.hostname {
				return writeToFile(tlsCred.GetCert())
			}
		}
		// Error no matching hostname found in the automate frontend_tls cert configuration
		return status.Wrap(
			err,
			status.CommandExecutionError,
			fmt.Sprintf("Unable to find matching certificate configuration for hostname %s", certCmdFlags.filepath),
		)
	} else if certCmdFlags.hostname != "" {
		found := false
		for _, tlsCred := range tlsCreds {
			if tlsCred.GetServerName() == certCmdFlags.hostname {
				found = true
				writer.Printf("Hostname: %s\nCert: %s\n", tlsCred.GetServerName(), tlsCred.GetCert())
				return nil
			}
		}
		if !found {
			// Error no matching hostname found in the automate frontend_tls cert configuration
			return status.Wrap(
				err,
				status.CommandExecutionError,
				fmt.Sprintf("Unable to find matching certificate configuration for hostname %s", certCmdFlags.filepath),
			)
		}
	} else if certCmdFlags.filepath != "" {
		if len(tlsCreds) > 1 {
			// There is more than one cert and it is unclear which one to save to the cert file
			return status.Wrap(
				err,
				status.CommandExecutionError,
				fmt.Sprintf("Found more than one host certificate configured, please specify a hostname using the -hostname flag"),
			)
		}
		return writeToFile(tlsCreds[0].GetCert())

	} // Print all certs to stdout
	for _, tlsCred := range tlsCreds {
		if tlsCred.GetServerName() != "" {
			writer.Printf("Hostname: %s\nCertificate:\n %s\n", tlsCred.GetServerName(), tlsCred.GetCert())
		} else {
			// Don't print the hostname if it isn't there
			// It is most likely not set, so let's keep it simple
			writer.Printf("Certificate:\n %s\n", tlsCred.GetCert())
		}
	}
	return nil
}

func writeToFile(cert string) error {
	err := ioutil.WriteFile(certCmdFlags.filepath, []byte(cert), 0644)
	if err != nil {
		return status.Wrap(
			err,
			status.FileAccessError,
			fmt.Sprintf("Unable to write to file %s", certCmdFlags.filepath),
		)
	}
	return nil
}
