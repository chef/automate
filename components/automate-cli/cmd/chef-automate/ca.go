package main

import (
	"context"
	"fmt"
	"time"

	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/certauthority"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

func init() {
	RootCmd.AddCommand(caCmd())
}

func caCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "internal-ca COMMAND",
		Short: "Manage Chef Automate's internal certificate authority",
		Long:  "Manage Chef Automate's internal certificate authority. Used for inter-service encryption and authentication.",
	}

	caInfo := &cobra.Command{
		Use:   "info",
		Short: "Print information the root certificate for the internal certificate authority",
		RunE:  runCAInfoCmd,
		Args:  cobra.MaximumNArgs(0),
	}

	regen := &cobra.Command{
		Use:   "regenerate",
		Short: "Commands to regenerate certificates issued by the internal certificate authority",
	}

	regenRoot := &cobra.Command{
		Use:   "root",
		Short: "Regenerate the root certificate for the internal certificate authority",
		RunE:  runRegenRootCmd,
		Args:  cobra.MaximumNArgs(0),
	}

	regen.AddCommand(regenRoot)

	cmd.AddCommand(regen)
	cmd.AddCommand(caInfo)
	return cmd
}

func runCAInfoCmd(*cobra.Command, []string) error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connecting to deployment-service failed",
		)
	}

	resp, err := connection.GetRootCert(context.Background(), &api.RootCertRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Failed to query certificate authority",
		)
	}

	cert, err := certauthority.PEMToCert(resp.Cert)
	if err != nil {
		return status.Wrap(
			err,
			status.UnknownError,
			"could not parse root certificate",
		)
	}

	writer.Printf("         Authority Name: %s\n", cert.Subject)
	writer.Printf("Root CA Expiration Date: %s (%s from now)\n", cert.NotAfter, prettyDuration(time.Until(cert.NotAfter)))
	return nil
}

func prettyDuration(d time.Duration) string {
	if d > 24*time.Hour {
		return fmt.Sprintf("%dd", int(d.Hours()/24))
	}

	return d.String()
}

func runRegenRootCmd(*cobra.Command, []string) error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connecting to deployment-service failed",
		)
	}

	_, err = connection.RegenerateRoot(context.Background(), &api.RegenerateRootRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Failed to regenerate certificate authority",
		)
	}

	writer.Title("Certificate authority regenerated.")
	return nil
}
