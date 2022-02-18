// Copyright © 2017 Chef Software

package main

import (
	"context"
	"os"

	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/lib/version"
)

var versionCmd = &cobra.Command{
	Use:   "version",
	Short: "Show CLI version",
	Long:  "Show the CLI version.",
	RunE:  runVersionCmd,
	Annotations: map[string]string{
		NoRequireRootAnnotation: NoRequireRootAnnotation,
	},
}

type versionResult struct {
	ClientVersion   string `json:"client_version"`
	ClientGitSHA    string `json:"client_git_sha"`
	ManifestVersion string `json:"manifest_version"`
	ManifestGitSHA  string `json:"manifest_git_sha"`
}

var verbose bool

func runVersionCmd(cmd *cobra.Command, args []string) error {
	writer.Printf("Version: %s\n", "2")
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
	versionCmd.Flags().BoolVarP(&verbose, "verbose", "v", false, "Show additional version information")
	RootCmd.AddCommand(versionCmd)
}
