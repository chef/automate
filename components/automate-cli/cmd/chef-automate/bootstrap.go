package main

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/spf13/cobra"
)

func newBootstrapBundleCmd() *cobra.Command {
	var bootstrapCmd = &cobra.Command{
		Use:    "bootstrap COMMAND",
		Hidden: true,
	}

	var bundleCmd = &cobra.Command{
		Use:    "bundle COMMAND",
		Hidden: true,
	}

	var createCmd = &cobra.Command{
		Use: "create <current working dir>/bootstrap_bundle.tar",
		Annotations: map[string]string{
			NoCheckVersionAnnotation: NoCheckVersionAnnotation,
			NoRequireRootAnnotation:  NoRequireRootAnnotation,
		},
		RunE:   runBootstrapBundleCreate,
		Hidden: true,
	}

	bootstrapCmd.AddCommand(bundleCmd)
	bundleCmd.AddCommand(createCmd)

	return bootstrapCmd
}

func runBootstrapBundleCreate(cmd *cobra.Command, args []string) error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		fmt.Println("Connection failed")
		return err
	}

	stream, err := connection.BootstrapBundle(context.Background(), &api.BootstrapBundleRequest{})
	if err != nil {
		return status.WithRecovery(
			status.Wrap(err, status.DeploymentServiceCallError, "Request to download bootstrap bundle failed"),
			recoveryMsg,
		)
	}

	cwd, err := os.Getwd()
	if err != nil {
		return status.Annotate(err, status.FileAccessError)
	}
	downloadedBundlePath := filepath.Join(cwd, "bootstrap-bundle.tar")
	downloadedBundleFile, err := os.OpenFile(downloadedBundlePath, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0600)
	if err != nil {
		return status.Annotate(err, status.FileAccessError)
	}

	defer downloadedBundleFile.Close()

	w := bufio.NewWriter(downloadedBundleFile)
	for {
		resp, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			return status.WithRecovery(
				status.Wrap(err, status.DeploymentServiceCallError, "Request to download bootstrap bundle failed"),
				recoveryMsg,
			)
		}

		data := resp.GetData()
		if len(data) <= 0 {
			continue
		}
		if _, err := w.Write(data); err != nil {
			return status.Annotate(err, status.FileAccessError)
		}
	}

	w.Flush()
	writer.Printf("Bootstrap bundle written to: %s\n", downloadedBundlePath)

	return nil
}

func init() {
	RootCmd.AddCommand(newBootstrapBundleCmd())
}
