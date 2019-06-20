package main

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/chef/automate/components/automate-deployment/pkg/bootstrap"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/spf13/cobra"
)

var bootstrapBundleCmdFlags = struct {
	overwriteFile bool
	rootDir       string
}{}

func newBootstrapBundleCmd() *cobra.Command {
	bootstrapCmd := &cobra.Command{
		Use:    "bootstrap COMMAND",
		Hidden: true,
	}

	bundleCmd := &cobra.Command{
		Use:    "bundle COMMAND",
		Hidden: true,
	}

	createCmd := &cobra.Command{
		Use:    "create [/path/to/bundle.abb]",
		Args:   cobra.RangeArgs(0, 1),
		RunE:   runBootstrapBundleCreate,
		Hidden: true,
	}

	createCmd.PersistentFlags().BoolVarP(
		&bootstrapBundleCmdFlags.overwriteFile,
		"overwrite",
		"o",
		false,
		"Overwrite existing bootstrap bundle file if one exists",
	)

	unpackCmd := &cobra.Command{
		Use:    "unpack /path/to/bundle.tar",
		Args:   cobra.ExactArgs(1),
		RunE:   runBootstrapBundleUnpack,
		Hidden: true,
	}

	unpackCmd.PersistentFlags().StringVar(
		&bootstrapBundleCmdFlags.rootDir,
		"root-dir",
		"",
		"Set the directory where the bundle will be unpacked",
	)

	bootstrapCmd.AddCommand(bundleCmd)
	bundleCmd.AddCommand(createCmd)
	bundleCmd.AddCommand(unpackCmd)

	return bootstrapCmd
}

func runBootstrapBundleCreate(cmd *cobra.Command, args []string) error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return status.Wrap(err, status.APIUnreachableError, "Failed to create a connection")
	}

	// TODO: add timeout
	stream, err := connection.BootstrapBundle(context.Background(), &api.BootstrapBundleRequest{})
	if err != nil {
		return status.WithRecovery(
			status.Wrap(err, status.DeploymentServiceCallError, "Request to download bootstrap bundle failed"),
			recoveryMsg,
		)
	}

	outfile := ""
	if len(args) > 0 && args[0] != "" {
		outfile, err = filepath.Abs(args[0])
		if err != nil {
			return status.Annotate(err, status.FileAccessError)
		}
	} else {
		cwd, err := os.Getwd()
		if err != nil {
			return status.Annotate(err, status.FileAccessError)
		}
		outfile = filepath.Join(cwd, "bootstrap-bundle.abb")
	}

	if _, err := os.Stat(outfile); err == nil {
		if !bootstrapBundleCmdFlags.overwriteFile {
			ok, err := writer.Confirm(fmt.Sprintf("%s file already exists. Do you wish to overwrite it?", outfile))
			if err != nil {
				return status.Annotate(err, status.FileAccessError)
			}
			if !ok {
				return status.New(status.FileAccessError, "Bootstrap bundle cannot be overwritten")
			}
		}
	}

	downloadedBundleFile, err := os.OpenFile(outfile, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0600)
	if err != nil {
		return status.Annotate(err, status.FileAccessError)
	}

	defer downloadedBundleFile.Close() // nolint: errcheck

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

	err = w.Flush()
	if err != nil {
		return status.Annotate(err, status.FileAccessError)
	}
	writer.Printf("Bootstrap bundle written to: %s\n", outfile)

	return nil
}

func runBootstrapBundleUnpack(cmd *cobra.Command, args []string) error {
	b := bootstrap.NewBundleCreator(
		bootstrap.WithBundleCreatorRootDir(bootstrapBundleCmdFlags.rootDir))
	f, err := os.Open(args[0])
	if err != nil {
		return status.Wrapf(err, status.FileAccessError, "could not open %q", args[0])
	}
	defer f.Close()
	if err := b.Unpack(f); err != nil {
		return err
	}
	return nil
}

func init() {
	RootCmd.AddCommand(newBootstrapBundleCmd())
}
