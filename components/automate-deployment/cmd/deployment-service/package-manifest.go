package main

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

func init() {
	RootCmd.AddCommand(&cobra.Command{
		Use:   "package-manifest",
		Short: "Dump the service package manifest",
		Long:  "Dump the service package manifest",
		Run:   getPackageManifest,
	})
}

func getPackageManifest(cmd *cobra.Command, args []string) {
	e := os.Stderr

	ds, err := client.Connection(1 * time.Second)
	if err != nil {
		fmt.Fprintf(e, "Failed connecting to deployment-service: %s\n", err.Error())
		os.Exit(1)
	}

	ctx := context.Background()

	res, err := ds.CurrentReleaseManifest(ctx, &api.CurrentReleaseManifestRequest{})
	if err != nil {
		fmt.Fprintf(e, "Failed to acquire current package manifest: %s\n", err.Error())
		os.Exit(1)
	}

	if _, err := os.Stdout.Write(res.Json); err != nil {
		fmt.Fprintf(e, "Failed writing package manifest: %s\n", err.Error())
		os.Exit(1)
	}

	os.Stdout.Sync()
	os.Exit(0)
}
