package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

func init() {
	RootCmd.AddCommand(dumpdbCmd())
}

func dumpdbCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "dumpdb",
		Short: "Dump deployment-service's database to stdout",
		Long: `Dump deployment-service's database to stdout

Environment:
  DEPLOYMENT_SERVICE_ADDRESS  set to a string like localhost:10160 to
                              configure the host and port
`,
		Run: runDumpDB,
	}
}

func runDumpDB(cmd *cobra.Command, args []string) {
	// Data is printed to stdout, so the command will commonly be run as
	// deployment-service dumpdb > backup.db
	// This means we need to print all other things to stderr
	e := os.Stderr

	ds, err := client.Connection(1 * time.Second)
	if err != nil {
		fmt.Fprintf(e, "Error connecting to deployment-service: %s\n", err.Error())
		os.Exit(1)
	}

	ctx := context.Background()

	stream, err := ds.DumpDB(ctx, &api.DumpDBRequest{})
	if err != nil {
		fmt.Fprintf(e, "Error connecting to dumpdb API: %s\n", err.Error())
		os.Exit(1)
	}

	// Write stream to stdout
	for {
		resp, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			err = errors.Wrap(err, "Error downloading database")
			fmt.Fprint(e, err.Error())
			os.Exit(1)
		}

		data := resp.GetData()
		if data == nil {
			break
		}
		if _, err := os.Stdout.Write(data); err != nil {
			err = errors.Wrap(err, "Error writing database to stdout")
			fmt.Fprint(e, err.Error())
			os.Exit(1)
		}
	}

	os.Stdout.Sync()
	os.Exit(0)

}
