package main

import (
	"fmt"
	"io"
	"os"
	"path"

	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-deployment/pkg/server"
)

func init() {
	RootCmd.AddCommand(restoreDBCmd())
}

func restoreDBCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "restoredb CONFIG_FILE",
		Short: "Restore deployment-service's database from stdin stream of a dump",
		Long:  "Restore deployment-service's database from stdin stream of a dump",
		Run:   runRestoreDB,
	}
}

func runRestoreDB(cmd *cobra.Command, args []string) {
	e := os.Stderr

	dbFile := path.Join(server.DataDir, server.DBName)
	f, err := os.OpenFile(dbFile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0600)

	if err != nil {
		fmt.Fprintf(e, "failed to open db file %s for writing: %v", dbFile, err)
		os.Exit(1)
	}
	defer f.Close()

	if _, err = io.Copy(f, os.Stdin); err != nil {
		fmt.Fprintf(e, "error copying backup data from stdin to db file: %v", err)
		os.Exit(1)
	}

	err = f.Sync()
	if err != nil {
		fmt.Fprintf(e, "WARN: sync failed: %s\n", err.Error())
	}
	fmt.Fprintf(e, "Backup data restored to file: %s\n", dbFile)

	os.Exit(0)

}
