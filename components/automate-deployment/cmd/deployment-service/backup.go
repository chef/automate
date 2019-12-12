package main

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-deployment/pkg/backup"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/components/automate-deployment/pkg/toml"
)

func init() {
	RootCmd.AddCommand(backupCmd())
}

func backupCmd() *cobra.Command {
	root := &cobra.Command{
		Use: "backup",
	}

	restore := &cobra.Command{
		Use:  "restore backupRepoPath backupID",
		RunE: runRestore,
	}

	root.AddCommand(restore)
	return root
}

func runRestore(cmd *cobra.Command, args []string) error {
	locationSpec := backup.FilesystemLocationSpecification{
		Path: args[0],
	}

	b, err := backup.LoadBackup(context.Background(), args[1], locationSpec)
	if err != nil {
		return err
	}

	fmt.Printf("Restoring %q from %s\n", b.GetID(), b.GetLocationSpec())
	fmt.Println("=================")
	cfg, err := toml.Marshal(b.GetConfig())
	if err != nil {
		panic(err)
	}
	fmt.Println(string(cfg))
	mfst, err := json.MarshalIndent(b.GetManifest(), "", "    ")
	if err != nil {
		panic(err)
	}
	fmt.Println("=================")
	fmt.Println(string(mfst))

	t := target.NewLocalTarget(false)
	restorer, err := backup.NewRestorer(b, t, &backup.CLIReporter{}, backup.RestorerOpts{})
	if err != nil {
		panic(err)
	}
	if err := restorer.Restore(context.Background()); err != nil {
		panic(err)
	}
	return nil
}
