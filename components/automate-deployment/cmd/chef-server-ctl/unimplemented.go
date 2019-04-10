// Copyright © 2018 Chef Software

package main

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

func init() {
	for _, c := range chefServerCtlUnimplementedCmds() {
		RootCmd.AddCommand(c)
	}
}

const unsupportedCmdErr = `Most likely you are running this by mistake :(

This copy of chef-server-ctl exists to support the
'chef-automate upgrade-from-v1 --self-test' mode. It is here to provide a stub
implementation of the 'chef-server-ctl stop' subcommand only. You probably
meant to run the copy of 'chef-server-ctl' provided by A2's Chef Server
integration. When everything is working correctly, you can find that copy of
'chef-server-ctl' in /bin or /hab/bin.
`

func chefServerCtlUnimplementedCmds() []*cobra.Command {
	subcommandNames := []string{
		"test",
		"org-create",
		"org-list",
		"org-show",
		"user-create",
		"user-list",
		"user-show",
		"org-user-add",
		"version",
		"org-user-remove",
		"add-user-key",
		"list-user-keys",
		"delete-user-key",
		"grant-server-admin-permissions",
		"list-server-admins",
		"remove-server-admin-permissions",
		"cleanup-bifrost",
		"filtered-dump",
		"reindex -a",
		"user-edit",
		"add-client-key",
		"list-client-keys",
		"delete-client-key",
		"user-delete",
		"org-delete",
	}
	subcommands := []*cobra.Command{}

	for _, name := range subcommandNames {
		cmd := &cobra.Command{
			Use:   name,
			Short: "Not implemented by this version of chef-server-ctl: always exits 1",
			Long:  unsupportedCmdErr,
			Run:   unimplementedCmd,
		}
		subcommands = append(subcommands, cmd)
	}
	return subcommands
}

func unimplementedCmd(cmd *cobra.Command, args []string) {
	fmt.Println(unsupportedCmdErr)
	os.Exit(1)
}
