package main

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

func commandRoot() *cobra.Command {
	rootCmd := &cobra.Command{
		Use: "users",
		Run: func(cmd *cobra.Command, args []string) {
			if err := cmd.Help(); err != nil {
				/* TODO (tyler) this gives an error:
				   `Errors unhandled.,LOW,HIGH (gas)`
				   We should probably fix it since gas is a security linter,
				   but I don't have enough context to fix it now or know why
				   it's failing. */
				fmt.Fprintln(os.Stderr, err.Error()) // #nosec
				os.Exit(2)
			}
			os.Exit(1)
		},
	}
	rootCmd.AddCommand(commandServe())
	return rootCmd
}

func main() {
	if err := commandRoot().Execute(); err != nil {
		/* TODO (tyler) this gives an error:
		   `Errors unhandled.,LOW,HIGH (gas)`
		   We should probably fix it since gas is a security linter,
		   but I don't have enough context to fix it now or know why
		   it's failing. */
		fmt.Fprintln(os.Stderr, err.Error()) // #nosec
		os.Exit(2)
	}
}
