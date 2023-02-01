package main

import (
	"fmt"
	"strings"

	"github.com/spf13/cobra"
	"github.com/spf13/pflag"
)

func CommandBuilder(cmd *cobra.Command, args []string) string {
	fullCommand := cmd.CommandPath()
	cmd.Flags().VisitAll(func(flag *pflag.Flag) {
		if flag.Changed {
			fullCommand += " --" + flag.Name + " " + flag.Value.String()
		}
	})
	return fmt.Sprint(fullCommand + " " + strings.Join(args, " "))
}
