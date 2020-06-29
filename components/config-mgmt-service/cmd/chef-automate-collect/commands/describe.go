package commands

import "github.com/spf13/cobra"

func newDescribeCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "describe",
		Short: "Prints metadata about the Chef Infra policy to stdout",
		RunE:  runDescribeCmd,
	}
}

func runDescribeCmd(c *cobra.Command, args []string) error {
	return nil
}
