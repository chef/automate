package commands

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

func newDescribeCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "describe",
		Short: "Prints metadata about the Chef Infra policy to stdout",
		Args:  cobra.MinimumNArgs(1),
		RunE:  runDescribeCmd,
	}
}

func runDescribeCmd(c *cobra.Command, args []string) error {
	policyLockPathIn := args[0]

	meta, err := newRolloutMetadata(policyLockPathIn)
	describeCmdFailErr(err)

	err = meta.ReadPolicyfileMetadata()
	describeCmdFailErr(err)

	err = meta.ReadGitMetadata()
	describeCmdFailErr(err)

	meta.ReadCIMetadata()

	outBytes, err := json.MarshalIndent(meta, "", "  ")
	describeCmdFailErr(err)

	os.Stdout.Write(outBytes)
	fmt.Println("")

	return nil
}

func describeCmdFailErr(err error) {
	if err != nil {
		cliIO.msg("Error: %s", err.Error())
		os.Exit(1)
	}
}
