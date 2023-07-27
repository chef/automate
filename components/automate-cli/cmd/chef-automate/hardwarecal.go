package main

import (
	"os"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/lib/hardwarecal"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/pmt"
	"github.com/spf13/cobra"
)

func init() {
	RootCmd.AddCommand(hardwareCalCmd())
}

func hardwareCalCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "hardware-cal COMMAND",
		Short: "Estimate Hardware Requirements for given inputs, this calculator is for Chef Automate HA",
		Long:  "Estimate Hardware Requirements for given inputs, this calculator is for Chef Automate HA",
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
		Hidden: true,
	}

	run := &cobra.Command{
		Use:   "run",
		Short: "This will trigger the hardware calculator to ask input values, based on which it will provide hardware requirements for Chef Automate HA",
		Long:  "This will trigger the hardware calculator to ask input values, based on which it will provide hardware requirements for Chef Automate HA",
		RunE:  runCal,
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
		Hidden: true,
	}

	cmd.AddCommand(run)
	return cmd
}

func runCal(*cobra.Command, []string) error {
	cw := writer
	fsu := &fileutils.FileSystemUtils{}
	p := pmt.PromptFactory(os.Stdin, os.Stdout, fsu)
	hc := hardwarecal.NewHardwareCalPrompt(p, cw)
	err := hc.Run()
	if err != nil {
		return status.Wrap(
			err,
			status.HardwareCalError,
			"Failed to Estimate Hardware Requirements",
		)
	}

	cw.Println("* These numbers are just for estimation, based on our test environment conditions. \n* Please do your performance analysis based on your environment and usages.")
	return nil
}
