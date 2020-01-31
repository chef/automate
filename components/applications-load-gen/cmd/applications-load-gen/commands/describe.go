package commands

import (
	"fmt"

	"github.com/spf13/cobra"
)

func newDescribeCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "describe",
		Short: "explains what running a load profile will do",
		RunE:  runDescribeCmd,
	}
}

func runDescribeCmd(cmd *cobra.Command, args []string) error {
	if err := rootFlags.ValidateProfileOpts(); err != nil {
		return err
	}

	fmt.Printf("Loading profile %q\n", rootFlags.SelectedProfile())

	profileCfg, err := rootFlags.LoadProfileCfg()
	if err != nil {
		return err
	}

	supGroups, err := profileCfg.BuildSupervisorGroups()
	if err != nil {
		return err
	}

	supGroups.ReScaleTo(rootFlags.svcCount)

	fmt.Print(supGroups.RollupStats())

	for _, supGroup := range supGroups {
		fmt.Print(supGroup.PrettyStr())
	}

	return nil
}
