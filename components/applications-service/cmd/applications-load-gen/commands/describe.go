package commands

import (
	"fmt"

	"github.com/burntsushi/toml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/applications-service/pkg/generator"
)

func newDescribeCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "describe",
		Short: "explains what running a load profile will do",
		RunE:  runDescribeCmd,
	}
}

func runDescribeCmd(cmd *cobra.Command, args []string) error {
	if profileFile == "" {
		return errors.New("no profile filename given")
	}

	fmt.Printf("Reading profile %q\n", profileFile)

	var profileCfg generator.LoadProfileCfg
	_, err := toml.DecodeFile(profileFile, &profileCfg)
	if err != nil {
		fmt.Printf("Invalid load profile\nError: %s\n", err)
		return err
	}

	supGroups := profileCfg.BuildSupervisorGroups()

	for _, supGroup := range supGroups {
		fmt.Println(supGroup.PrettyStr())
	}

	return nil
}
