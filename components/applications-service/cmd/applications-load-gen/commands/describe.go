package commands

import (
	"fmt"

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
	if profileFile == "" && !useDefaultProfile {
		return errors.New("no profile filename given")
	}

	fmt.Printf("Reading profile %q\n", profileFile)

	var profileCfg *generator.LoadProfileCfg
	var err error

	if useDefaultProfile {
		profileCfg, err = generator.BuiltinConfig()
	} else {
		profileCfg, err = generator.ProfileFromFile(profileFile)
	}
	if err != nil {
		return err
	}

	supGroups, err := profileCfg.BuildSupervisorGroups()
	if err != nil {
		return err
	}

	for _, supGroup := range supGroups {
		fmt.Print(supGroup.PrettyStr())
	}

	return nil
}
