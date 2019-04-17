package commands

import (
	"fmt"
	"runtime"

	"github.com/chef/automate/components/applications-service/pkg/generator"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

var runFlags struct {
	authToken string
	tick      int
	verbosity int
}

func newRunCmd() *cobra.Command {
	c := &cobra.Command{
		Use:   "run",
		Short: "runs the load generator",
		RunE:  runRunCmd,
	}
	c.PersistentFlags().StringVarP(
		&runFlags.authToken,
		"auth-token",
		"t",
		"",
		"Automate auth token",
	)
	c.PersistentFlags().IntVar(
		&runFlags.tick,
		"tick",
		30,
		"interval between hab sup healthcheck ticks",
	)
	c.PersistentFlags().CountVarP(
		&runFlags.verbosity,
		"verbose",
		"V",
		"log each supervisor cycle; give twice to log every message",
	)

	return c
}

func runRunCmd(cmd *cobra.Command, args []string) error {
	if rootFlags.profileFile == "" && !rootFlags.useDefaultProfile {
		return errors.New("no profile filename given")
	}

	if runFlags.authToken == "" {
		return errors.New("no auth token given")
	}

	fmt.Printf("Reading profile %q\n", rootFlags.profileFile)

	var profileCfg *generator.LoadProfileCfg
	var err error

	if rootFlags.useDefaultProfile {
		profileCfg, err = generator.BuiltinConfig()
	} else {
		profileCfg, err = generator.ProfileFromFile(rootFlags.profileFile)
	}
	if err != nil {
		return err
	}

	runnerCfg := &generator.RunnerConfig{
		AuthToken: runFlags.authToken,
		Host:      "localhost",
		Tick:      runFlags.tick,
		Verbosity: runFlags.verbosity,
	}

	runner, err := profileCfg.BuildRunner()
	if err != nil {
		return err
	}
	runner.Run(runnerCfg)
	runtime.Goexit() // waits for all the other threads
	return nil
}
