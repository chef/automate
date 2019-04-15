package commands

import (
	"fmt"
	"runtime"

	"github.com/chef/automate/components/applications-service/pkg/generator"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

var authToken string
var tick int
var verbosity int

func newRunCmd() *cobra.Command {
	c := &cobra.Command{
		Use:   "run",
		Short: "runs the load generator",
		RunE:  runRunCmd,
	}
	c.PersistentFlags().StringVarP(
		&authToken,
		"auth-token",
		"t",
		"",
		"Automate auth token",
	)
	c.PersistentFlags().IntVar(
		&tick,
		"tick",
		30,
		"interval between hab sup healthcheck ticks",
	)
	c.PersistentFlags().CountVarP(
		&verbosity,
		"verbose",
		"V",
		"log each supervisor cycle; give twice to log every message",
	)

	return c
}

func runRunCmd(cmd *cobra.Command, args []string) error {
	if profileFile == "" && !useDefaultProfile {
		return errors.New("no profile filename given")
	}

	if authToken == "" {
		return errors.New("no auth token given")
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

	runnerCfg := &generator.RunnerConfig{
		AuthToken: authToken,
		Host:      "localhost",
		Tick:      tick,
		Verbosity: verbosity,
	}

	runner, err := profileCfg.BuildRunner()
	if err != nil {
		return err
	}
	runner.Run(runnerCfg)
	runtime.Goexit() // waits for all the other threads
	return nil
}
