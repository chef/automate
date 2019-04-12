package commands

import (
	"fmt"
	"runtime"

	"github.com/burntsushi/toml"
	"github.com/chef/automate/components/applications-service/pkg/generator"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

var authToken string
var tick int

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

	return c
}

func runRunCmd(cmd *cobra.Command, args []string) error {
	if profileFile == "" {
		return errors.New("no profile filename given")
	}

	if authToken == "" {
		return errors.New("no auth token given")
	}

	fmt.Printf("Reading profile %q\n", profileFile)

	var profileCfg generator.LoadProfileCfg
	_, err := toml.DecodeFile(profileFile, &profileCfg)
	if err != nil {
		fmt.Printf("Invalid load profile\nError: %s\n", err)
		return err
	}

	runnerCfg := &generator.RunnerConfig{
		AuthToken: authToken,
		Host:      "localhost",
		Tick:      tick,
	}

	runner := profileCfg.BuildRunner()
	runner.Run(runnerCfg)
	runtime.Goexit() // waits for all the other threads
	return nil
}
