package commands

import (
	"fmt"
	"runtime"

	"github.com/chef/automate/components/applications-load-gen/pkg/generator"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

var runFlags struct {
	authToken string
	host      string
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
	c.PersistentFlags().StringVarP(
		&runFlags.host,
		"host",
		"H",
		"localhost",
		"Automate hostname",
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
	if err := rootFlags.ValidateProfileOpts(); err != nil {
		return err
	}

	if runFlags.authToken == "" {
		return errors.New("no auth token given")
	}

	fmt.Printf("Loading profile %q\n", rootFlags.SelectedProfile())

	profileCfg, err := rootFlags.LoadProfileCfg()

	if err != nil {
		return err
	}

	runnerCfg := &generator.RunnerConfig{
		AuthToken: runFlags.authToken,
		Host:      runFlags.host,
		Tick:      runFlags.tick,
		Verbosity: runFlags.verbosity,
	}

	runner, err := profileCfg.BuildRunner()
	if err != nil {
		return err
	}
	runner.SupervisorGroups.ReScaleTo(rootFlags.svcCount)
	runner.Run(runnerCfg)
	runtime.Goexit() // waits for all the other threads
	return nil
}
