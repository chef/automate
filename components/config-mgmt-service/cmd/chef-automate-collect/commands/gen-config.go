package commands

import (
	"os"

	"github.com/BurntSushi/toml"
	"github.com/spf13/cobra"
)

var genConfigCommands = struct {
	verbose            bool
	insecureConnection bool
	privateConfig      bool
	writeRepoConfig    bool
}{}

func newGenConfigCommand() *cobra.Command {
	c := &cobra.Command{
		Use:   "gen-config AUTOMATE_URL AUTOMATE_AUTH_TOKEN",
		Short: "verify config parameters and emit config",
		Long: `
		gen-config makes a request to Automate to verify the Automate URL and
		token. If successful, it emits 
		`,
		Args: cobra.ExactArgs(2),
		RunE: runGenConfigCommand,
	}
	c.Flags().BoolVarP(&genConfigCommands.verbose,
		"verbose",
		"v",
		false,
		"enable verbose output (goes to stderr)",
	)
	c.Flags().BoolVarP(&genConfigCommands.insecureConnection,
		"insecure",
		"k",
		false,
		"do not verify the Automate host's TLS certificates/hostname",
	)
	c.Flags().BoolVarP(&genConfigCommands.privateConfig,
		"private",
		"p",
		false,
		"emit private config with credentials",
	)
	c.Flags().BoolVar(&genConfigCommands.writeRepoConfig,
		"repo",
		false,
		"write config suitable for use in a shared SCM repo",
	)
	return c
}

func runGenConfigCommand(c *cobra.Command, args []string) error {
	automateURLIn := args[0]
	token := args[1]

	cliIO := CLIIO{}

	ac, err := newAutomateConfig(automateURLIn, token)
	if err != nil {
		return err
	}
	ac.InsecureTLS = genConfigCommands.insecureConnection
	conf := &Config{
		Automate: []*AutomateConfig{ac},
	}

	err = ac.Test()
	if err != nil {
		return err
	}

	cliIO.msg("# Config verified by successful request to %q\n", ac.TestURL)

	// if --repo was passed, write repo config files
	if genConfigCommands.writeRepoConfig {
		err := conf.WriteRepoConfigFiles()
		if err != nil {
			return err
		}
	}

	if !genConfigCommands.writeRepoConfig {
		var confToEmit AutomateCollectorConfig
		confToEmit = conf

		if genConfigCommands.privateConfig {
			confToEmit = conf.WithPrivate()
		}

		enc := toml.NewEncoder(os.Stdout)
		enc.Encode(confToEmit)
	}

	return nil
}
