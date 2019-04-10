package main

import (
	"net/url"
	"os"

	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/runner"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"

	// This registers the diagnostics in the integration package
	_ "github.com/chef/automate/components/automate-cli/pkg/diagnostics/integration"
)

var diagnosticsCmd = &cobra.Command{
	Use:    "diagnostics COMMAND",
	Short:  "diagnostics runs integration tests",
	Hidden: true,
}

const runDiagnosticsRunLongDesc = `
This command is capable of generating data and verifying
that it made it into Chef Automate.

Diagnostics have 3 phases, each of which are optional:
- Generate: Generates data and puts it into Chef Automate. For example,
Chef Client Runs can be sent to the data-collector endpoint.
- Verify: Verifies that the data generated is available.
- Cleanup: Attempts to cleanup any data that was put into the system.

Using --skip-generate, --skip-verify, and --skip-cleanup, each of these
phases can be skipped. If a phase is skipped, a context file will
be created so that it can be loaded

This command takes an optional set of tags. If no tags are provided, all
available diagnostics are run. Otherwise, only those matching the the tag
will be run. A diagnostic matches a given tag if it is the same as its name
or is explicitly tagged with that tag.
`

var diagnosticsRunCmd = &cobra.Command{
	Use:   "run [TAG1 TAG2 TAGN...]",
	Short: "Runs diagnostic tests against Chef Automate",
	Long:  runDiagnosticsRunLongDesc,
	RunE:  runDiagnosticsRunCmd,
}

var diagnosticsRunCmdOpts = struct {
	skipGenerate bool
	skipVerify   bool
	skipCleanup  bool
	saveFilePath string
	adminToken   string
	lbURL        string
}{}

func runDiagnosticsRunCmd(cmd *cobra.Command, args []string) error {
	var tstContext diagnostics.TestContext

	dsClient, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}

	lbURL, err := url.Parse(diagnosticsRunCmdOpts.lbURL)
	if err != nil {
		return status.Wrap(
			err,
			status.InvalidCommandArgsError,
			"Parsing LB URL failed",
		)
	}

	if diagnosticsRunCmdOpts.skipGenerate {
		// Load
		file, err := os.Open(diagnosticsRunCmdOpts.saveFilePath)
		if err != nil {
			return status.Annotate(err, status.FileAccessError)
		}
		defer func() {
			_ = file.Close()
		}()
		tstContext, err = diagnostics.LoadTestContext(dsClient, file, diagnostics.WithLBURL(*lbURL))
		if err != nil {
			return status.Wrap(
				err,
				status.DiagnosticsError,
				"Failed to load test context",
			)
		}
	} else {
		tstContext = diagnostics.NewTestContext(dsClient,
			diagnostics.WithLBURL(*lbURL),
			diagnostics.WithAdminToken(diagnosticsRunCmdOpts.adminToken))
	}

	r := runner.New(
		runner.WithTestContext(tstContext),
		runner.WithSkipGeneratePhase(diagnosticsRunCmdOpts.skipGenerate),
		runner.WithSkipVerifyPhase(diagnosticsRunCmdOpts.skipVerify),
		runner.WithSkipCleanupPhase(diagnosticsRunCmdOpts.skipCleanup),
		runner.WithMatchingTags(diagnostics.StringArrayToTagFilters(args)),
	)
	if err = r.Run(); err != nil {
		return status.Annotate(err, status.DiagnosticsError)
	}

	if diagnosticsRunCmdOpts.skipVerify || diagnosticsRunCmdOpts.skipCleanup {
		file, err := os.Create(diagnosticsRunCmdOpts.saveFilePath)
		if err != nil {
			return status.Annotate(err, status.FileAccessError)
		}
		err = tstContext.WriteJSON(file)
		if err != nil {
			return status.Annotate(err, status.FileAccessError)
		}
	}
	return nil
}

func init() {
	if isDevMode() {
		diagnosticsCmd.Hidden = false
	}
	diagnosticsCmd.AddCommand(diagnosticsRunCmd)
	diagnosticsRunCmd.Flags().BoolVar(
		&diagnosticsRunCmdOpts.skipGenerate,
		"skip-generate",
		false,
		"Skips generate step")
	diagnosticsRunCmd.Flags().BoolVar(
		&diagnosticsRunCmdOpts.skipVerify,
		"skip-verify",
		false,
		"Skips verify step")
	diagnosticsRunCmd.Flags().BoolVar(
		&diagnosticsRunCmdOpts.skipCleanup,
		"skip-cleanup",
		false,
		"Skips cleanup step")
	diagnosticsRunCmd.Flags().StringVar(
		&diagnosticsRunCmdOpts.saveFilePath,
		"save-file",
		"diagnostics-context.json",
		"The saved context. This file is must already be present if the generate step is skipped.",
	)
	diagnosticsRunCmd.Flags().StringVar(
		&diagnosticsRunCmdOpts.adminToken,
		"admin-token",
		"",
		"An optional admin token to use. This option is only valid if --skip-generate is not passed.",
	)
	diagnosticsRunCmd.Flags().StringVar(
		&diagnosticsRunCmdOpts.lbURL,
		"lb-url",
		"https://localhost",
		"URL for the automate-load-balancer",
	)
	RootCmd.AddCommand(diagnosticsCmd)
}
