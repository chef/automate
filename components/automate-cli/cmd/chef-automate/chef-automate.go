// Copyright © 2017 Chef Software

// package main contains all the logic for parsing command line options/arguments
// using Cobra. It also parses config files into the appropriate structs.
package main

import (
	"context"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-cli/pkg/selfupdater"
	"github.com/chef/automate/components/automate-cli/pkg/selfupdater/executablecache"
	"github.com/chef/automate/components/automate-cli/pkg/selfupdater/updatesource"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/lib/platform/sys"
	"github.com/chef/automate/lib/version"
)

const (
	// NoCheckVersionAnnotation is a cobra key for group annotations
	NoCheckVersionAnnotation = "no-check-version"
	// NoRequireRootAnnotation is a cobra key for group annotations
	NoRequireRootAnnotation = "no-require-root"
)

var writer *cli.Writer
var executableCache executablecache.ExecutableCache

// This is nil by default... we set it in the tests so we don't
// need to connect to a real server
var injectedVersionClient client.ManifestVersionClient

type globalOptions struct {
	debug          bool
	noCheckVersion bool
	resultJSON     string
}

var globalOpts = &globalOptions{}

// RootCmd represents the base command when called without any subcommands
var RootCmd = newRootCmd()

func init() {
	writer = cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
	executableCache = executablecache.New()
}

func main() {
	// Set umask for our process immediately
	sys.SetUmask(022)
	if disableAutoUpdate() {
		Execute()
	} else {
		// If autoupdates have not been disabled, we will start by getting on
		// the latest executable we have in our cache.
		EarlyUpdate()
	}
}

// EarlyUpdate runs the latest available executable in the cache
func EarlyUpdate() {
	updateSource := updatesource.Local(executableCache)
	err := nextCLIOrElse(updateSource, true, func(err error) {
		// If we fail to get a new cli, we'll execute what we have
		Execute()
	})
	// Normally, we wouldn't get here. This case happens if we're unable to
	// execute the update.
	// WARNING: This path does not write out the json if requested because we
	// will not have parsed the flags when the exit handler runs
	handleResultAndExit(status.NewCmdResult(err))
}

// Execute adds all child commands to the root command and sets flags appropriately.
// This is called by main.main(). It only needs to happen once to the rootCmd.
func Execute() {
	handleResultAndExit(status.NewCmdResult(RootCmd.Execute()))
}

func newRootCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "chef-automate COMMAND",
		Short: "Chef Automate CLI",
		Long:  "A helpful utility to deploy and manage Chef Automate.",
		Annotations: map[string]string{
			NoCheckVersionAnnotation: NoCheckVersionAnnotation,
		},
		SilenceErrors: true,
		SilenceUsage:  true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			if globalOpts.debug {
				logrus.SetLevel(logrus.DebugLevel)
				logrus.Debugf("chef-automate %s (%s)", version.Version, version.GitSHA)
			} else {
				logrus.SetLevel(logrus.WarnLevel)
			}

			if !cmd.IsAvailableCommand() && !cmd.Hidden {
				// Allow for soft deprecation: we allow command to be run but warn that it will be deprecated
				// in the future.
				if cmd.Deprecated != "" {
					logrus.Debugf("Running deprecated command: %s", cmd.Name())
				} else {
					return status.Errorf(status.InvalidCommandArgsError, "%s is not a command", cmd.Name())
				}
			}

			if _, found := cmd.Annotations[NoRequireRootAnnotation]; found {
				logrus.Debug("Skipping root user check")
			} else {
				if os.Geteuid() != 0 {
					return status.Errorf(status.MustBeRootError, "%s must be run as the root user", cmd.Name())
				}
			}

			if _, found := cmd.Annotations[NoCheckVersionAnnotation]; found || globalOpts.noCheckVersion || disableAutoUpdate() {
				logrus.Debug("Not checking version")
			} else {
				err := runCLIFromDS()
				if err != nil {
					return err
				}
			}

			return nil
		},
	}

	cmd.PersistentFlags().BoolVarP(&globalOpts.debug, "debug", "d", false, "Enable debug output")
	cmd.PersistentFlags().BoolVar(&globalOpts.noCheckVersion, "no-check-version", false, "Disable version check")
	cmd.PersistentFlags().StringVar(&globalOpts.resultJSON, "result-json", "", "Write command result as JSON to PATH")

	return cmd
}

func handleResultAndExit(result *status.CmdResult) {
	if globalOpts.resultJSON != "" {
		logrus.Debugf("Writing command result to %s", globalOpts.resultJSON)
		result.WriteFile(globalOpts.resultJSON)
	}

	if result.Status == status.Failure {
		if result.ErrorStackTrace != "" {
			logrus.Debug(result.ErrorStackTrace)
		}

		msg := strings.Builder{}
		msg.WriteString(result.ErrorDescription)
		if result.ErrorRecovery != "" {
			fmt.Fprintf(&msg, "\n%s", result.ErrorRecovery)
		}
		writer.Failt(result.ErrorType, msg.String())
	}

	os.Exit(result.ErrorCode)
}

func runCLIFromDS() error {
	connection, err := client.Connection(500 * time.Millisecond)
	if err != nil {
		logrus.WithError(err).Debug("Unable to connect to server to get its version. Skipping version check.")
		return nil
	}

	updateSource := updatesource.DeploymentService(connection)
	return nextCLIOrElse(updateSource, false, func(err error) {
		if err != nil {
			client.WarnIfNotUpToDateAgainstServer(injectedVersionClient, writer, version.BuildTime)
		}
	})
}

// nextCLIOrElse updates the CLI and runs the updated version. If a updated cli is not
// available or we fail to get the updated cli, f is called with any error that occurred (if any)
// If an error occurs during reexecing, f is not called
func nextCLIOrElse(updateSource updatesource.UpdateSource, allowUpgradeAgain bool, f func(error)) error {
	updater := selfupdater.NewSelfUpdater(updateSource, executableCache)
	nextExe, err := updater.NextExecutable(context.Background())

	if err != nil {
		logrus.WithError(err).Debug("Couldn't update cli")
		f(err)
		return nil
	}

	if !nextExe.Available() || version.BuildTime > nextExe.Version {
		logrus.Debug("CLI update not available")
		f(nil)
		return nil
	}

	logrus.WithFields(logrus.Fields{
		"currentVersion": version.BuildTime,
		"nextVersion":    nextExe.Version,
	}).Debug("Reexecing")

	extraEnv := []string{}
	if !allowUpgradeAgain {
		extraEnv = append(extraEnv, fmt.Sprintf("%s=1", noUpdateCLIEnv))
	}

	if err := nextExe.ReExec(extraEnv); err != nil {
		writer.FailWrapCause(err, "Could not run the correct chef-automate executable")
		return err
	}

	// We should never get here
	return errors.New("Unreachable")
}

const noUpdateCLIEnv = "CHEF_AUTOMATE_NO_UPDATE_CLI"

func disableAutoUpdate() bool {
	// We need to use an environment variable here because we need to know this before
	// we've parsed any flags
	return os.Getenv(noUpdateCLIEnv) == "1"
}
