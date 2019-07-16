package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest/parser"
)

var airgapCmdFlags = airgapFlags{}

type airgapFlags struct {
	manifestPath   string
	workspacePath  string
	channel        string
	overrideOrigin string
	hartifactsPath string
	verbose        bool
	hartsOnly      bool
	retries        int
	retryDelay     int
}

func (f airgapFlags) validateArgs() error {
	if f.manifestPath != "" && f.channel != "" {
		return status.New(status.AirgapCreateInstallBundleError, "You cannot provide both a manifest.json and a release channel")
	}

	if f.hartifactsPath != "" && f.overrideOrigin == "" {
		return status.New(status.InvalidCommandArgsError, "You must provide --override-origin with --hartifacts")
	}

	if f.hartifactsPath == "" && f.overrideOrigin != "" {
		return status.New(status.InvalidCommandArgsError, "Must must provide --hartifacts with --override-origin")
	}

	return nil
}

const successString = `Your Automate Install Bundle has been written to %s. To install the
bundle on an Internet-disconnected server, copy the bundle and the chef-automate tool
to the server and run

chef-automate deploy --airgap-bundle </path/to/bundle>
`

func newAirgapCmd() *cobra.Command {
	var airgapCmd = &cobra.Command{
		Use: "airgap COMMAND",
	}

	var bundleCmd = &cobra.Command{
		Use: "bundle COMMAND",
	}

	var bundleCreateCmd = &cobra.Command{
		Use: "create [/path/to/bundle.aib]",
		Annotations: map[string]string{
			NoCheckVersionAnnotation: NoCheckVersionAnnotation,
			NoRequireRootAnnotation:  NoRequireRootAnnotation,
		},
		RunE: runAirgapCreateInstallBundle,
		Args: cobra.RangeArgs(0, 1),
	}

	bundleCreateCmd.PersistentFlags().StringVarP(
		&airgapCmdFlags.channel, "channel", "c", "",
		"Release channel to pull packages from",
	)

	bundleCreateCmd.PersistentFlags().StringVarP(
		&airgapCmdFlags.manifestPath, "manifest", "m", "",
		"Path to a release manifest.json",
	)

	bundleCreateCmd.PersistentFlags().StringVarP(
		&airgapCmdFlags.workspacePath, "workspace", "w", "",
		"Path to workspace storage location where temporary data will be stored",
	)

	bundleCreateCmd.PersistentFlags().StringVar(
		&airgapCmdFlags.hartifactsPath, "hartifacts", "",
		"Path to habitat package artifacts",
	)

	bundleCreateCmd.PersistentFlags().StringVar(
		&airgapCmdFlags.overrideOrigin, "override-origin", "",
		"Origin of habitat package creator",
	)

	bundleCreateCmd.PersistentFlags().IntVarP(
		&airgapCmdFlags.retries, "retries", "r", 2,
		"Number of times to retry failed hab package downloads",
	)

	bundleCreateCmd.PersistentFlags().IntVar(
		&airgapCmdFlags.retryDelay, "retry-delay", -1,
		"Number of seconds to wait between retries (exponential backoff is used if not provided)",
	)

	if !isDevMode() {
		for _, flagName := range []string{
			"override-origin",
			"hartifacts",
		} {
			err := bundleCreateCmd.PersistentFlags().MarkHidden(flagName)
			if err != nil {
				fmt.Printf("failed configuring cobra: %s\n", err.Error())
				panic(":(")
			}
		}
	}

	var bundleUnpackCmd = &cobra.Command{
		Use: "unpack /path/to/bundle.aib",
		Annotations: map[string]string{
			NoCheckVersionAnnotation: NoCheckVersionAnnotation,
			NoRequireRootAnnotation:  NoRequireRootAnnotation,
		},
		RunE:   runAirgapUnpackInstallBundle,
		Args:   cobra.ExactArgs(1),
		Hidden: true,
	}

	bundleUnpackCmd.PersistentFlags().BoolVar(
		&airgapCmdFlags.hartsOnly, "harts-only", false, "Only unpack hartifacts and their keys",
	)

	var bundleInfoCmd = &cobra.Command{
		Use: "info /path/to/bundle.aib",
		Annotations: map[string]string{
			NoCheckVersionAnnotation: NoCheckVersionAnnotation,
			NoRequireRootAnnotation:  NoRequireRootAnnotation,
		},
		RunE: runAirgapInfoInstallBundle,
		Args: cobra.ExactArgs(1),
	}

	bundleInfoCmd.PersistentFlags().BoolVar(
		&airgapCmdFlags.verbose, "verbose", false, "Output full AIB metadata",
	)

	if isDevMode() {
		bundleUnpackCmd.Hidden = false
	}

	airgapCmd.AddCommand(bundleCmd)
	bundleCmd.AddCommand(bundleCreateCmd)
	bundleCmd.AddCommand(bundleInfoCmd)
	bundleCmd.AddCommand(bundleUnpackCmd)

	return airgapCmd
}

type installBundlePath struct {
	InstallBundlePath string `json:"install_bundle_path"`
}

type installBundleCreateProgress struct{}

func (installBundleCreateProgress) Downloading(name string, try int) {
	if try > 1 {
		writer.Bodyf("Downloading %s (try %d)", name, try)
	} else {
		writer.Bodyf("Downloading %s", name)
	}
	writer.StartSpinner()
}

func (installBundleCreateProgress) DownloadComplete(name string, wasCached bool) {
	writer.StopSpinner()
	if wasCached {
		writer.Bodyf("%s found in cache", name)
	} else {
		writer.Bodyf("Downloaded %s", name)
	}
}

func (installBundleCreateProgress) RetriableDownloadError(name string, info string, delay time.Duration) {
	writer.StopSpinner()
	writer.Bodyf("Downloading %s failed (will retry in %s): %s", name, delay, info)
}

func runAirgapCreateInstallBundle(cmd *cobra.Command, args []string) error {
	if err := airgapCmdFlags.validateArgs(); err != nil {
		return err
	}

	opts := []airgap.InstallBundleCreatorOpt{}

	if airgapCmdFlags.channel != "" {
		opts = append(
			opts,
			airgap.WithInstallBundleChannel(airgapCmdFlags.channel),
		)
	}

	if airgapCmdFlags.manifestPath != "" {
		opts = append(
			opts,
			airgap.WithInstallBundleManifestFile(airgapCmdFlags.manifestPath),
		)
	}

	if airgapCmdFlags.hartifactsPath != "" && airgapCmdFlags.overrideOrigin != "" {
		opts = append(
			opts,
			airgap.WithInstallBundleHartifactsPath(
				airgapCmdFlags.hartifactsPath,
				airgapCmdFlags.overrideOrigin,
			),
		)
	}

	if len(args) > 0 && args[0] != "" {
		opts = append(opts, airgap.WithInstallBundleOutputPath(args[0]))
	}

	if airgapCmdFlags.workspacePath == "" {
		var err error
		airgapCmdFlags.workspacePath, err = ioutil.TempDir("", "aib-workspace")
		if err != nil {
			return status.Wrap(
				err,
				status.FileAccessError,
				"Creating temporary workspace directory failed",
			)
		}
		logrus.WithField(
			"dir", airgapCmdFlags.workspacePath,
		).Debug("Using temporary workspace")

		defer func() {
			_ = os.RemoveAll(airgapCmdFlags.workspacePath)
		}()
	}

	opts = append(
		opts,
		airgap.WithInstallBundleWorkspacePath(airgapCmdFlags.workspacePath),
		airgap.WithInstallBundleRetries(airgapCmdFlags.retries),
		airgap.WithInstallBundleRetryDelay(airgapCmdFlags.retryDelay),
	)

	writer.Title("Creating Airgap Installation Bundle...")
	creator := airgap.NewInstallBundleCreator(opts...)
	outputFile, err := creator.Create(installBundleCreateProgress{})
	if err != nil {
		writer.StopSpinner()
		return status.Annotate(err, status.AirgapCreateInstallBundleError)
	}

	writer.Successf(successString, outputFile)
	status.GlobalResult = installBundlePath{
		InstallBundlePath: outputFile,
	}
	return nil
}

func runAirgapUnpackInstallBundle(cmd *cobra.Command, args []string) error {
	_, err := airgap.Unpack(args[0], airgap.WithUnpackHartsOnly(airgapCmdFlags.hartsOnly))
	if err != nil {
		return status.Annotate(err, status.AirgapUnpackInstallBundleError)
	}
	return nil
}

func runAirgapInfoInstallBundle(cmd *cobra.Command, args []string) error {
	metadata, manifestBytes, err := airgap.GetMetadata(args[0])
	if err != nil {
		return status.Annotate(err, status.AirgapUnpackInstallBundleError)
	}

	manifest, err := parser.ManifestFromBytes(manifestBytes)
	if err != nil {
		return status.Annotate(err, status.AirgapUnpackInstallBundleError)
	}

	writer.Printf("  Version: %s\n", manifest.Build)
	writer.Printf("Build SHA: %s\n", manifest.BuildSHA)
	writer.Printf(" Packages:\n")
	for _, pkg := range manifest.Packages {
		writer.Printf("           %s\n", habpkg.Ident(&pkg))
	}

	if airgapCmdFlags.verbose {
		writer.Printf("\nHab binary path: %s\n", metadata.HabBinPath)
		writer.Printf("  Manifest path: %s\n", metadata.HabBinPath)
		writer.Printf("     Hartifacts:\n")
		for _, p := range metadata.HartifactPaths {
			writer.Printf("                 %s\n", p)
		}

		writer.Printf("    Origin Keys:\n")
		for _, p := range metadata.OriginKeyPaths {
			writer.Printf("                 %s\n", p)
		}

	}

	return nil
}

func init() {
	RootCmd.AddCommand(newAirgapCmd())
}
