package commands

import (
	"fmt"
	"os"

	"github.com/chef/automate/components/applications-load-gen/pkg/generator"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

type LoadGenCommonOpts struct {
	profileFile           string
	useDefaultProfile     bool
	useBuiltinFullProfile bool
	svcCount              int32
}

func (l LoadGenCommonOpts) LoadProfileCfg() (*generator.LoadProfileCfg, error) {
	if rootFlags.useDefaultProfile {
		return generator.BuiltinConfig()
	}
	if rootFlags.useBuiltinFullProfile {
		return generator.BuiltinConfigFull()
	}
	return generator.ProfileFromFile(rootFlags.profileFile)
}

func (l LoadGenCommonOpts) SelectedProfile() string {
	if l.profileFile != "" {
		return fmt.Sprintf("file %s", l.profileFile)
	}
	if l.useDefaultProfile {
		return "builtin simple profile"
	}
	if l.useBuiltinFullProfile {
		return "builtin full profile"
	}

	return "none"
}

func (l LoadGenCommonOpts) ValidateProfileOpts() error {
	if !rootFlags.ProfileSpecified() {
		return errors.New("no profile filename or builtin profile specified")
	}
	if rootFlags.TooManyProfilesSpecified() {
		return errors.New("more than one profile specified. specify one profile filename or builtin profile")
	}
	return nil
}

func (l LoadGenCommonOpts) ProfileSpecified() bool {
	return l.countProfilesSpecified() > 0
}

func (l LoadGenCommonOpts) TooManyProfilesSpecified() bool {
	return l.countProfilesSpecified() > 1
}

func (l LoadGenCommonOpts) countProfilesSpecified() int {
	total := 0
	if l.profileFile != "" {
		total++
	}
	if l.useDefaultProfile {
		total++
	}
	if l.useBuiltinFullProfile {
		total++
	}
	return total
}

var rootFlags LoadGenCommonOpts

// RootCmd is the command runner.
var RootCmd = &cobra.Command{
	Use:          "applications-load-gen",
	Short:        "Load Generator for Chef Automate Applications Service",
	SilenceUsage: true,
}

// Execute adds all child commands to the root command sets flags appropriately.
// This is called by main.main(). It only needs to happen once to the rootCmd.
func Execute() {
	if err := RootCmd.Execute(); err != nil {
		os.Exit(-1)
	}
}

func init() {
	// global config
	RootCmd.PersistentFlags().StringVarP(
		&rootFlags.profileFile,
		"profile",
		"p",
		"",
		"file that configures the shape of the load to be generated",
	)
	RootCmd.PersistentFlags().BoolVar(
		&rootFlags.useDefaultProfile,
		"use-default-profile",
		false,
		"use simple builtin load profile",
	)

	RootCmd.PersistentFlags().BoolVar(
		&rootFlags.useBuiltinFullProfile,
		"use-builtin-full-profile",
		false,
		"use more complex builtin load profile",
	)

	RootCmd.PersistentFlags().Int32VarP(
		&rootFlags.svcCount,
		"svc-count",
		"c",
		0,
		"scale the traffic profile to (approximately) the given number of services",
	)

	RootCmd.AddCommand(newDescribeCmd())
	RootCmd.AddCommand(newRunCmd())
}
