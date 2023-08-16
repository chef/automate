package main

import (
	"testing"

	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
)

func TestGenerateOriginalAutomateCLICommand(t *testing.T) {
	// Define a Cobra command for testing purposes
	cmd := &cobra.Command{
		Use:   "my-command",
		Short: "A description of my command",
		Run: func(cmd *cobra.Command, args []string) {
			// Not used in this test
		},
	}

	// Define some flags for the Cobra command
	cmd.Flags().String("string-flag", "", "A description of the string flag")
	cmd.Flags().Int("int-flag", 0, "A description of the int flag")
	cmd.Flags().Bool("bool-flag", false, "A description of the bool flag")

	// Set the values of the string and int flags
	cmd.Flags().Set("string-flag", "string-value")
	cmd.Flags().Set("int-flag", "42")

	// Call the GenerateOriginalAutomateCLICommand function with the defined Cobra command and arguments
	fullCommand := GenerateOriginalAutomateCLICommand(cmd, []string{"arg1", "arg2"})
	expectedCommand := "sudo my-command --int-flag 42 --string-flag string-value arg1 arg2"
	assert.Equal(t, fullCommand, expectedCommand)

	// Set the bool flag to true and test the GenerateOriginalAutomateCLICommand function again
	cmd.Flag("bool-flag").Changed = true
	fullCommand = GenerateOriginalAutomateCLICommand(cmd, []string{"arg1", "arg2"})
	expectedCommand = "sudo my-command --bool-flag --int-flag 42 --string-flag string-value arg1 arg2"
	assert.Equal(t, fullCommand, expectedCommand)
}

func basicCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "example-for-test",
		RunE:  runExampleForTestCmd,
		Short: "just_for_testing_nothing_happens",
		Long:  "just_for_testing_nothing_happens",
	}
}

func runExampleForTestCmd(cmd *cobra.Command, _ []string) error {
	return nil
}

func TestGetEnabledFlags(t *testing.T) {
	// Define a Cobra command for testing purposes
	cmd := &cobra.Command{
		Use:   "my-command",
		Short: "A description of my command",
		Run: func(cmd *cobra.Command, args []string) {
			// Not used in this test
		},
	}

	// Define some flags for the Cobra command
	cmd.Flags().String("string-flag", "", "A description of the string flag")
	cmd.Flags().Int("int-flag", 0, "A description of the int flag")
	cmd.Flags().Bool("bool-flag", false, "A description of the bool flag")

	// Set the values of the string and int flags
	cmd.Flags().Set("string-flag", "string-value")
	cmd.Flags().Set("int-flag", "42")

	tests := []struct {
		TestName       string
		flagsToIgnore  map[string]int
		EnableBoolFlag bool
		ExpectedOutput string
	}{
		{"Not Ignoring any flag", map[string]int{}, false, " --int-flag 42 --string-flag string-value"},
		{"Ignoring some flag", map[string]int{"string-flag": 1}, false, " --int-flag 42"},
		{"Bool flag enabled", map[string]int{}, true, " --bool-flag --int-flag 42 --string-flag string-value"},
	}

	for _, e := range tests {
		t.Run(e.TestName, func(t *testing.T) {
			cmd.Flag("bool-flag").Changed = false
			if e.EnableBoolFlag {
				cmd.Flag("bool-flag").Changed = true
			}
			flags := GetEnabledFlags(cmd, e.flagsToIgnore)
			assert.Equal(t, e.ExpectedOutput, flags)
		})
	}
}
