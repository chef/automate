package main

import (
	"testing"

	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
)

func TestCommandBuilder(t *testing.T) {
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

	// Call the CommandBuilder function with the defined Cobra command and arguments
	fullCommand := CommandBuilder(cmd, []string{"arg1", "arg2"})
	expectedCommand := "sudo my-command --int-flag 42 --string-flag string-value arg1 arg2"
	assert.Equal(t, fullCommand, expectedCommand)

	// Set the bool flag to true and test the CommandBuilder function again
	cmd.Flag("bool-flag").Changed = true
	fullCommand = CommandBuilder(cmd, []string{"arg1", "arg2"})
	expectedCommand = "sudo my-command --bool-flag --int-flag 42 --string-flag string-value arg1 arg2"
	assert.Equal(t, fullCommand, expectedCommand)
}
