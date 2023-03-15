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

func TestRunCmdOnSingleAutomateNodeNCopyReport(t *testing.T) {
	type args struct {
		cmd      *cobra.Command
		args     []string
		fileName string
	}
	tests := []struct {
		name    string
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
		{
			name: "Test 1,  Want type error",
			args: args{
				cmd:  basicCmd(),
				args: []string{"one", "two"},
			},
			wantErr: true,
		},

		{
			name: "Test 2, Want type error",
			args: args{
				cmd:  basicCmd(),
				args: []string{"zyx", "pqr"},
			},
			wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if err := RunCmdOnSingleAutomateNodeNCopyReport(tt.args.cmd, tt.args.args, tt.args.fileName); (err != nil) != tt.wantErr {
				t.Errorf("RunCmdOnSingleAutomateNodeNCopyReport() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}
