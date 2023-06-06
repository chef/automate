package main

import (
	"testing"

	"github.com/spf13/cobra"
)

var nodeDelCmd = &cobra.Command{
	// use getClient in the command implementation
}

// var argsAws = []string{"aws"}
// var argsExistingNodes = []string{"existing_infra"}
// var argsSomeThingElse = []string{"something_else"}
// var argsEmpty = []string{}

func Test_runpreInfrastructureCmd(t *testing.T) {
	tests := []struct {
		testName string
		cmd      *cobra.Command
		args     []string
		wantErr  bool
	}{
		{"Test node delete", nodeDelCmd, []string{"argsEmpty"}, true},
	}
	for _, tt := range tests {
		t.Run(tt.testName, func(t *testing.T) {
			if err := runDeleteNodeCmd(tt.cmd, tt.args); (err != nil) != tt.wantErr {
				t.Errorf("runDeleteNodeCmd() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}
