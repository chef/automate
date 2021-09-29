package main

import (
	"testing"

	"github.com/spf13/cobra"
)

var stubCmd = &cobra.Command{
	// use getClient in the command implementation
}
var argsAws = []string{"aws"}
var argsExistingNodes = []string{"existing_infra"}
var argsSomeThingElse = []string{"something_else"}
var argsEmpty = []string{}

func Test_runInitConfigHACmd(t *testing.T) {
	tests := []struct {
		testName string
		cmd      *cobra.Command
		args     []string
		wantErr  bool
	}{
		{"aws mode of deployment", stubCmd, argsAws, false},
		{"existing node mode of deployment", stubCmd, argsExistingNodes, false},
		{"Invalid mode of deployment", stubCmd, argsSomeThingElse, true},
		{"No args passed", stubCmd, argsEmpty, false},
	}
	for _, tt := range tests {
		t.Run(tt.testName, func(t *testing.T) {
			if err := runInitConfigHACmd(tt.cmd, tt.args); (err != nil) != tt.wantErr {
				t.Errorf("runInitConfigHACmd() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}
