package main

import (
	"testing"
	"time"

	"github.com/chef/automate/lib/io/fileutils"
)

func TestIPValidation(t *testing.T) {
	// &MockSSHUtilsImpl{}
	// w := majorupgrade_utils.NewCustomWriterWithInputs("x")

	NewStatusSummary(&AutomteHAInfraDetails{
		
	}, &fileutils.MockFileSystemUtils{}, FeStatus{}, BeStatus{}, 10, time.Second, &StatusSummaryCmdFlags{})
}
