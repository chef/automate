package v2

import (
	"encoding/json"
	"fmt"
	"os"
	"testing"

	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/stretchr/testify/require"
)

func TestRunningWorkflow01EstimatedTime(t *testing.T) {
	w := loadWorkflowFromDisk(t, "running_workflow_01")
	require.NotZero(t, w.EstimatedTimeComplete())
}

func loadWorkflowFromDisk(t *testing.T, name string) *workflowInstance {
	t.Helper()

	loaded := false
	w := &patterns.ChainWorkflowInstance{}

	paramsFname := fmt.Sprintf("testdata/%s/params.json", name)
	fParams, err := os.Open(paramsFname)
	if err != nil {
		if !os.IsNotExist(err) {
			t.Fatalf("could not open %s", paramsFname)
		}
	} else {
		defer fParams.Close()
		params := patterns.ChainWorkflowParams{}
		if err := json.NewDecoder(fParams).Decode(&params); err != nil {
			t.Fatalf("failed to decode %s", paramsFname)
		}
		w.WithParams(&params)
		loaded = true
	}

	payloadFname := fmt.Sprintf("testdata/%s/payload.json", name)
	fPayload, err := os.Open(payloadFname)
	if err != nil {
		if !os.IsNotExist(err) {
			t.Fatalf("could not open %s", payloadFname)
		}
	} else {
		defer fPayload.Close()
		payload := patterns.ChainWorkflowPayload{}
		if err := json.NewDecoder(fPayload).Decode(&payload); err != nil {
			t.Fatalf("failed to decode %s", payloadFname)
		}
		w.WithPayload(&payload)
		loaded = true
	}

	if !loaded {
		t.Fatalf("no workflow data found for %s", name)
	}

	return &workflowInstance{
		chain: w,
	}
}
