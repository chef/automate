package server

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"testing"
	"text/template"
	"time"

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

	tmpl, err := template.ParseFiles(payloadFname)
	if err != nil {
		t.Fatalf("could not parse file: %s", err)
	}

	b := bytes.NewBuffer(nil)
	err = tmpl.Execute(b, struct {
		StartTime   string
		LastUpdated string
	}{
		StartTime:   time.Now().Format(time.RFC3339),
		LastUpdated: time.Now().Add(time.Minute).Format(time.RFC3339),
	})
	if err != nil {
		t.Fatalf("could not update time fields in template: %s", err)
	}

	payload := patterns.ChainWorkflowPayload{}
	if err := json.Unmarshal(b.Bytes(), &payload); err != nil {
		t.Fatalf("failed to decode %s", payloadFname)
	}
	w.WithPayload(&payload)
	loaded = true

	if !loaded {
		t.Fatalf("no workflow data found for %s", name)
	}

	return &workflowInstance{
		chain: w,
	}
}
