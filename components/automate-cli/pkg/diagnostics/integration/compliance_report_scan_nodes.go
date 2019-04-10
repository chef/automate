package integration

import (
	"bytes"
	"encoding/json"
	"fmt"
	"text/template"
	"time"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.uber.org/multierr"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/clirequest"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const nodeForScanningTemplateStr = `
{
    "name": "{{ .Name }}",
    "manager":"automate",
      "target_config": {
        "backend":"local",
        "host":"localhost",
        "port": 22
      },
      "tags": [
        { "key":"test-node", "value":"diagnostics" },
        { "key":"compliance-service", "value":"diagnostics" }
      ]
  }
`

// Deliberately using an empty profiles list for speed of testing.
const scanJobTemplateStr = `
{
	"name": "{{ .Name }}",
	"tags": [],
	"type": "exec",
	"nodes": ["{{ .NodeID }}"],
	"profiles": [""],
	"retries": 1,
	"node_selectors": []
}
`

type complianceScanReportSave struct {
	ID        string `json:"id"`
	Name      string `json:"name"`
	ScanJobID string `json:"scan_job_id"`
}

// CreateComplianceReportScanNodesDiagnostic create the diagnostic struct for compliance scan job
func CreateComplianceReportScanNodesDiagnostic() diagnostics.Diagnostic {
	tmpl := template.Must(template.New("compliance-report-scan-job").Parse(scanJobTemplateStr))

	return diagnostics.Diagnostic{
		Name: "compliance-scanning",
		Tags: diagnostics.Tags{"compliance", "license-usage", "cli", "skip-for-deep-upgrade"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			buf := bytes.NewBuffer([]byte{})
			ts := time.Now()
			name := "integration-diagnostic-" + ts.Format("20060102150405")

			// Create a node.
			t := template.Must(template.New("createNode").Parse(nodeForScanningTemplateStr))
			err := t.Execute(buf, struct {
				Name string
			}{
				Name: name,
			})

			if err != nil {
				return err
			}

			reqPath := "/api/v0/nodes"
			resp, err := tstCtx.DoLBRequest(
				reqPath,
				lbrequest.WithMethod("POST"),
				lbrequest.WithJSONBody(buf.String()),
			)

			if err != nil && resp.StatusCode != 200 {
				err = errors.New("Status code not 200")
			}
			if err != nil {
				return errors.Wrapf(err, "Request POST %s failed\nBody:\n%s\nResponse:\n%+v", reqPath, buf.String(), resp)
			}

			defer func() {
				_ = resp.Body.Close()
			}()

			respUnmarshalled := make(map[string]interface{})
			err = json.NewDecoder(resp.Body).Decode(&respUnmarshalled)

			if err != nil {
				return err
			}

			nodeID, ok := respUnmarshalled["id"].(string)

			if !ok {
				return errors.New("Could not find id in response")
			}
			buf.Reset()

			// Create a scan job
			err = tmpl.Execute(buf, struct {
				Name   string
				NodeID string
			}{
				Name:   name,
				NodeID: nodeID,
			})

			if err != nil {
				return err
			}
			reqPath = "/api/v0/compliance/scanner/jobs"
			resp, err = tstCtx.DoLBRequest(
				reqPath,
				lbrequest.WithMethod("POST"),
				lbrequest.WithJSONBody(buf.String()),
			)

			if resp != nil && resp.StatusCode != 200 {
				err = errors.New("Status code not 200")
			}
			if err != nil {
				return errors.Wrapf(err, "Request POST %s failed\nBody:\n%s\nResponse:\n%+v", reqPath, buf.String(), resp)
			}

			defer func() {
				_ = resp.Body.Close()
			}()

			respUnmarshalled = make(map[string]interface{})
			err = json.NewDecoder(resp.Body).Decode(&respUnmarshalled)

			if err != nil {
				return err
			}

			jobid, ok := respUnmarshalled["id"].(string)

			if !ok {
				return errors.New("Could not find id in response")
			}

			save := complianceScanReportSave{
				ID:        nodeID,
				Name:      "local node",
				ScanJobID: jobid,
			}

			tstCtx.SetValue("compliance-report-scan-nodes", save)

			return nil
		},

		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := complianceScanReportSave{}
			err := tstCtx.GetValue("compliance-report-scan-nodes", &loaded)
			if err != nil {
				tstCtx.Errorf("No compliance-report-scan-nodes in test context")
				tstCtx.FailNow()
			}

			err = Retry(3, 5*time.Second, func() error {
				resp, err := clirequest.LicenseUsage()
				require.NoError(tstCtx, err)
				assert.Equal(tstCtx, "/proc/self/exe license usage --result-json /tmp/license_usage.json", resp.Command)
				assert.Equal(tstCtx, "OK", resp.Status)
				assert.Equal(tstCtx, 0, resp.ErrorCode)

				containsEntity := false
				for _, node := range resp.Result.ScannedNodes {
					if node.ID == loaded.ID {
						containsEntity = true
						break
					}

					// All nodes should have these values set to something.
					assert.NotEqual(tstCtx, "", node.ID)
					assert.NotEqual(tstCtx, "", node.Name)
					assert.NotEqual(tstCtx, "", node.ScanJobID)

				}

				if !containsEntity {
					return errors.Errorf("Could not find node %s in usage's list of scanned nodes", loaded.ID)
				}

				return nil
			})
			require.NoError(tstCtx, err)
		},

		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := complianceScanReportSave{}
			err := tstCtx.GetValue("compliance-report-scan-nodes", &loaded)

			if err != nil {
				return errors.Wrap(err, "Generated context was not found")
			}

			errs := []error{}

			// Delete scan job
			reqPath := fmt.Sprintf("/api/v0/compliance/scanner/jobs/id/%s", loaded.ScanJobID)
			resp, err := tstCtx.DoLBRequest(
				reqPath,
				lbrequest.WithMethod("DELETE"),
			)

			if err != nil {
				return errors.Wrapf(err, "Failed to DELETE %s", reqPath)
			}

			defer func() {
				_ = resp.Body.Close()
			}()

			if resp.StatusCode != 200 {
				return errors.New("Unexpected status code")
			}

			// Delete scanned nodes
			reqPath = "/api/v0/ingest/events/chef/nodedelete"
			resp, err = tstCtx.DoLBRequest(
				reqPath,
				lbrequest.WithMethod("POST"),
				lbrequest.WithJSONObjectBody(struct {
					NodeID string `json:"node_id"`
				}{
					NodeID: loaded.ID,
				}),
			)

			// not sure this still does what I want. before, when this was a loop, there was a continue.
			if err != nil {
				errs = append(errs, errors.Wrapf(err, "Failed to delete %s", err))
			}

			if resp.StatusCode != 200 {
				errs = append(errs, errors.Errorf("Failed to delete %s. Got status code %d", reqPath, resp.StatusCode))
				fmt.Printf("%+v\n", errs)
			}

			return multierr.Combine(errs...)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateComplianceReportScanNodesDiagnostic(),
	)
}
