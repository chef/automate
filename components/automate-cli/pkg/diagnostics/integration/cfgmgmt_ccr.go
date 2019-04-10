package integration

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
	"text/template"
	"time"

	"go.uber.org/multierr"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/gofrs/uuid"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const cfgmgmtCCRTemplateStr = `
{
	"chef_server_fqdn": "chef-automate-diagnostics.chef.invalid",
	"entity_uuid": "{{ .EntityUUID }}",
	"expanded_run_list": {
	  "id": "_default",
	  "run_list": [
		{
		  "type": "recipe",
		  "name": "chef-automate-diagnostics::default",
		  "version": null,
		  "skipped": false
		}
	  ]
	},
	"id": "{{ .EntityUUID }}",
	"message_version": "1.0.0",
	"message_type": "run_converge",
	"node": {
	  "name": "{{ .NodeName }}",
	  "chef_environment": "chef-automate-diagnostics",
	  "run_list": [],
	  "normal": {
		"diagnostics": [
		  "present"
		]
	  }
	},
	"node_name": "{{ .NodeName }}.chef.invalid",
	"organization_name": "chef_delivery",
	"resources": [
	  {
			"type": "file",
			"name": "/tmp/test.txt",
			"id": "/tmp/test.txt",
			"after": {
				"owner": null,
				"group": null,
				"mode": null,
				"path": "/tmp/test.txt"
			},
			"before": {},
			"duration": "0",
			"delta": "",
			"result": "nothing",
			"ignore_failure": true,
			"status": "skipped",
			"cookbook_name": "chef-automate-diagnostics",
			"cookbook_version": "0.1.1",
			"conditional": "not_if { action == :nothing }"
		},
		{
			"type": "file",
			"name": "/tmp/test1.txt",
			"id": "/tmp/test1.txt",
			"after": {
				"owner": null,
				"group": null,
				"mode": null,
				"path": "/tmp/test1.txt"
			},
			"before": {},
			"duration": "0",
			"delta": "",
			"result": "nothing",
			"ignore_failure": false,
			"status": "skipped",
			"cookbook_name": "chef-automate-diagnostics",
			"cookbook_version": "0.1.1",
			"conditional": "not_if { action == :nothing }"
		}
	],
	"run_id": "{{ .RunID }}",
	"run_list": [
	  "recipe[chef-automate-diagnostics::default]"
	],
	"start_time": "{{ .StartTime }}",
	"end_time": "{{ .EndTime }}",
	"source": "chef-automate-diagnostics",
	"status": "success",
	"total_resource_count": 1,
	"updated_resource_count": 1
  }
`

const cfgmgmtCCRDateTimeFormat = "2006-01-02T15:04:05Z"

type cfgmgmtCCREntity struct {
	EntityUUID string `json:"entity_uuid"`
	RunID      string `json:"run_id"`
}

type cfgmgmtCCRSave struct {
	CreatedEntities []cfgmgmtCCREntity `json:"created_entities"`
}

type resource struct {
	IgnoreFailure *bool   `json:"ignore_failure"`
	Name          *string `json:"name"`
}

type runResponse struct {
	Resources []*resource `json:"resources"`
}

// CreateCfgmgmtCCRDiagnostic create the diagnostic struct for ingesting and verifying CCRs
func CreateCfgmgmtCCRDiagnostic() diagnostics.Diagnostic {
	tmpl := template.Must(template.New("cfgmgmt-ccr-create").Parse(cfgmgmtCCRTemplateStr))

	return diagnostics.Diagnostic{
		Name: "cfgmgmt-ccr",
		Tags: diagnostics.Tags{"cfgmgmt"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			// Make sure we cover multiple days
			now := time.Now().UTC()
			days := []time.Time{
				now,
				now.AddDate(0, 0, -1),
				now.AddDate(0, 0, -2),
			}
			save := cfgmgmtCCRSave{
				CreatedEntities: []cfgmgmtCCREntity{},
			}

			for _, day := range days {
				entityUUID := uuid.Must(uuid.NewV4()).String()
				runID := uuid.Must(uuid.NewV4()).String()
				nodeName := "chef-automate-diagnostics-integration-" + entityUUID
				startTime := day.Format(cfgmgmtCCRDateTimeFormat)
				endTime := day.Format(cfgmgmtCCRDateTimeFormat)

				buf := bytes.NewBuffer([]byte{})
				err := tmpl.Execute(buf, struct {
					EntityUUID string
					NodeName   string
					RunID      string
					StartTime  string
					EndTime    string
				}{
					EntityUUID: entityUUID,
					NodeName:   nodeName,
					RunID:      runID,
					StartTime:  startTime,
					EndTime:    endTime,
				})

				if err != nil {
					return err
				}

				save.CreatedEntities = append(save.CreatedEntities, cfgmgmtCCREntity{
					EntityUUID: entityUUID,
					RunID:      runID,
				})

				tstCtx.SetValue("cfgmgmt-ccr", save)

				resp, err := tstCtx.DoLBRequest(
					"/api/v0/ingest/events/chef/run",
					lbrequest.WithMethod("POST"),
					lbrequest.WithJSONBody(buf.String()),
				)

				if err != nil {
					return errors.Wrapf(err, "Failed to POST /api/v0/ingest/events/chef/run: Body\n%s", buf.String())
				}
				defer func() {
					_ = resp.Body.Close()
				}()
				if resp.StatusCode != 200 {
					return errors.Errorf("Failed to POST /api/v0/ingest/events/chef/run: StatusCode:%d\nBody:\n%s", resp.StatusCode, buf.String())
				}
			}

			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := cfgmgmtCCRSave{}
			err := tstCtx.GetValue("cfgmgmt-ccr", &loaded)
			if err != nil {
				tstCtx.Errorf("No cfgmgmt-ccr in test context")
				tstCtx.FailNow()
			}

			for _, entity := range loaded.CreatedEntities {
				testRuns(tstCtx, entity.EntityUUID, entity.RunID)

				testAttributes(tstCtx, entity.EntityUUID)
			}
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := cfgmgmtCCRSave{}
			err := tstCtx.GetValue("cfgmgmt-ccr", &loaded)
			if err != nil {
				return err
			}

			errs := []error{}
			for _, entity := range loaded.CreatedEntities {
				reqPath := "/api/v0/ingest/events/chef/nodedelete"
				resp, err := tstCtx.DoLBRequest(
					reqPath,
					lbrequest.WithMethod("POST"),
					lbrequest.WithJSONObjectBody(struct {
						NodeID string `json:"node_id"`
					}{
						NodeID: entity.EntityUUID,
					}),
				)

				if err != nil {
					errs = append(errs, errors.Wrapf(err, "Failed to delete %s", err))
					continue
				}

				if resp.StatusCode != 200 {
					errs = append(errs, errors.Errorf("Failed to delete %s. Got status code %d", reqPath, resp.StatusCode))
					fmt.Printf("%+v\n", errs)
				}
			}
			return multierr.Combine(errs...)
		},
	}
}

func testAttributes(tstCtx diagnostics.VerificationTestContext, entityUUID string) {
	respUnmarshalled := make(map[string]interface{})
	reqPath := fmt.Sprintf("/api/v0/cfgmgmt/nodes/%s/attribute", entityUUID)

	resp, err := getResponse(tstCtx, reqPath)
	require.NoErrorf(tstCtx, err, "Failed to GET %s", reqPath)
	defer func() {
		_ = resp.Body.Close()
	}()

	err = json.NewDecoder(resp.Body).Decode(&respUnmarshalled)
	require.NoErrorf(tstCtx, err, "Failed to decode response body for GET %s", reqPath)

	assert.Contains(tstCtx, respUnmarshalled["normal"], "diagnostics", "Expected GET %s to contain .normal.diagnostics", reqPath)
}

func testRuns(tstCtx diagnostics.VerificationTestContext, entityUUID string, runID string) {
	reqPath := fmt.Sprintf("/api/v0/cfgmgmt/nodes/%s/runs/%s", entityUUID, runID)

	runRespUnmarshalled, err := getRuns(tstCtx, reqPath)
	require.NoErrorf(tstCtx, err, "Could not find CCR using GET %s", reqPath)

	assert.Equal(tstCtx, 2, len(runRespUnmarshalled.Resources))

	assert.Equal(tstCtx, true, *runRespUnmarshalled.Resources[0].IgnoreFailure)
	assert.Equal(tstCtx, false, *runRespUnmarshalled.Resources[1].IgnoreFailure)
}

func getRuns(tstCtx diagnostics.VerificationTestContext, reqPath string) (runResponse, error) {
	runRespUnmarshalled := runResponse{}
	resp, err := getResponse(tstCtx, reqPath)
	if err != nil {
		return runRespUnmarshalled, err
	}
	defer func() {
		_ = resp.Body.Close()
	}()

	err = json.NewDecoder(resp.Body).Decode(&runRespUnmarshalled)
	if err != nil {
		return runRespUnmarshalled, err
	}

	return runRespUnmarshalled, nil
}

func getResponse(tstCtx diagnostics.VerificationTestContext, reqPath string) (*http.Response, error) {
	// A maximum of 5 retries for the entire test to find the expected
	// entity_names
	maxTries := 5
	for tries := 0; tries < maxTries; tries++ {
		resp, err := tstCtx.DoLBRequest(reqPath)
		if err != nil {
			return nil, err
		}

		// We wont retry a flakey server
		require.NoErrorf(tstCtx, err, "Failed to GET %s", reqPath)
		if resp.StatusCode == 200 {
			return resp, nil
		}

		time.Sleep(2 * time.Duration(tries) * time.Second)
	}

	return nil, nil
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateCfgmgmtCCRDiagnostic(),
	)
}
