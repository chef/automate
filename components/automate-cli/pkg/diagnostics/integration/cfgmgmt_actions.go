package integration

import (
	"bytes"
	"encoding/json"
	"fmt"
	"text/template"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/gofrs/uuid"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const cfgmgmtActionsTemplateStr = `
{
	"message_type": "action",
	"message_version": "0.1.1",
	"organization_name": "chef",
	"service_hostname": "chef-automate-diagnostics-integration.chef.co",
	"recorded_at": "{{ .RecordedAt }}",
	"remote_hostname": "10.194.12.215",
	"request_id": "g3IAA2QAEGVyY2hlZkAxMjcuMC4wLjEBAAIksQbkAAIAAAAA",
	"requestor_name": "chef-automate-diagnostics-integration",
	"requestor_type": "client",
	"user_agent": "Chef Client/12.21.3 (ruby-2.3.1-p112; ohai-8.24.0; x86_64-linux; +https://chef.io)",
	"id": "action-id",
	"run_id": "run-id",
	"task": "update",
	"entity_type": "node",
	"entity_name": "{{ .EntityName }}",
	"remote_request_id": "8a64e85c-7ffb-4db3-a02e-1074d8aab25b",
	"data": {
	}
  }
  `

type cfgmgmtActionsEntity struct {
	EntityName       string `json:"entity_name"`
	RecordedAtMillis int64  `json:"recorded_at_millis"`
}

type cfgmgmtActionsSave struct {
	CreatedEntities []cfgmgmtActionsEntity `json:"created_entities"`
}

// CreateCfgmgmtActionsDiagnostic create the diagnostic struct for notification rules
func CreateCfgmgmtActionsDiagnostic() diagnostics.Diagnostic {
	tmpl := template.Must(template.New("cfgmgmt-actions-create").Parse(cfgmgmtActionsTemplateStr))

	return diagnostics.Diagnostic{
		Name: "cfgmgmt-actions",
		Tags: diagnostics.Tags{"cfgmgmt"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			// Make sure we cover multiple days
			now := time.Now().UTC()
			days := []time.Time{
				now,
				now.AddDate(0, 0, -1),
				now.AddDate(0, 0, -2),
			}
			save := cfgmgmtActionsSave{
				CreatedEntities: []cfgmgmtActionsEntity{},
			}

			for _, day := range days {

				id := uuid.Must(uuid.NewV4()).String()
				entityName := "chef-automate-diagnostics-integration-" + id
				buf := bytes.NewBuffer([]byte{})
				err := tmpl.Execute(buf, struct {
					RecordedAt string
					EntityName string
				}{
					RecordedAt: day.Format(time.RFC3339),
					EntityName: entityName,
				})

				if err != nil {
					return err
				}

				save.CreatedEntities = append(save.CreatedEntities, cfgmgmtActionsEntity{
					EntityName:       entityName,
					RecordedAtMillis: day.UnixNano() / int64(time.Millisecond),
				})

				tstCtx.SetValue("cfgmgmt-actions", save)

				resp, err := tstCtx.DoLBRequest(
					"/api/v0/events/data-collector",
					lbrequest.WithMethod("POST"),
					lbrequest.WithJSONBody(buf.String()),
				)

				if err != nil {
					return errors.Wrapf(err, "Failed to POST /api/v0/events/data-collector: Body\n%s", buf.String())
				}
				defer func() {
					_ = resp.Body.Close()
				}()
				if resp.StatusCode != 200 {
					return errors.Wrapf(err, "Failed to POST /api/v0/events/data-collector: Body:\n%s", buf.String())
				}
			}

			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := cfgmgmtActionsSave{}
			err := tstCtx.GetValue("cfgmgmt-actions", &loaded)
			if err != nil {
				tstCtx.Errorf("No cfgmgmt-actions in test context")
				tstCtx.FailNow()
			}

			// A maximum of 5 retries for the entire test to find the expected
			// entity_names
			tries := 0
			maxTries := 5

			for _, entity := range loaded.CreatedEntities {
				reqPath := fmt.Sprintf("/api/v0/eventfeed?collapse=true&page_size=100&start=%d&end=%d", entity.RecordedAtMillis-1, entity.RecordedAtMillis+1)
				found := false
			RETRY_LOOP:
				for {
					resp, err := tstCtx.DoLBRequest(reqPath)

					// We wont retry a flakey server
					require.NoError(tstCtx, err, "Failed to GET %s", reqPath)
					defer func() {
						_ = resp.Body.Close()
					}()
					// We wont retry a flakey backend. It should always return 200
					require.Equal(tstCtx, 200, resp.StatusCode, "Failed to GET %s", reqPath)

					type eventsFeedResp struct {
						Events []struct {
							EntityName string `json:"entity_name"`
						} `json:"events"`
					}

					respUnmarshalled := eventsFeedResp{}
					err = json.NewDecoder(resp.Body).Decode(&respUnmarshalled)
					// We should always get valid json
					require.NoError(tstCtx, err, "Failed to decode body of GET %s", reqPath)

					for _, respEntity := range respUnmarshalled.Events {
						if respEntity.EntityName == entity.EntityName {
							found = true
							break RETRY_LOOP
						}
					}
					tries++
					if tries >= maxTries {
						break RETRY_LOOP
					}
					time.Sleep(2 * time.Duration(tries) * time.Second)
				}
				assert.True(tstCtx, found, "Could not find entity %s in GET %s", entity.EntityName, reqPath)
			}

		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateCfgmgmtActionsDiagnostic(),
	)
}
