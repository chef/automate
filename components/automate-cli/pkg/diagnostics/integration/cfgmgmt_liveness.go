package integration

import (
	"bytes"
	"fmt"
	"text/template"
	"time"

	uuid "github.com/gofrs/uuid"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.uber.org/multierr"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/clirequest"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const cfgmgmtLivenessTemplateStr = `
{
	"chef_server_fqdn": "chef-automate-diagnostics.chef.invalid",
	"source": "liveness_agent",
	"message_version": "0.0.1",
	"event_type": "node_ping",
	"organization_name": "happytrees",
	"node_name": "{{ .NodeName }}",
	"entity_uuid": "{{ .EntityUUID }}",
	"@timestamp": "{{ .Timestamp }}"
  }
`

const cfgmgmtLivenessDateTimeFormat = "2006-01-02T15:04:05Z"

type configMgmtLivenessEntity struct {
	Timestamp  string `json:"@timestamp"`
	EntityUUID string `json:"entity_uuid"`
	NodeName   string `json:"node_name"`
}

type configMgmtLivenessSave struct {
	CreatedEntities []configMgmtLivenessEntity `json:"created_entities"`
}

// CreateCfgmgmtLivenessDiagnostic creates a diagnostic that sends a liveness ping and
// verifies they show up in node inventory
func CreateCfgmgmtLivenessDiagnostic() diagnostics.Diagnostic {
	tmpl := template.Must(template.New("cfgmgmt-liveness-create").Parse(cfgmgmtLivenessTemplateStr))

	return diagnostics.Diagnostic{
		Name: "cfgmgmt-liveness",
		Tags: diagnostics.Tags{"cfgmgmt", "license-usage", "cli", "skip-for-deep-upgrade"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			now := time.Now().UTC()
			days := []time.Time{
				now,
				now.AddDate(0, 0, -1),
				now.AddDate(0, 0, -2),
			}
			save := configMgmtLivenessSave{
				CreatedEntities: []configMgmtLivenessEntity{},
			}

			for _, day := range days {
				entityUUID := uuid.Must(uuid.NewV4()).String()
				nodeName := "chef-automate-diagnostics-integration-" + entityUUID
				timestamp := day.Format(cfgmgmtLivenessDateTimeFormat)

				buf := bytes.NewBuffer([]byte{})
				livenessPingEntity := configMgmtLivenessEntity{
					EntityUUID: entityUUID,
					NodeName:   nodeName,
					Timestamp:  timestamp,
				}

				if err := tmpl.Execute(buf, livenessPingEntity); err != nil {
					return err
				}

				save.CreatedEntities = append(save.CreatedEntities, livenessPingEntity)
				tstCtx.SetValue("cfgmgmt-liveness", save)

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
					return errors.Errorf("Failed to POST /api/v0/events/data-collector: StatusCode:%d\nBody:\n%s", resp.StatusCode, buf.String())
				}
			}
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := configMgmtLivenessSave{}
			err := tstCtx.GetValue("cfgmgmt-liveness", &loaded)
			if err != nil {
				tstCtx.Errorf("No cfgmgmt-liveness in test context")
				tstCtx.FailNow()
			}

			err = Retry(2, 5*time.Second, func() error {
				resp, err := clirequest.LicenseUsage()
				require.NoError(tstCtx, err)
				assert.Equal(tstCtx, "/proc/self/exe license usage --result-json /tmp/license_usage.json", resp.Command)
				assert.Equal(tstCtx, "OK", resp.Status)
				assert.Equal(tstCtx, 0, resp.ErrorCode)

				nodes := make([]clirequest.LicenseUsageNode, 0)
				for _, entity := range loaded.CreatedEntities {
					containsEntity := false
					for _, node := range resp.Result.ManagedNodes {
						// Wtf? Why is name actually the uuid?
						if node.ID == entity.EntityUUID {
							containsEntity = true
							break
						}

						// All nodes should have these values set to something.
						assert.NotEqual(tstCtx, "", node.ID)
						assert.NotEqual(tstCtx, "", node.LastSeen)
						assert.NotEqual(tstCtx, "", node.CheckinType)

						// We can't assume the only nodes in the system are the ones
						// created by the test, so grab the nodes created by the test.
						if node.Metadata.Organization == "happytrees" && node.CheckinType == "liveness-agent" {
							nodes = append(nodes, node)
						}
					}

					if !containsEntity {
						return errors.Errorf("Could not find node %s in the usage list", entity.EntityUUID)
					}
				}

				// Verify the number of liveness nodes created.
				assert.Equal(tstCtx, 3, len(nodes))
				return nil
			})
			require.NoError(tstCtx, err)
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := configMgmtLivenessSave{}
			err := tstCtx.GetValue("cfgmgmt-liveness", &loaded)
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

func init() {
	diagnostics.RegisterDiagnostic(
		CreateCfgmgmtLivenessDiagnostic(),
	)
}
