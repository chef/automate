package integration

import (
	"bytes"
	"encoding/json"
	"fmt"
	"text/template"
	"time"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const complianceNodeCreateTemplateStr = `
{
	"name":"{{ .Name }}",
	"target_config": {
		"backend":"ssh",
		"secrets":[],
		"port":22,
		"sudo":false,
		"host":"198.51.100.222"
	},
	"tags":[]
}
`

type complianceNodeSave struct {
	ID string `json:"id"`
}

// CreateComplianceNodeDiagnostic create the diagnostic struct for compliance node
func CreateComplianceNodeDiagnostic() diagnostics.Diagnostic {
	tmpl := template.Must(template.New("compliance-node").Parse(complianceNodeCreateTemplateStr))

	return diagnostics.Diagnostic{
		Name: "compliance-node",
		Tags: diagnostics.Tags{"compliance"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			buf := bytes.NewBuffer([]byte{})
			ts := time.Now()
			name := "integration-diagnostic-" + ts.Format("20060102150405")
			err := tmpl.Execute(buf, struct {
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

			if resp != nil && resp.StatusCode != 200 {
				err = errors.New("Status code not 200")
			}
			if err != nil {
				return errors.Wrapf(err, "Request POST %s failed\nBody:\n%s", reqPath, buf.String())
			}

			defer func() {
				_ = resp.Body.Close()
			}()

			respUnmarshalled := make(map[string]interface{})
			err = json.NewDecoder(resp.Body).Decode(&respUnmarshalled)

			if err != nil {
				return err
			}

			id, ok := respUnmarshalled["id"].(string)

			if !ok {
				return errors.New("Could not find id in response")
			}

			tstCtx.SetValue("compliance-node", complianceNodeSave{ID: id})
			return err
		},

		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := complianceNodeSave{}

			err := tstCtx.GetValue("compliance-node", &loaded)
			require.NoError(tstCtx, err, "Generated context was not found")

			reqPath := fmt.Sprintf("/api/v0/nodes/id/%s", loaded.ID)
			resp, err := tstCtx.DoLBRequest(reqPath)
			require.NoError(tstCtx, err, "Failed to make compliance API call")
			defer func() {
				_ = resp.Body.Close()
			}()

			require.Equal(tstCtx, 200, resp.StatusCode, "Failed to GET %s", reqPath)
		},

		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := complianceNodeSave{}
			err := tstCtx.GetValue("compliance-node", &loaded)

			if err != nil {
				return errors.Wrap(err, "Generated context was not found")
			}

			reqPath := fmt.Sprintf("/api/v0/nodes/id/%s", loaded.ID)
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

			return nil
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateComplianceNodeDiagnostic(),
	)
}
