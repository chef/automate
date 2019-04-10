package integration

import (
	"bytes"
	"encoding/json"
	"fmt"
	"text/template"
	"time"

	uuid "github.com/gofrs/uuid"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const complianceSecretCreateTemplateStr = `
{
	"name": "{{ .Name }}",
	"type": "sudo",
	"data": [
		{
			"key": "password",
			"value": "{{ .Password }}"
		}
	]
}
`

type complianceSecretSave struct {
	ID string `json:"id"`
}

// CreateComplianceSecretDiagnostic create the diagnostic struct for compliance secrets
func CreateComplianceSecretDiagnostic() diagnostics.Diagnostic {
	tmpl := template.Must(template.New("compliance-secret").Parse(complianceSecretCreateTemplateStr))

	return diagnostics.Diagnostic{
		Name: "compliance-secret",
		Tags: diagnostics.Tags{"compliance"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			buf := bytes.NewBuffer([]byte{})
			ts := time.Now()
			name := "integration-diagnostic-" + ts.Format("20060102150405")
			password := uuid.Must(uuid.NewV4()).String()
			err := tmpl.Execute(buf, struct {
				Name     string
				Password string
			}{
				Name:     name,
				Password: password,
			})

			if err != nil {
				return err
			}

			reqPath := "/api/v0/secrets"
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

			tstCtx.SetValue("compliance-secret", complianceSecretSave{ID: id})
			return err
		},

		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := complianceSecretSave{}

			err := tstCtx.GetValue("compliance-secret", &loaded)
			require.NoError(tstCtx, err, "Generated context was not found")

			reqPath := fmt.Sprintf("/api/v0/secrets/id/%s", loaded.ID)
			resp, err := tstCtx.DoLBRequest(reqPath)
			require.NoError(tstCtx, err)
			defer func() {
				_ = resp.Body.Close()
			}()

			require.Equal(tstCtx, 200, resp.StatusCode, "Failed to GET %s", reqPath)
		},

		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := complianceSecretSave{}
			err := tstCtx.GetValue("compliance-secret", &loaded)

			if err != nil {
				return errors.Wrap(err, "Generated context was not found")
			}

			reqPath := fmt.Sprintf("/api/v0/secrets/id/%s", loaded.ID)
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
		CreateComplianceSecretDiagnostic(),
	)
}
