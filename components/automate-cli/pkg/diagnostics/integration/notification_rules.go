package integration

import (
	"bytes"
	"encoding/json"
	"strconv"
	"text/template"
	"time"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const notificationsCreateTemplateStr = `
{
	"rule": {
		"name": "{{.RuleName}}",
		"event": "CCRFailure",
		"SlackAlert": {
			"url": "http://localhost:55565"
		}
	}
}
`

type notificationsSave struct {
	ID string `json:"id"`
}

// CreateNotificationRulesDiagnostic create the diagnostic struct for notification rules
func CreateNotificationRulesDiagnostic() diagnostics.Diagnostic {
	tmpl := template.Must(template.New("notifications-create").Parse(notificationsCreateTemplateStr))

	return diagnostics.Diagnostic{
		Name: "notification-rules",
		Tags: diagnostics.Tags{"notifications"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			buf := bytes.NewBuffer([]byte{})
			ts := time.Now()
			ruleName := "integration-diagnostic-" + ts.Format("20060102150405")
			err := tmpl.Execute(buf, struct {
				RuleName string
			}{
				RuleName: ruleName,
			})

			if err != nil {
				return err
			}

			resp, err := tstCtx.DoLBRequest(
				"/api/v0/notifications/rules",
				lbrequest.WithMethod("POST"),
				lbrequest.WithJSONBody(buf.String()),
			)

			if resp != nil && resp.StatusCode != 200 {
				err = errors.New("Status code not 200")
			}
			if err != nil {
				return errors.Wrapf(err, "Request POST /api/v0/notifications/rules failed\nBody:\n%s", buf.String())
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

			tstCtx.SetValue("notification-rules", notificationsSave{ID: id})
			return err
		},

		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := notificationsSave{}

			err := tstCtx.GetValue("notification-rules", &loaded)
			require.NoError(tstCtx, err, "Generated context was not found")

			resp, err := tstCtx.DoLBRequest("/api/v0/notifications/rules/" + loaded.ID)
			require.NoError(tstCtx, err, "Failed to make notifications API call")
			defer func() {
				_ = resp.Body.Close()
			}()

			require.Equal(tstCtx, 200, resp.StatusCode, "Could not find rules with id %s", loaded.ID)
		},

		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := notificationsSave{}
			err := tstCtx.GetValue("notification-rules", &loaded)

			if err != nil {
				return errors.Wrap(err, "Generated context was not found")
			}

			resp, err := tstCtx.DoLBRequest(
				"/api/v0/notifications/rules/"+loaded.ID,
				lbrequest.WithMethod("DELETE"),
			)

			if err != nil {
				return errors.Wrapf(err, "Request DELETE /api/v0/notifications/rules/%s failed", loaded.ID)
			}

			defer func() {
				_ = resp.Body.Close()
			}()

			if resp.StatusCode != 200 {
				return errors.New("Unexpected status code " + strconv.Itoa(resp.StatusCode))
			}

			return nil
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateNotificationRulesDiagnostic(),
	)
}
