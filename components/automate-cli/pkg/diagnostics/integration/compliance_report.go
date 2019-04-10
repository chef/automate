package integration

import (
	"bytes"
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

const complianceReportTemplateStr = `
{
	"type": "inspec_report",
	"node_uuid": "{{ .NodeUUID }}",
	"report_uuid": "{{ .ReportUUID }}",
	"job_uuid": "{{ .JobUUID }}",
	"node_name": "{{ .NodeName }}",
	"environment": "chef-automate-diagnostics",
	"roles": ["chef-automate-diagnostics"],
	"recipes": ["chef-automate-diagnostics"],
	"status": "passed",
	"end_time": "{{ .EndTime }}",
	"version": "2.1.10",
	"platform": {
	  "name": "centos",
	  "release": "5.11"
	},
	"statistics": {
	  "duration": 3.309065
	},
	"other_checks": [],
	"profiles": [
	  {
		"name": "chef-automate-diagnostics-test",
		"title": "chef-automate-diagnostics-test",
		"version": "2.1.0",
		"sha256": "11111111111111111111111111111111111111111111111111111fb1ff143988",
		"maintainer": "Chef Software",
		"summary": "Fake test data",
		"license": "Apache-2.0",
		"copyright": "Chef Software",
		"supports": [
		  {
			"os-family": "unix"
		  }
		],
		"attributes": [],
		"groups": [
		  {
			"id": "controls/a2_test_spec.rb",
			"controls": [
			  "a2-diag-01"
			],
			"title": "A2 Diagnostics Test"
		  }
		],
		"controls": [
		  {
			"id": "a2-diag-01",
			"title": "The is a title",
			"desc": "This is a description.",
			"impact": 1,
			"refs": [],
			"tags": {},
			"code": "control 'a2-diag-01' do\n  impact 1.0\n  title 'This is a title'\n  desc 'This is a description.'\n  describe some(thing) do\n    it { should exist }\n  end\n",
			"source_location": {
			  "line": 99,
			  "ref": "a2-diagnostics-test/controls/a2_test_spec.rb"
			},
			"results": [
			  {
				"status": "passed",
				"code_desc": "Things happened",
				"run_time": 0.000134
			  }
			]
		  }
		],
		"supports": null,
		"attributes": null,
		"groups": null
	  }
	]
  }
`

const complianceReportDateTimeFormat = "2006-01-02T15:04:05Z"

type complianceReportEntity struct {
	NodeUUID   string `json:"node_uuid"`
	ReportUUID string `json:"report_uuid"`
}

type complianceReportSave struct {
	CreatedEntities []complianceReportEntity `json:"created_entities"`
}

// CreateComplianceReportDiagnostic create the diagnostic struct for ingesting and verifying compliance reports
func CreateComplianceReportDiagnostic() diagnostics.Diagnostic {
	tmpl := template.Must(template.New("compliance-report-create").Parse(complianceReportTemplateStr))

	return diagnostics.Diagnostic{
		Name: "compliance-report",
		Tags: diagnostics.Tags{"compliance"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			// Make sure we cover multiple days
			now := time.Now().UTC()
			days := []time.Time{
				now,
				now.AddDate(0, 0, -1),
				now.AddDate(0, 0, -2),
			}
			save := complianceReportSave{
				CreatedEntities: []complianceReportEntity{},
			}

			for _, day := range days {
				nodeUUID := uuid.Must(uuid.NewV4()).String()
				reportUUID := uuid.Must(uuid.NewV4()).String()
				jobUUID := uuid.Must(uuid.NewV4()).String()

				nodeName := "chef-automate-diagnostics-integration-" + nodeUUID
				endTime := day.Format(complianceReportDateTimeFormat)

				buf := bytes.NewBuffer([]byte{})
				err := tmpl.Execute(buf, struct {
					NodeUUID   string
					ReportUUID string
					JobUUID    string
					NodeName   string
					EndTime    string
				}{
					NodeUUID:   nodeUUID,
					ReportUUID: reportUUID,
					JobUUID:    jobUUID,
					NodeName:   nodeName,
					EndTime:    endTime,
				})

				if err != nil {
					return err
				}

				save.CreatedEntities = append(save.CreatedEntities, complianceReportEntity{
					NodeUUID:   nodeUUID,
					ReportUUID: reportUUID,
				})

				tstCtx.SetValue("compliance-report", save)

				reqPath := "/api/v0/events/data-collector"
				resp, err := tstCtx.DoLBRequest(
					reqPath,
					lbrequest.WithMethod("POST"),
					lbrequest.WithJSONBody(buf.String()),
				)

				if err != nil {
					return errors.Wrapf(err, "Failed to POST %s: Body\n%s", reqPath, buf.String())
				}
				defer func() {
					_ = resp.Body.Close()
				}()
				if resp.StatusCode != 200 {
					return errors.Errorf("Failed to POST %s: StatusCode:%d\nBody:\n%s", reqPath, resp.StatusCode, buf.String())
				}
			}

			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := complianceReportSave{}
			err := tstCtx.GetValue("compliance-report", &loaded)
			if err != nil {
				tstCtx.Errorf("No compliance-report in test context")
				tstCtx.FailNow()
			}

			// A maximum of 5 retries for the entire test to find the expected
			// entity_names
			tries := 0
			maxTries := 5

			for _, entity := range loaded.CreatedEntities {
				nodeReqPath := fmt.Sprintf("/api/v0/compliance/reporting/nodes/id/%s", entity.NodeUUID)
				found := false
				for {
					resp, err := tstCtx.DoLBRequest(nodeReqPath)

					// We wont retry a flakey server
					require.NoError(tstCtx, err, "Failed to GET %s", nodeReqPath)
					defer func() {
						_ = resp.Body.Close()
					}()
					if resp.StatusCode == 200 {
						found = true
						break
					}

					tries++
					if tries >= maxTries {
						break
					}
					time.Sleep(2 * time.Duration(tries) * time.Second)
				}
				// Make sure the node, run id pair was found
				assert.True(tstCtx, found, "Could not find node using GET %s", nodeReqPath)

				reportReqPath := fmt.Sprintf("/api/v0/compliance/reporting/reports/id/%s", entity.ReportUUID)

				for {
					resp, err := tstCtx.DoLBRequest(reportReqPath, lbrequest.WithMethod("POST"))
					require.NoError(tstCtx, err, "Failed to POST %s", reportReqPath)
					defer func() {
						_ = resp.Body.Close()
					}()
					if resp.StatusCode == 200 {
						require.Equal(tstCtx, 200, resp.StatusCode, "POST %s failed", reportReqPath)
						break
					}

					tries++
					if tries >= maxTries {
						assert.Failf(tstCtx, "POST %s failed with status code %d", reportReqPath, resp.StatusCode)
						break
					}
					time.Sleep(2 * time.Duration(tries) * time.Second)
				}
			}
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateComplianceReportDiagnostic(),
	)
}
