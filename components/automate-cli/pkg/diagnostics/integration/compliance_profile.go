package integration

import (
	"archive/zip"
	"bytes"
	"fmt"
	"io/ioutil"
	"text/template"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const complianceProfileInspecYamlTemplateStr = `
name: {{ .Name }}
title: {{ .Name }}
maintainer: Chef Software, Inc.
copyright: Chef Software, Inc.
copyright_email: support@chef.io
license: Proprietary, All rights reserved
summary: Integration Diagnostic Test
version: {{ .Version }}
supports:
  - os-family: unix
`
const complianceProfileControlStr = `
title 'Filesystem Configuration'

control 'fs-1' do
  impact 0.5
  title 'Create a separate partition for /tmp'
  desc '
    Since /tmp is world-writable, avoid resource
    exhaustion on the host by mounting this folder
    to a separate partition.
  '
  describe mount('/tmp') do
    it { should be_mounted }
  end
end
`

type complianceProfileSave struct {
	Name    string `json:"name"`
	Version string `json:"version"`
	Owner   string `json:"owner"`
}

func createComplianceProfileZip(inspecYaml string, sampleControl string) ([]byte, error) {
	buf := new(bytes.Buffer)
	w := zip.NewWriter(buf)

	var files = []struct {
		Name, Body string
	}{
		{"inspec.yml", inspecYaml},
		{"controls/", ""},
		{"controls/sample_spec.rb", sampleControl},
	}

	for _, file := range files {
		f, err := w.Create(file.Name)
		if err != nil {
			return nil, errors.Wrapf(err, "Failed to create %s in zip", file.Name)
		}

		if file.Body != "" {
			_, err = f.Write([]byte(file.Body))

			if err != nil {
				return nil, errors.Wrapf(err, "Failed to write data %s in zip", file.Name)
			}
		}
	}

	err := w.Close()

	if err != nil {
		return nil, errors.Wrap(err, "Failed to close zip writer")
	}

	return buf.Bytes(), nil
}

// CreateComplianceProfileDiagnostic create the diagnostic struct for compliance profile
func CreateComplianceProfileDiagnostic() diagnostics.Diagnostic {
	tmpl := template.Must(template.New("compliance-profile").Parse(complianceProfileInspecYamlTemplateStr))

	return diagnostics.Diagnostic{
		Name: "compliance-profile",
		Tags: diagnostics.Tags{"compliance"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			buf := bytes.NewBuffer([]byte{})
			ts := time.Now()
			owner := "integration-diagnostic"
			name := "integration-diagnostic-" + ts.Format("20060102150405")
			version := "1.1.1"
			err := tmpl.Execute(buf, struct {
				Owner   string
				Name    string
				Version string
			}{
				Owner:   owner,
				Name:    name,
				Version: version,
			})

			if err != nil {
				return err
			}

			zipData, err := createComplianceProfileZip(buf.String(), complianceProfileControlStr)
			if err != nil {
				return err
			}

			reqPath := fmt.Sprintf("/api/v0/compliance/profiles?contentType=application/zip&owner=%s", owner)
			resp, err := tstCtx.DoLBRequest(
				reqPath,
				lbrequest.WithMethod("POST"),
				lbrequest.WithFormFile("profile.zip", zipData),
			)

			if err != nil {
				return errors.Wrapf(err, "Request POST %s failed", reqPath)
			}
			defer func() {
				_ = resp.Body.Close()
			}()

			if resp.StatusCode != 200 {
				err = errors.New("Status code not 200")
				respBody, _ := ioutil.ReadAll(resp.Body)
				return errors.Wrapf(err, "Request POST %s failed with %d\nResponse Body:\n%s", reqPath, resp.StatusCode, respBody)
			}

			tstCtx.SetValue("compliance-profile", complianceProfileSave{
				Owner:   owner,
				Name:    name,
				Version: version,
			})
			return err
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := complianceProfileSave{}
			err := tstCtx.GetValue("compliance-profile", &loaded)
			require.NoError(tstCtx, err, "Generated context was not found")

			reqPath := fmt.Sprintf("/api/v0/compliance/profiles/read/%s/%s/version/%s", loaded.Owner, loaded.Name, loaded.Version)
			resp, err := tstCtx.DoLBRequest(reqPath)
			require.NoError(tstCtx, err, "Failed to GET %s", reqPath)
			defer func() {
				_ = resp.Body.Close()
			}()
			assert.Equal(tstCtx, 200, resp.StatusCode, "GET %s did not respond with 200", reqPath)
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := complianceProfileSave{}
			err := tstCtx.GetValue("compliance-profile", &loaded)
			if err != nil {
				return errors.Wrap(err, "Generated context was not found")
			}

			reqPath := fmt.Sprintf("/api/v0/compliance/profiles/%s/%s/version/%s", loaded.Owner, loaded.Name, loaded.Version)
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
		CreateComplianceProfileDiagnostic(),
	)
}
