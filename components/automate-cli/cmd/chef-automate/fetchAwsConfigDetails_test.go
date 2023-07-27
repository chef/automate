package main

import (
	"encoding/json"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

var AwsAutoTfvarsJsonString = `
{"automate_private_ips":["10.0.130.162", "10.0.153.152"], "chef_server_private_ips":["10.0.136.84", "10.0.149.79"], "postgresql_private_ips":["10.0.135.82", "10.0.159.64", "10.0.161.255"], "opensearch_private_ips":["10.0.133.254", "10.0.144.64", "10.0.163.250"], "automate_lb_fqdn":"A2-df60949d-automate-lb-1317318119.eu-west-2.elb.amazonaws.com", "automate_frontend_url":"https://A2-df60949d-automate-lb-1317318119.eu-west-2.elb.amazonaws.com", "bucket_name":"test", "aws_os_snapshot_role_arn":" ", "os_snapshot_user_access_key_id":" ", "os_snapshot_user_access_key_secret":" "}
`

var OutputAutoTfvarsFileContent = `automate_private_ips         = ["10.0.130.162", "10.0.153.152"]
chef_server_private_ips      = ["10.0.136.84", "10.0.149.79"]
postgresql_private_ips       = ["10.0.135.82", "10.0.159.64", "10.0.161.255"]
opensearch_private_ips    = ["10.0.133.254", "10.0.144.64", "10.0.163.250"]
automate_lb_fqdn                = "A2-df60949d-automate-lb-1317318119.eu-west-2.elb.amazonaws.com"
automate_frontend_url       = "https://A2-df60949d-automate-lb-1317318119.eu-west-2.elb.amazonaws.com"
bucket_name                  = "test"
aws_os_snapshot_role_arn     = " "
os_snapshot_user_access_key_id = " "
os_snapshot_user_access_key_secret = " "`

func Test_fetchAwsConfigFromTerraform(t *testing.T) {
	params := new(configDetails)
	err := json.Unmarshal([]byte(AwsAutoTfvarsJsonString), &params)
	assert.NoError(t, err)

	tests := []struct {
		TestName            string
		ExpectedOutput      *configDetails
		WriteString         string
		ExpectedError       error
		ConvTfvarToJsonFunc func(string) string
	}{
		{
			TestName:       "Valid Content",
			ExpectedOutput: params,
			WriteString:    OutputAutoTfvarsFileContent,
			ExpectedError:  nil,
			ConvTfvarToJsonFunc: func(string) string {
				return AwsAutoTfvarsJsonString
			},
		},
		{
			TestName:       "Invalid Content",
			ExpectedOutput: nil,
			WriteString:    "test",
			ExpectedError:  errors.New(""),
			ConvTfvarToJsonFunc: func(string) string {
				return ""
			},
		},
	}

	for _, e := range tests {
		t.Run(e.TestName, func(t *testing.T) {
			ConvTfvarToJsonFunc = e.ConvTfvarToJsonFunc
			res, err := fetchAwsConfigFromTerraform()
			if e.ExpectedError != nil {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
			assert.Equal(t, e.ExpectedOutput, res)
		})
	}
}
