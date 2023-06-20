package main

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
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
	if err != nil {
		fmt.Println(err)
		return
	}

	dirPath := filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", "reference_architectures", "deployment")
	filePath := dirPath + "/output.auto.tfvars"
	err = os.MkdirAll(dirPath, os.ModePerm)
	if err != nil {
		fmt.Println("Error creating directories:", err)
		return
	}

	file, err := os.Create(filePath)
	if err != nil {
		fmt.Println("Error creating file:", err)
		return
	}
	defer os.Remove(file.Name())

	tests := []struct {
		TestName       string
		ExpectedOutput *configDetails
		WriteString    string
		ExpectedError  error
	}{
		{
			TestName:       "Valid Content",
			ExpectedOutput: params,
			WriteString:    OutputAutoTfvarsFileContent,
			ExpectedError:  nil,
		},
		{
			TestName:       "Invalid Content",
			ExpectedOutput: nil,
			WriteString:    "test",
			ExpectedError:  errors.New(""),
		},
	}

	for _, e := range tests {
		_, err = file.WriteString(e.WriteString)
		assert.NoError(t, err)
		t.Run(e.TestName, func(t *testing.T) {
			res, err := fetchAwsConfigFromTerraform()
			if e.ExpectedError != nil {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
			assert.Equal(t, e.ExpectedOutput, res)
		})
	}

	err = file.Close()
	if err != nil {
		fmt.Println(err)
		return
	}
}
