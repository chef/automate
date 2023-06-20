package main

import (
	"encoding/json"
	"path/filepath"
)

type configDetails struct {
	AutomateIps          []string `json:"automate_private_ips"`
	ChefServerIps        []string `json:"chef_server_private_ips"`
	PostgresqlIps        []string `json:"postgresql_private_ips"`
	OpensearchIps        []string `json:"opensearch_private_ips"`
	AutomateLBFqdn       string   `json:"automate_lb_fqdn"`
	AutomateFrontendURL  string   `json:"automate_frontend_url"`
	BucketName           string   `json:"bucket_name"`
	OsSnapshotRoleArn    string   `json:"aws_os_snapshot_role_arn"`
	OsSnapshotUserID     string   `json:"os_snapshot_user_access_key_id"`
	OsSnapshotUserSecret string   `json:"os_snapshot_user_access_key_secret"`
}

func fetchAwsConfigFromTerraform() (*configDetails, error) {
	AwsConfigJsonString := convTfvarToJson(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "terraform", "reference_architectures", "deployment", "output.auto.tfvars"))
	awsAutoTfvarConfig, err := getJsonFromTerraformOutputAutoTfVarsFile(AwsConfigJsonString)
	if err != nil {
		return nil, err
	}
	return awsAutoTfvarConfig, nil
}

func getJsonFromTerraformOutputAutoTfVarsFile(jsonString string) (*configDetails, error) {
	params := configDetails{}
	err := json.Unmarshal([]byte(jsonString), &params)
	if err != nil {
		writer.Fail(err.Error())
		return nil, err
	}
	return &params, nil
}
