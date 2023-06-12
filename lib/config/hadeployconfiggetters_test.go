package config

import "testing"

func TestGetConfigInitials(t *testing.T) {
	// Test case 1: Existing infrastructure
	existingInfraConfig := &HaDeployConfig{
		Architecture: &Architecture{
			ExistingInfra: &ConfigInitials{
				SSHUser: "existing_user",
			},
		},
	}
	expectedConfig1 := &ConfigInitials{
		SSHUser: "existing_user",
	}
	actualConfig1 := existingInfraConfig.GetConfigInitials()
	if actualConfig1.SSHUser != expectedConfig1.SSHUser {
		t.Errorf("Expected SSHUser: %s, but got: %s", expectedConfig1.SSHUser, actualConfig1.SSHUser)
	}

	// Test case 2: AWS architecture
	awsConfig := &HaDeployConfig{
		Architecture: &Architecture{
			Aws: &ConfigInitials{
				SSHUser: "aws_user",
			},
		},
	}
	expectedConfig2 := &ConfigInitials{
		SSHUser: "aws_user",
	}
	actualConfig2 := awsConfig.GetConfigInitials()
	if actualConfig2.SSHUser != expectedConfig2.SSHUser {
		t.Errorf("Expected SSHUser: %s, but got: %s", expectedConfig2.SSHUser, actualConfig2.SSHUser)
	}

	// Test case 3: Empty configuration
	emptyConfig := &HaDeployConfig{}
	expectedConfig3 := (*ConfigInitials)(nil)
	actualConfig3 := emptyConfig.GetConfigInitials()
	if actualConfig3 != expectedConfig3 {
		t.Errorf("Expected ConfigInitials: %v, but got: %v", expectedConfig3, actualConfig3)
	}
}

func TestGetObjectStorageConfig(t *testing.T) {
	// Test case 1: Existing infrastructure
	existingInfraConfig := &HaDeployConfig{
		Architecture: &Architecture{
			ExistingInfra: &ConfigInitials{},
		},
		ObjectStorage: &ObjectStorage{
			&ConfigObjectStorage{
				BucketName: "test_bucket",
				AccessKey:  "test_access_key",
				SecretKey:  "test_secret_key",
				Endpoint:   "test_endpoint",
				Region:     "test_region",
			},
		},
	}
	expectedConfig1 := &ConfigObjectStorage{
		BucketName: "test_bucket",
		AccessKey:  "test_access_key",
		SecretKey:  "test_secret_key",
		Endpoint:   "test_endpoint",
		Region:     "test_region",
	}
	actualConfig1 := existingInfraConfig.GetObjectStorageConfig()
	if actualConfig1.BucketName != expectedConfig1.BucketName {
		t.Errorf("Expected BucketName: %s, but got: %s", expectedConfig1.BucketName, actualConfig1.BucketName)
	}
	if actualConfig1.AccessKey != expectedConfig1.AccessKey {
		t.Errorf("Expected AccessKey: %s, but got: %s", expectedConfig1.AccessKey, actualConfig1.AccessKey)
	}
	if actualConfig1.SecretKey != expectedConfig1.SecretKey {
		t.Errorf("Expected SecretKey: %s, but got: %s", expectedConfig1.SecretKey, actualConfig1.SecretKey)
	}
	if actualConfig1.Endpoint != expectedConfig1.Endpoint {
		t.Errorf("Expected Endpoint: %s, but got: %s", expectedConfig1.Endpoint, actualConfig1.Endpoint)
	}
	if actualConfig1.Region != expectedConfig1.Region {
		t.Errorf("Expected Region: %s, but got: %s", expectedConfig1.Region, actualConfig1.Region)
	}

	// Test case 2: Empty configuration
	emptyConfig := &HaDeployConfig{}
	expectedConfig2 := (*ConfigObjectStorage)(nil)
	actualConfig2 := emptyConfig.GetObjectStorageConfig()
	if actualConfig2 != expectedConfig2 {
		t.Errorf("Expected ConfigObjectStorage: %v, but got: %v", expectedConfig2, actualConfig2)
	}
}
