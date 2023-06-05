package config

import "strconv"

func (config *HaDeployConfig) GetSSHUser() string {
	if config.IsExistingInfra() {
		return config.Architecture.ExistingInfra.SSHUser
	} else if config.IsAws() {
		return config.Architecture.Aws.SSHUser
	}
	return ""
}

func (config *HaDeployConfig) GetSSHGroupName() string {
	if config.IsExistingInfra() {
		return config.Architecture.ExistingInfra.SSHGroupName
	} else if config.IsAws() {
		return config.Architecture.Aws.SSHGroupName
	}
	return ""
}

func (config *HaDeployConfig) GetSSHKeyFile() string {
	if config.IsExistingInfra() {
		return config.Architecture.ExistingInfra.SSHKeyFile
	} else if config.IsAws() {
		return config.Architecture.Aws.SSHKeyFile
	}
	return ""
}

func (config *HaDeployConfig) GetSSHPort() string {
	if config.IsExistingInfra() {
		return config.Architecture.ExistingInfra.SSHPort
	} else if config.IsAws() {
		return config.Architecture.Aws.SSHPort
	}
	return ""
}

func (config *HaDeployConfig) GetSecretsKeyFile() string {
	if config.IsExistingInfra() {
		return config.Architecture.ExistingInfra.SecretsKeyFile
	} else if config.IsAws() {
		return config.Architecture.Aws.SecretsKeyFile
	}
	return ""
}

func (config *HaDeployConfig) GetSecretsStoreFile() string {
	if config.IsExistingInfra() {
		return config.Architecture.ExistingInfra.SecretsStoreFile
	} else if config.IsAws() {
		return config.Architecture.Aws.SecretsStoreFile
	}
	return ""
}

func (config *HaDeployConfig) GetSudoPassword() string {
	if config.IsExistingInfra() {
		return config.Architecture.ExistingInfra.SudoPassword
	} else if config.IsAws() {
		return config.Architecture.Aws.SudoPassword
	}
	return ""
}

func (config *HaDeployConfig) GetLoggingMonitoringManagement() string {
	if config.IsExistingInfra() {
		return config.Architecture.ExistingInfra.LoggingMonitoringManagement
	} else if config.IsAws() {
		return config.Architecture.Aws.LoggingMonitoringManagement
	}
	return ""
}

func (config *HaDeployConfig) GetArchitecture() string {
	if config.IsExistingInfra() {
		return config.Architecture.ExistingInfra.Architecture
	} else if config.IsAws() {
		return config.Architecture.Aws.Architecture
	}
	return ""
}

func (config *HaDeployConfig) GetWorkspacePath() string {
	if config.IsExistingInfra() {
		return config.Architecture.ExistingInfra.WorkspacePath
	} else if config.IsAws() {
		return config.Architecture.Aws.WorkspacePath
	}
	return ""
}

func (config *HaDeployConfig) GetBackupMount() string {
	if config.IsExistingInfra() {
		return config.Architecture.ExistingInfra.BackupMount
	} else if config.IsAws() {
		return config.Architecture.Aws.BackupMount
	}
	return ""
}

func (config *HaDeployConfig) GetBackupConfig() string {
	if config.IsExistingInfra() {
		return config.Architecture.ExistingInfra.BackupConfig
	} else if config.IsAws() {
		return config.Architecture.Aws.BackupConfig
	}
	return ""
}

func (config *HaDeployConfig) GetS3BucketName() string {
	if config.IsExistingInfra() {
		return config.ObjectStorage.Config.BucketName
	} else if config.IsAws() {
		return config.Architecture.Aws.S3BucketName
	}
	return ""
}

func (config *HaDeployConfig) GetObjectStorageAccessKey() string {
	if config.IsExistingInfra() {
		return config.ObjectStorage.Config.AccessKey
	}
	return ""
}

func (config *HaDeployConfig) GetObjectStorageSecretKey() string {
	if config.IsExistingInfra() {
		return config.ObjectStorage.Config.SecretKey
	}
	return ""
}

func (config *HaDeployConfig) GetObjectStorageEndpoint() string {
	if config.IsExistingInfra() {
		return config.ObjectStorage.Config.Endpoint
	}
	return ""
}

func (config *HaDeployConfig) GetObjectStorageRegion() string {
	if config.IsExistingInfra() {
		return config.ObjectStorage.Config.Region
	}
	return ""
}

func (config *HaDeployConfig) GetAutomateNodeCount() int {
	// Since we are handeling type check in Verify error handling not needed
	count, _ := strconv.Atoi(config.Automate.Config.InstanceCount)
	return count
}

func (config *HaDeployConfig) GetChefServerNodeCount() int {
	count, _ := strconv.Atoi(config.ChefServer.Config.InstanceCount)
	return count
}

func (config *HaDeployConfig) GetPostgresqlNodeCount() int {
	count, _ := strconv.Atoi(config.Postgresql.Config.InstanceCount)
	return count
}
func (config *HaDeployConfig) GetOpenSearchNodeCount() int {
	count, _ := strconv.Atoi(config.Opensearch.Config.InstanceCount)
	return count
}

func (config *HaDeployConfig) GetAutomateNodeIps() []string {

	count, _ := strconv.Atoi(config.Automate.Config.)
	return count
}

func (config *HaDeployConfig) GetChefServerNodeIps() []string  {
	count, _ := strconv.Atoi(config.ChefServer.Config.InstanceCount)
	return count
}

func (config *HaDeployConfig) GetPostgresqlNodeIps()  []string  {
	count, _ := strconv.Atoi(config.Postgresql.Config.InstanceCount)
	return count
}
func (config *HaDeployConfig) GetOpenSearchNodeIps()  []string  {
	count, _ := strconv.Atoi(config.Opensearch.Config.InstanceCount)
	return count
}
