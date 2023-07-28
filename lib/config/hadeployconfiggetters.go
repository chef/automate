package config

func (config *HaDeployConfig) GetConfigInitials() *ConfigInitials {
	if config.IsExistingInfra() {
		return config.Architecture.ExistingInfra
	} else if config.IsAws() {
		return config.Architecture.Aws
	}
	return nil
}

func (config *HaDeployConfig) GetObjectStorageConfig() *ConfigObjectStorage {
	if config.IsExistingInfra() {
		if config.ObjectStorage == nil {
			return nil
		}
		return config.ObjectStorage.Config
	}
	return nil
}
