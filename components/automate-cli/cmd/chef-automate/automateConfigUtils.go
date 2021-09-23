package main

import ptoml "github.com/pelletier/go-toml"

func getModeFromConfig(configPath []string) string {
	initConfigHAPath := initConfigHAPathFlags.path
	if len(configPath) > 0 {
		initConfigHAPath = configPath[0]
	}
	if checkIfFileExist(initConfigHAPath) {
		config, err := ptoml.LoadFile(initConfigHAPath)
		if err != nil {
			writer.Println(err.Error())
			return AUTOMATE
		}
		if config.Get("architecture.existing_infra") != nil {
			return EXISTING_INFRA_MODE
		} else if config.Get("architecture.aws") != nil {
			return AWS_MODE
		} else {
			return AUTOMATE
		}
	} else if checkIfFileExist(initConfigHabA2HAPathFlag.a2haDirPath + "a2ha.rb") {
		return HA_MODE
	} else {
		return AUTOMATE
	}
}
