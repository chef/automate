package main

import (
	"path/filepath"

	ptoml "github.com/pelletier/go-toml"
)

func getModeFromConfig(configPath []string) (string, error) {
	initConfigHAPath := initConfigHAPathFlags.path
	if len(configPath) > 0 {
		initConfigHAPath = configPath[0]
	}
	if checkIfFileExist(initConfigHAPath) {
		config, err := ptoml.LoadFile(initConfigHAPath)
		if err != nil {
			writer.Println(err.Error())
			return "", err
		}
		if config.Get("architecture.existing_infra") != nil {
			return EXISTING_INFRA_MODE, nil
		} else if config.Get("architecture.aws") != nil {
			return AWS_MODE, nil
		} else {
			return AUTOMATE, nil
		}
	} else if checkIfFileExist(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "a2ha.rb")) {
		return HA_MODE, nil
	} else {
		return AUTOMATE, nil
	}
}
