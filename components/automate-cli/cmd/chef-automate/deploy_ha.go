// Copyright © 2017 Chef Software

package main

import (
	ptoml "github.com/pelletier/go-toml"
)

func isA2HADeployment() bool {
	initConfigHAPath := initConfigHAPathFlags.path
	config, err := ptoml.LoadFile(initConfigHAPath)
	if err != nil {
		return false
	}
	if config.Get("architecture.existing_infra") != nil || config.Get("architecture.aws") != nil {
		return true
	} else {
		return false
	}
}
