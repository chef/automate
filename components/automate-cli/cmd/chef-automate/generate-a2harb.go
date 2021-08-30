// Copyright © 2017 Chef Software

package main

import (
	"bytes"
	"html/template"
	"io/ioutil"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	ptoml "github.com/pelletier/go-toml"
	"github.com/pkg/errors"
)

func readConfigAndWriteToFile() error {
	writer.Printf("reading configs from toml file")
	initConfigHAPath := initConfigHAPathFlags.path
	templateBytes, err := ioutil.ReadFile(initConfigHAPath)
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	config, err := ptoml.LoadFile(initConfigHAPath)
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	architectureAws := config.Get("architecture.aws")
	if architectureAws != nil {
		AwsConfig := AwsConfigToml{}
		err := ptoml.Unmarshal(templateBytes, &AwsConfig)
		if err != nil {
			return status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
		}
		finalTemplate := renderSettingsToA2HARBFile(awsA2harbTemplate, AwsConfig)
		writeToA2HARBFile(finalTemplate, "a2ha.rb")
	} else if config.Get("architecture.existing_infra") != nil {
		ExitiingInfraConfig := ExistingInfraConfigToml{}
		err := ptoml.Unmarshal(templateBytes, &ExitiingInfraConfig)
		if err != nil {
			return status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
		}
		finalTemplate := renderSettingsToA2HARBFile(existingNodesA2harbTemplate, ExitiingInfraConfig)
		writeToA2HARBFile(finalTemplate, "a2ha.rb")
	} else {
		msg := "Invalid toml configuration"
		return status.Wrap(errors.New(msg), status.ConfigError, msg)
	}
	return nil
}

func writeToA2HARBFile(template string, path string) {
	err := ioutil.WriteFile(path, []byte(template), 0600)
	if err != nil {
		writer.Printf("Writing into A2HA.rb file failed")
	}
	writer.Printf("Config written to %s", path)
}

func renderSettingsToA2HARBFile(templateName string, data interface{}) string {
	temp := template.Must(template.New("init").
		Funcs(template.FuncMap{"StringsJoin": strings.Join}).
		Parse(templateName))

	var buf bytes.Buffer
	err := temp.Execute(&buf, data)
	if err != nil {
		writer.Printf("some error occurred while rendering template \n %s", err)
	}
	finalTemplate := buf.String()
	return finalTemplate
}
