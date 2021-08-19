// Copyright © 2017 Chef Software

package main

import (
	"bytes"
	"fmt"
	"html/template"
	"io/ioutil"
	"strings"

	ptoml "github.com/pelletier/go-toml"
)

func readConfigAndWriteToFile() error {
	fmt.Printf("reading configs from toml file")
	initConfigHAPath := initConfigHAPathFlags.path
	templateBytes, err := ioutil.ReadFile(initConfigHAPath)
	config, err := ptoml.LoadFile(initConfigHAPath)
	if err != nil {
		fmt.Println(err.Error())
	}
	architectureAws := config.Get("architecture.aws")
	if architectureAws != nil {
		AwsConfig := AwsConfigToml{}
		ptoml.Unmarshal(templateBytes, &AwsConfig)
		finalTemplate := renderSettingsToA2HARBFile(awsA2harbTemplate, AwsConfig)
		writeToA2HARBFile(finalTemplate, "a2ha.rb")
	} else if config.Get("architecture.existing_infra") != nil {
		ExitiingInfraConfig := ExistingInfraConfigToml{}
		ptoml.Unmarshal(templateBytes, &ExitiingInfraConfig)
		finalTemplate := renderSettingsToA2HARBFile(existingNodesA2harbTemplate, ExitiingInfraConfig)
		writeToA2HARBFile(finalTemplate, "a2ha.rb")
	} else {
		writer.Printf("Invalid toml configuration")
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

func convertStructArrayToStringArray(data []interface{}) []string {
	dataArray := make([]string, len(data))
	for i := range dataArray {
		dataArray[i] = data[i].(string)
	}
	return dataArray
}
