// Copyright © 2017 Chef Software

package main

import (
	"bytes"
	"container/list"
	"io/ioutil"
	"strings"
	"text/template"

	"github.com/pkg/errors"
)

func getSingleErrorFromList(l *list.List) error {
	errorString := ""
	for e := l.Front(); e != nil; e = e.Next() {
		errorString = errorString + "\n" + e.Value.(string)
	}
	return errors.New(errorString)
}

func writeToA2HARBFile(template string, path string) {
	err := ioutil.WriteFile(path, []byte(template), 0600)
	if err != nil {
		writer.Printf("Writing into A2HA.rb file failed\n")
	}
	writer.Printf("Config written to %s\n", path)
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
