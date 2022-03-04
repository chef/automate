package majorupgradechecklist

import (
	"bytes"
	"encoding/json"
	"io/ioutil"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
)

type Checklist struct {
	Name        string
	Description string
	TestFunc    func(ChecklistHelper) error
}

type ChecklistHelper struct {
	Writer cli.FormatWriter
}

func ReadJsonFile(path string) (*PostChecklist, error) {
	byteValue, err := ioutil.ReadFile(path) // nosemgrep
	if err != nil {
		return nil, err
	}
	params := PostChecklist{}

	err = json.Unmarshal(byteValue, &params)
	if err != nil {
		return nil, err
	}
	return &params, nil
}

func CreateJsonFile(params *PostChecklist, path string) error {
	var buffer bytes.Buffer
	data, err := json.Marshal(*params)
	if err != nil {
		return err
	}
	buffer.Write(data)
	buffer.WriteString("\n")
	err = ioutil.WriteFile(path, buffer.Bytes(), 0644) // nosemgrep
	if err != nil {
		return err
	}
	return nil
}

func GetMajorVersion(version string) (string, bool) {
	resp, is_major_version := manifest.IsSemVersionFmt(version)
	if is_major_version {
		return resp, is_major_version
	}
	return version, false
}
