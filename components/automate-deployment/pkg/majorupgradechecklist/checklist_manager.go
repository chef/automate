package majorupgradechecklist

import (
	"bytes"
	"encoding/json"
	"io/ioutil"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/pkg/errors"
)

type PreChecklistManager interface {
	RunChecklist() error
}

type PerCheckList struct {
	Msg      string `json:"msg"`
	Response string `json:"-"`
}

type PostCheckList struct {
	Id         string `json:"id"`
	Msg        string `json:"msg"`
	Cmd        string `json:"cmd"`
	Optional   bool   `json:"optional"`
	IsExecuted bool   `json:"is_executed"`
}

type PerPostChecklist struct {
	currentMajorVersion string
	isMajorVersion      bool
	Version             string          `json:"version"`
	PostChecklist       []PostCheckList `json:"post_checklist"`
	Seen                bool            `json:"seen"`
}

const (
	upgrade_metadata = "/hab/svc/deployment-service/var/upgrade_metadata.json"
)

//NewChecklistManager returns the checklist inspector for given major release

func NewChecklistManager(writer cli.FormatWriter, version, major string) (PreChecklistManager, error) {

	if major == "" {
		major, _ = GetMajorVersion(version)
	}

	switch major {
	case "4":
		return NewV3ChecklistManager(writer, version), nil
	default:
		return nil, status.Errorf(status.UpgradeError, "invalid major version")
	}
}

type Checklist struct {
	Name        string
	Description string
	TestFunc    func(ChecklistHelper) error
}

type ChecklistHelper struct {
	Writer cli.FormatWriter
}
type PostChecklistManager interface {
	CreatePostChecklistFile() error
	ReadPendingPostChecklistFile() ([]string, error)
	ReadPostChecklistById(id string) (bool, error)
	UpdatePostChecklistFile(id string) error
}

func NewCRUDChecklist(version string) PostChecklistManager {
	return NewPerPostChecklist(version)
}

func NewPerPostChecklist(version string) *PerPostChecklist {
	majorVersion, isMajorVersion := GetMajorVersion(version)

	prepost := &PerPostChecklist{}
	prepost.currentMajorVersion = majorVersion
	prepost.isMajorVersion = isMajorVersion
	return prepost
}

func (ci *PerPostChecklist) CreatePostChecklistFile() error {
	params := PerPostChecklist{}
	if isExternalPG() {
		params.PostChecklist = append(params.PostChecklist, postChecklistExternal...)
	} else {
		params.PostChecklist = append(params.PostChecklist, postChecklistEmbedded...)
	}

	params.Version = ci.currentMajorVersion
	err := CreateJsonFile(params, upgrade_metadata)
	if err != nil {
		return err
	}

	return nil
}

func (ci *PerPostChecklist) ReadPostChecklistById(id string) (bool, error) {
	ChecklistId_Found := false
	res, err := ReadJsonFile(upgrade_metadata)
	if err != nil {
		return false, err
	}
	for i := 0; i < len(res.PostChecklist); i++ {

		if res.PostChecklist[i].Id == id {
			ChecklistId_Found = res.PostChecklist[i].IsExecuted
			break

		}
	}
	return ChecklistId_Found, nil
}

func ReadJsonFile(path string) (*PerPostChecklist, error) {
	byteValue, err := ioutil.ReadFile(path)
	if err != nil {
		return nil, err
	}
	params := PerPostChecklist{}

	err = json.Unmarshal(byteValue, &params)
	if err != nil {
		return nil, err
	}
	return &params, nil
}

func CreateJsonFile(params PerPostChecklist, path string) error {
	var buffer bytes.Buffer
	data, err := json.Marshal(params)
	if err != nil {
		return err
	}
	buffer.Write(data)
	buffer.WriteString("\n")
	err = ioutil.WriteFile(path, buffer.Bytes(), 0644)
	if err != nil {
		return err
	}
	return nil
}

func (ci *PerPostChecklist) ReadPendingPostChecklistFile() ([]string, error) {
	var postCmdList []string
	var showPostChecklist = false
	res, err := ReadJsonFile(upgrade_metadata)
	if err != nil {
		return nil, err
	}

	if res.Version == ci.currentMajorVersion {
		if (isExternalPG() && !ci.Seen) || !isExternalPG() {
			for i := 0; i < len(res.PostChecklist); i++ {
				if (!res.PostChecklist[i].Optional && !res.PostChecklist[i].IsExecuted) || !res.Seen {
					showPostChecklist = true
					break
				}
			}

			if showPostChecklist {
				for i := 0; i < len(res.PostChecklist); i++ {
					if !res.PostChecklist[i].IsExecuted {
						postCmdList = append(postCmdList, res.PostChecklist[i].Msg)
					}
				}
			}

			if isExternalPG() {
				res.Seen = true
				err = CreateJsonFile(*res, upgrade_metadata)
				if err != nil {
					return nil, err
				}
			}
			return postCmdList, nil
		}
	}

	return nil, errors.Errorf("Failed to read checklist since version didn't match")
}

func (ci *PerPostChecklist) UpdatePostChecklistFile(id string) error {
	res, err := ReadJsonFile(upgrade_metadata)
	if err != nil {
		return err
	}
	for i, v := range res.PostChecklist {
		if v.Id == id {
			res.PostChecklist[i].IsExecuted = true
		}
	}

	err = CreateJsonFile(*res, upgrade_metadata)
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
