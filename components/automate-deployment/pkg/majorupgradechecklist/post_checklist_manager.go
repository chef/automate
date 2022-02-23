package majorupgradechecklist

import (
	"github.com/pkg/errors"
)

const (
	upgrade_metadata = "/hab/svc/deployment-service/var/upgrade_metadata.json"
)

type PostChecklistManager struct {
	version string
}

type PostCheckListItem struct {
	Id         string `json:"id"`
	Msg        string `json:"msg"`
	Cmd        string `json:"cmd"`
	Optional   bool   `json:"optional"`
	IsExecuted bool   `json:"is_executed"`
}

type PostChecklist struct {
	Version       string              `json:"version"`
	PostChecklist []PostCheckListItem `json:"post_checklist"`
	Seen          bool                `json:"seen"`
}

func NewPostChecklistManager(version string) PostChecklistManager {
	majorVersion, _ := GetMajorVersion(version)

	return PostChecklistManager{
		version: majorVersion,
	}
}

func (ci *PostChecklistManager) CreatePostChecklistFile() error {
	params := PostChecklist{}
	if isExternalPG() {
		params.PostChecklist = append(params.PostChecklist, postChecklistExternal...)
	} else {
		params.PostChecklist = append(params.PostChecklist, postChecklistEmbedded...)
	}

	params.Version = ci.version
	err := CreateJsonFile(&params, upgrade_metadata)
	if err != nil {
		return err
	}

	return nil
}

func (ci *PostChecklistManager) ReadPostChecklistById(id string) (bool, error) {
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

func (ci *PostChecklistManager) ReadPendingPostChecklistFile() ([]string, error) {
	var postCmdList []string
	var showPostChecklist = false
	res, err := ReadJsonFile(upgrade_metadata)
	if err != nil {
		return nil, err
	}

	if res.Version == ci.version {
		if (isExternalPG() && !res.Seen) || !isExternalPG() {
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
				err = CreateJsonFile(res, upgrade_metadata)
				if err != nil {
					return nil, err
				}
			}
			return postCmdList, nil
		}
	} else {
		return nil, errors.Errorf("Failed to read checklist since version didn't match")
	}
	return nil, nil
}

func (ci *PostChecklistManager) UpdatePostChecklistFile(id string) error {
	res, err := ReadJsonFile(upgrade_metadata)
	if err != nil {
		return err
	}
	for i, v := range res.PostChecklist {
		if v.Id == id {
			res.PostChecklist[i].IsExecuted = true
		}
	}

	err = CreateJsonFile(res, upgrade_metadata)
	if err != nil {
		return err
	}
	return nil
}
