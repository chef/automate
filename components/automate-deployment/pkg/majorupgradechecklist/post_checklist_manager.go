package majorupgradechecklist

import (
	"github.com/pkg/errors"
)

const (
	upgrade_metadata = "/hab/svc/deployment-service/var/upgrade_metadata.json"
)

type PostChecklistManager struct {
	version string
	ci      ChecklistManager
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

func NewPostChecklistManager(version string) (*PostChecklistManager, error) {
	majorVersion, _ := GetMajorVersion(version)
	ci, err := NewChecklistManager(nil, version)
	if err != nil {
		return nil, err
	}
	return &PostChecklistManager{
		version: majorVersion,
		ci:      ci,
	}, nil
}

func (pcm *PostChecklistManager) CreatePostChecklistFile() error {
	params := PostChecklist{}
	params.PostChecklist = append(params.PostChecklist, pcm.ci.GetPostChecklist()...)
	params.Version = pcm.version
	err := CreateJsonFile(&params, upgrade_metadata)
	if err != nil {
		return err
	}

	return nil
}

func (pcm *PostChecklistManager) ReadPostChecklistById(id string) (bool, error) {
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

func (pcm *PostChecklistManager) ReadPendingPostChecklistFile() ([]string, error) {
	var postCmdList []string
	var showPostChecklist = false
	res, err := ReadJsonFile(upgrade_metadata)
	if err != nil {
		return nil, err
	}

	if res.Version == pcm.version {
		for i := 0; i < len(res.PostChecklist); i++ {
			if (!res.PostChecklist[i].Optional && !res.PostChecklist[i].IsExecuted) || (isExternalPG() && !res.Seen) {
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
	} else {
		return nil, errors.Errorf("Failed to read checklist since version didn't match")
	}
}

func (pcm *PostChecklistManager) UpdatePostChecklistFile(id string) error {
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
