package majorupgradechecklist

import (
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/sirupsen/logrus"
)

const (
	FILE_NAME        = "upgrade_metadata.json"
	UPGRADE_METADATA = "/hab/svc/deployment-service/var/" + FILE_NAME
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
		return &PostChecklistManager{
			version: majorVersion,
		}, err
	}
	return &PostChecklistManager{
		version: majorVersion,
		ci:      ci,
	}, nil
}

func (pcm *PostChecklistManager) CreatePostChecklistFile(path string, isExecuted bool) error {
	params := PostChecklist{}
	params.PostChecklist = append(params.PostChecklist, pcm.ci.GetPostChecklist(isExecuted)...)
	params.Version = pcm.version
	err := CreateJsonFile(&params, path)
	if err != nil {
		return err
	}

	return nil
}

func (pcm *PostChecklistManager) ReadPostChecklistById(id string, path string) (bool, error) {
	ChecklistId_Found := false
	res, err := ReadJsonFile(path)
	if err != nil {
		logrus.Info("Failed to read post checklist by id:", err)
		return ChecklistId_Found, nil
	}
	for i := 0; i < len(res.PostChecklist); i++ {

		if res.PostChecklist[i].Id == id {
			ChecklistId_Found = res.PostChecklist[i].IsExecuted
			break

		}
	}
	return ChecklistId_Found, nil
}

func (pcm *PostChecklistManager) ReadPendingPostChecklistFile(path string, isExternalPG bool) ([]string, error) {
	var postCmdList []string
	var showPostChecklist = false
	res, err := ReadJsonFile(path)
	if err != nil {
		logrus.Info("Failed to read pending post checklist:", err)
		return postCmdList, nil
	}

	if res.Version == pcm.version {
		for i := 0; i < len(res.PostChecklist); i++ {
			if (!res.PostChecklist[i].Optional && !res.PostChecklist[i].IsExecuted) || (isExternalPG && !res.Seen) {
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

		if isExternalPG {
			res.Seen = true
			err = CreateJsonFile(res, path)
			if err != nil {
				logrus.Info("Failed to update pending post checklist for external database:", err)
			}
		}
	} else {
		logrus.Info("Failed to read checklist since version didn't match")
		return postCmdList, status.Errorf(status.UpgradeError, "Failed to read checklist since version didn't match")
	}
	return postCmdList, nil
}

func (pcm *PostChecklistManager) UpdatePostChecklistFile(id string, path string) error {
	res, err := ReadJsonFile(path)
	if err != nil {
		logrus.Info("Failed to read to update post checklist:", err)
		return nil
	}
	for i, v := range res.PostChecklist {
		if v.Id == id {
			res.PostChecklist[i].IsExecuted = true
		}
	}

	err = CreateJsonFile(res, path)
	if err != nil {
		logrus.Info("Failed to update post checklist:", err)
	}
	return nil
}
