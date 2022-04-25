package majorupgradechecklist

import (
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

const (
	FILE_NAME        = "upgrade_metadata.json"
	UPGRADE_METADATA = "/hab/svc/deployment-service/var/" + FILE_NAME
)

type PostChecklistManager struct {
	version      string
	ci           ChecklistManager
	isExternalDB bool
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
	var writer cli.FormatWriter
	externalDB := false

	majorVersion, _ := GetMajorVersion(version)
	switch majorVersion {
	case "3":
		externalDB = IsExternalPG()
	case "4":
		externalDB = IsExternalElasticSearch(writer)
	}

	ci, err := NewChecklistManager(nil, version)
	if err != nil {
		return &PostChecklistManager{
			version:      majorVersion,
			isExternalDB: externalDB,
		}, err
	}
	return &PostChecklistManager{
		version:      majorVersion,
		isExternalDB: externalDB,
		ci:           ci,
	}, nil
}

func (pcm *PostChecklistManager) CreatePostChecklistFile(path string) error {
	params := PostChecklist{}
	params.PostChecklist = append(params.PostChecklist, pcm.ci.GetPostChecklist()...)
	params.Version = pcm.version
	err := CreateJsonFile(&params, path)
	if err != nil {
		return err
	}

	return nil
}

func (pcm *PostChecklistManager) ReadPostChecklistById(id string, path string) (bool, error) {
	checklistIDIsExecuted := true
	res, err := ReadJsonFile(path)
	if err != nil {
		// overriding error to nil, in the case of file not found
		return checklistIDIsExecuted, nil
	}

	for i := 0; i < len(res.PostChecklist); i++ {
		if res.PostChecklist[i].Id == id {
			checklistIDIsExecuted = res.PostChecklist[i].IsExecuted
			break
		}
	}

	return checklistIDIsExecuted, nil
}

func (pcm *PostChecklistManager) ReadPendingPostChecklistFile(path string) ([]string, error) {
	var postCmdList []string
	var showPostChecklist = false
	res, err := ReadJsonFile(path)
	if err != nil {
		return postCmdList, err
	}

	if res.Version == pcm.version {
		for i := 0; i < len(res.PostChecklist); i++ {
			if (!res.PostChecklist[i].Optional && !res.PostChecklist[i].IsExecuted) || (pcm.isExternalDB && !res.Seen) {
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

		if pcm.isExternalDB {
			res.Seen = true
			err = CreateJsonFile(res, path)
			if err != nil {
				return postCmdList, err
			}
		}
	} else {
		return postCmdList, status.Errorf(status.UpgradeError, "Failed to read checklist since version didn't match")
	}
	return postCmdList, nil
}

func (pcm *PostChecklistManager) UpdatePostChecklistFile(id string, path string) error {
	res, err := ReadJsonFile(path)
	if err != nil {
		return err
	}
	for i, v := range res.PostChecklist {
		if v.Id == id {
			res.PostChecklist[i].IsExecuted = true
		}
	}

	err = CreateJsonFile(res, path)
	if err != nil {
		return err
	}
	return nil
}
