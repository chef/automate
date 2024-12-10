package majorupgradechecklist

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/lib/io/fileutils"
	cm "github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/sirupsen/logrus"
)

const (
	MIN_DIRSIZE_GB float64 = 5

	DISKSPACE_CHECK_ERROR = `You do not have minimum space available to continue with this %s. 
Please ensure you have %.2f GB free disk space.
To skip this free disk space check please use --skip-storage-check flag.`
	HAB_DIR                  = "/hab"
	HAB_5GB_FREE_SPACE_ERROR = "\n  - /hab/ directory should have more than 5GB free space"
	DIR_FREE_SPACE_ERROR     = "\n  - %s directory should have more than %.2fGB free space"
	DEST_FLAG_INFO           = "%s\nDestination directory chosen to check free disk space: %s\nTo change destination directory please use --os-dest-data-dir flag"
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

func IsExternalElasticSearch() bool {
	res, err := client.GetAutomateConfig(int64(client.DefaultClientTimeout))
	if err != nil {
		logrus.Error("failed to get elastic search configuration: ", err.Error())
		return false
	}
	return res.Config.GetGlobal().GetV1().GetExternal().GetElasticsearch().GetEnable().GetValue()
}

func CheckSpaceAvailable(isMigration bool, dbDataPath string, version string, skipStorageCheck bool, osDestDataDir string) (bool, error) {

	if skipStorageCheck {
		return true, nil
	}
	habRootPath := getHabRootPath(habrootcmd)
	if !isMigration {
		version, _ = GetMajorVersion(version)
		switch version {
		case "3":
			dbDataPath = habRootPath + "svc/automate-postgresql/data/pgdata"
		case "4":
			dbDataPath = habRootPath + "svc/automate-elasticsearch/data"
		case "5":
			dbDataPath = habRootPath + "svc/automate-postgresql/data/pgdata"
		}
	}
	habFreeSpace, err := cm.GetFreeSpaceinGB(habRootPath)
	if err != nil {
		return false, status.Errorf(status.AvailableSpaceError, err.Error())
	}
	sizeESDir, err := cm.CalDirSizeInGB(dbDataPath)
	if err != nil {
		return false, status.Errorf(status.CalESDirSizeError, err.Error())
	}
	destDir := habRootPath
	eSDirSizeWithBuffer := sizeESDir * 11 / 10
	message := ""
	isDestDirFlag := len(strings.TrimSpace(osDestDataDir)) > 0

	if isDestDirFlag && osDestDataDir != HAB_DIR {
		destDir = osDestDataDir
		sizeDestDir, err := cm.GetFreeSpaceinGB(osDestDataDir)
		if err != nil {
			return false, status.Errorf(status.CalDestDirSizeError, err.Error())
		}
		if habFreeSpace < MIN_DIRSIZE_GB {
			message = HAB_5GB_FREE_SPACE_ERROR
		}
		if sizeDestDir < eSDirSizeWithBuffer {
			message += fmt.Sprintf(DIR_FREE_SPACE_ERROR, osDestDataDir, eSDirSizeWithBuffer)
		}
	} else {
		if habFreeSpace < MIN_DIRSIZE_GB+eSDirSizeWithBuffer {
			message = fmt.Sprintf(DIR_FREE_SPACE_ERROR, habRootPath, MIN_DIRSIZE_GB+eSDirSizeWithBuffer)
		}
	}
	if message != "" {
		if !isMigration && isDestDirFlag {
			message = fmt.Sprintf(DEST_FLAG_INFO, message, destDir)
		}
		return false, status.Errorf(status.InsufficientSpaceError, message)
	}
	return true, nil
}

func SetSeenTrueForExternal() error {
	path := fileutils.GetHabRootPath() + majorupgrade_utils.UPGRADE_METADATA
	res, err := ReadJsonFile(fileutils.GetHabRootPath() + majorupgrade_utils.UPGRADE_METADATA)
	if err != nil {
		return nil
	}

	res.Seen = true
	err = CreateJsonFile(res, path)
	if err != nil {
		return err
	}
	return nil
}
