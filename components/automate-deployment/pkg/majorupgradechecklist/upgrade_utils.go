package majorupgradechecklist

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"math"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	cm "github.com/chef/automate/lib/io/fileutils"
	"github.com/sirupsen/logrus"
)

const (
	MIN_DIRSIZE_GB float64 = 5

	DISKSPACE_CHECK_ERROR = `You do not have minimum space available to continue with this %s. 
Please ensure you have %.2f GB free disk space.
To skip this free disk space check please use --skip-storage-check flag.`
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

func CheckSpaceAvailable(isMigration bool, dbDataPath string, writer cli.FormatWriter, version string, skipStorageCheck bool, osDestDataDir string) (bool, error) {
	habRootPath := getHabRootPath(habrootcmd)
	habDirSize, err := cm.CalDirSizeInGB(habRootPath)

	if err != nil {
		if writer != nil {
			writer.Error(err.Error())
		}
		return false, status.Errorf(status.UnknownError, err.Error())
	}

	// If (/hab) dir size is less than 5GB, then throw error
	// habSpaceAvailable, err := cm.CheckSpaceAvailability(habRootPath, MIN_DIRSIZE_GB)
	// if err != nil || !habSpaceAvailable {
	// 	if writer != nil {
	// 		writer.Errorln(fmt.Sprintf("Hab (%s) directory should have more than %.2fGB free space", habRootPath, MIN_DIRSIZE_GB))
	// 	}
	// 	return false, status.New(status.UnknownError, fmt.Sprintf("Hab (%s) directory should have more than %.2fGB free space.", habRootPath, MIN_DIRSIZE_GB))
	// }

	if !isMigration {
		version, _ = GetMajorVersion(version)
		switch version {
		case "3":
			dbDataPath = habRootPath + "svc/automate-postgresql/data/pgdata"
		case "4":
			dbDataPath = habRootPath + "svc/automate-elasticsearch/data"
		}
	}

	dbDataSize, err := cm.CalDirSizeInGB(dbDataPath)
	if err != nil {
		if writer != nil {
			writer.Error(err.Error())
		}
		return false, status.Errorf(status.UnknownError, err.Error())
	}

	minReqDiskSpace := math.Max(MIN_DIRSIZE_GB, math.Max(habDirSize, dbDataSize)) * 11 / 10

	diskSpaceErrorType := "migration"
	destDir := habRootPath

	if !isMigration {
		if osDestDataDir != "" {
			destDir = osDestDataDir
		}
		if writer != nil {
			resp, err := writer.Confirm(fmt.Sprintf("Ensure destination directory (%s) is having min. %.2f GB free space ?", destDir, minReqDiskSpace))
			if err != nil {
				writer.Error(err.Error())
				return false, status.Errorf(status.UnknownError, err.Error())
			}
			if !resp {
				return false, status.New(status.UnknownError, fmt.Sprintf(diskSpaceError, minReqDiskSpace))
			}
		}
		diskSpaceErrorType = "upgrade"
	}

	if !skipStorageCheck {
		if !isMigration && writer != nil {
			writer.Printf("Destination directory chosen to check free disk space: %s\n", destDir)
			writer.Println("To change destination directory please use --os-dest-data-dir flag")
		}
		spaceAvailable, err := cm.CheckSpaceAvailability(destDir, minReqDiskSpace)
		if err != nil || !spaceAvailable {
			return false, status.New(status.UnknownError, fmt.Sprintf(DISKSPACE_CHECK_ERROR, diskSpaceErrorType, minReqDiskSpace))
		}
	}
	return true, nil
}
