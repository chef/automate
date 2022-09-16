package majorupgradechecklist

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"math"
	"net"
	"net/http"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/pkg/errors"
)

type indexVersion struct {
	Settings struct {
		Index struct {
			Version struct {
				CreatedString string `json:"created_string"`
				Created       string `json:"created"`
			} `json:"version"`
		} `json:"index"`
	} `json:"settings"`
}

type indexDetails struct {
	Name    string
	Version string
	Error   string
}

type V4ChecklistManager struct {
	writer       cli.FormatWriter
	version      string
	isExternalES bool
}

type IndexInfo struct {
	Settings struct {
		Index struct {
			Version struct {
				CreatedString string `json:"created_string"`
				Created       string `json:"created"`
			} `json:"version"`
		} `json:"index"`
	} `json:"settings"`
}

type indexData struct {
	Name          string
	MajorVersion  int64
	CreatedString string
	IsDeleted     bool
}

const (
	initMsgV4 = `This is a Major upgrade. 
========================

  1) In this release Elastic Search is migrated to Open Search
  2) This upgrade will require special care. 

===== Your installation is using %s Elastic Search =====

  Please confirm this checklist that you have taken care of these steps 
  before continuing with the Upgrade to version %s:
`

	externalESUpgradeMsg = "You are ready with your external open search with migrated data from elastic search, this should be done with the help of your Administrator"

	externalESUpgradeError = "As you are using external elastic search, migrations of data to open search is required before the upgrade"

	shardingError = "sharding has to be disabled before upgrade"

	s3UrlError = "Upgrade terminated as automate version 4 will only support https://s3.amazonaws.com pattern"

	run_os_data_migrate = `Migrate Data from Elastic Search to Open Search using this command:
     $ ` + run_os_data_migrate_cmd

	run_os_data_migrate_cmd = `chef-automate post-major-upgrade migrate --data=es`

	run_os_data_cleanup = `If you are sure all data is available in Upgraded Automate, then we can free up old elastic search Data by running: 
     $ ` + run_os_data_cleanup_cmd

	run_os_data_cleanup_cmd = `chef-automate post-major-upgrade clear-data --data=es`

	disable_maintenance_mode = `Disable the maintenance mode if you enabled previously using:
	$ ` + disable_maintenance_mode_cmd

	disable_maintenance_mode_cmd = `chef-automate maintenance off`
	enable_maintenance_mode_cmd  = `chef-automate maintenance on`
	filename                     = "s3.toml"
	automatePatchCmd             = "chef-automate config patch %s"
	habrootcmd                   = "HAB_LICENSE=accept-no-persist hab pkg path chef/deployment-service"
	s3regex                      = "https?://s3.(.*).amazonaws.com"
	s3EndpointConf               = `
		[global.v1.backups.s3.bucket]
			endpoint = "https://s3.amazonaws.com"
	`
	urlChangeMessage = `Your S3 url in backup config is changed from %s to https://s3.amazonaws.com. 
This is because automate version 4 now only supports this format due to AWS SDK upgrade.
`
	msg            = "\nFollow the guide below to learn more about reindexing:\nhttps://www.elastic.co/guide/en/elasticsearch/reference/6.8/docs-reindex.html"
	oldIndexError  = "The index %s is from an older version of elasticsearch version %s.\nPlease reindex in elasticsearch 6. %s\n%s"
	indexBatchSize = 10

	maintenanceModeMsg = "This upgrade put the system in maintenance mode. During that period no new ingestion of data can happen. \n The maintenance mode will be switched off automatically at the end of a successful upgrade. But in case of an unsuccessful upgrade, you have to set it ‘Off’ manually.\nAre you ready to proceed?"
)

var sourceList = []string{".automate", ".locky", "saved-searches", ".tasks"}

var postChecklistV4Embedded = []PostCheckListItem{
	{
		Id:         "upgrade_status",
		Msg:        run_chef_automate_upgrade_status,
		Cmd:        run_chef_automate_upgrade_status_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "migrate_es",
		Msg:        run_os_data_migrate,
		Cmd:        run_os_data_migrate_cmd,
		IsExecuted: false,
	}, {
		Id:         "check_ui",
		Msg:        ui_check,
		Cmd:        "",
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "clean_up",
		Msg:        run_os_data_cleanup,
		Cmd:        run_os_data_cleanup_cmd,
		Optional:   true,
		IsExecuted: false,
	},
}

var postChecklistV4External = []PostCheckListItem{
	{
		Id:         "upgrade_status",
		Msg:        run_chef_automate_upgrade_status,
		Cmd:        run_chef_automate_upgrade_status_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "status",
		Msg:        run_chef_automate_status,
		Cmd:        run_chef_automate_status_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "disable_maintenance_mode",
		Msg:        disable_maintenance_mode,
		Cmd:        disable_maintenance_mode_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "check_ui",
		Msg:        ui_check,
		Cmd:        "",
		Optional:   true,
		IsExecuted: false,
	},
}

func NewV4ChecklistManager(writer cli.FormatWriter, version string) *V4ChecklistManager {
	return &V4ChecklistManager{
		writer:       writer,
		version:      version,
		isExternalES: IsExternalElasticSearch(),
	}
}

func (ci *V4ChecklistManager) GetPostChecklist() []PostCheckListItem {
	var postChecklist []PostCheckListItem
	if ci.isExternalES {
		postChecklist = postChecklistV4External
	} else {
		postChecklist = postChecklistV4Embedded
	}
	return postChecklist
}

func (ci *V4ChecklistManager) RunChecklist(timeout int64, flags ChecklistUpgradeFlags) error {
	var dbType string
	checklists := []Checklist{}
	var postcheck []PostCheckListItem

	if ci.isExternalES {
		dbType = "External"
		postcheck = postChecklistV4External
		checklists = append(checklists, []Checklist{downTimeCheckV4(), backupCheck(), replaceS3Url(), externalESUpgradeCheck(),
			postChecklistIntimationCheckV4(!ci.isExternalES)}...)
	} else {
		dbType = "Embedded"
		postcheck = postChecklistV4Embedded
		checklists = append(checklists, []Checklist{storeSearchEngineSettings(), deleteA1Indexes(timeout), deleteStaleIndices(timeout), downTimeCheckV4(), backupCheck(), replaceS3Url(), diskSpaceCheck(ci.version, flags.SkipStorageCheck, flags.OsDestDataDir),
			disableSharding(), postChecklistIntimationCheckV4(!ci.isExternalES)}...)
	}
	checklists = append(checklists, showPostChecklist(&postcheck), promptUpgradeContinueV4(!ci.isExternalES))

	helper := ChecklistHelper{
		Writer: ci.writer,
	}

	ci.writer.Println(fmt.Sprintf(initMsgV4, dbType, ci.version)) //display the init message
	for _, item := range checklists {
		if item.TestFunc == nil {
			continue
		}
		if err := item.TestFunc(helper); err != nil {
			return errors.Wrap(err, "one of the checklist was not accepted/satisfied for upgrade")
		}
	}
	return nil
}

func postChecklistIntimationCheckV4(isEmbedded bool) Checklist {
	return Checklist{
		Name:        "post_checklist_intimation_acceptance",
		Description: "confirmation check for post checklist intimation",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("After this upgrade completes, you will have to run Post upgrade steps to ensure your data is migrated and your Automate is ready for use")
			if err != nil {
				h.Writer.Error(err.Error())

				shardError := enableSharding(h, isEmbedded)
				if shardError != nil {
					h.Writer.Error(shardError.Error())
				}

				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(postChecklistIntimationError)

				shardError := enableSharding(h, isEmbedded)
				if shardError != nil {
					h.Writer.Error(shardError.Error())
				}

				return status.New(status.InvalidCommandArgsError, postChecklistIntimationError)
			}
			return nil
		},
	}
}

func promptUpgradeContinueV4(isEmbedded bool) Checklist {
	return Checklist{
		Name:        "post_checklist",
		Description: "display post checklist and ask for final confirmation",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm(v3_post_checklist_confirmation)
			if err != nil {
				h.Writer.Error(err.Error())

				shardError := enableSharding(h, isEmbedded)
				if shardError != nil {
					h.Writer.Error(shardError.Error())
				}

				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error("end user not ready to upgrade")
				shardError := enableSharding(h, isEmbedded)
				if shardError != nil {
					h.Writer.Error(shardError.Error())
				}
				h.Writer.Println("Finished enabling sharding.")
				return status.New(status.InvalidCommandArgsError, "end user not ready to upgrade")
			}
			return nil
		},
	}
}

func enableSharding(h ChecklistHelper, isEmbedded bool) error {
	if isEmbedded {
		h.Writer.Println("\nEnabling sharding...")
		return reEnableShardAllocation()
	}
	return nil
}

func downTimeCheckV4() Checklist {
	return Checklist{
		Name:        "down_time_acceptance",
		Description: "confirmation for downtime",
		TestFunc: func(h ChecklistHelper) error {
			config, err := client.GetAutomateConfig(int64(client.DefaultClientTimeout))
			if err != nil {
				h.Writer.Errorln("failed to get  maintenance mode config " + err.Error())
				return nil
			}
			maintenanceFlag := config.GetConfig().GetLoadBalancer().GetV1().GetSys().GetService().GetMaintenanceMode().GetValue()
			if !maintenanceFlag {
				resp, err := h.Writer.Confirm(maintenanceModeMsg)
				if err != nil {
					h.Writer.Error(err.Error())
					return status.Errorf(status.InvalidCommandArgsError, err.Error())
				}
				if !resp {
					h.Writer.Error(downTimeError)
					return status.New(status.InvalidCommandArgsError, downTimeError)
				}

				out, err := exec.Command("/bin/sh", "-c", enable_maintenance_mode_cmd).Output()
				if !strings.Contains(string(out), "Updating deployment configuration") || err != nil {
					h.Writer.Errorln("error in enabling the maintenance mode")
					return nil
				}

			}
			return nil
		},
	}
}

func externalESUpgradeCheck() Checklist {
	return Checklist{
		Name:        "external_es_upgrade_acceptance",
		Description: "confirmation check for external es upgrade",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm(externalESUpgradeMsg)
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(externalESUpgradeError)
				return status.New(status.InvalidCommandArgsError, externalESUpgradeError)
			}
			return nil
		},
	}
}

/*
const disableShardCurlRequest = `curl --location --request PUT 'http://localhost:10141/_cluster/settings'
-u 'admin:admin' --header 'Content-Type: application/json'
--data-raw '{ "persistent": { "cluster.routing.allocation.enable": "primaries"}}'`

const flushIndexRequest = `curl --location --request PUT 'http://localhost:10168/_cluster/settings' \
-u 'admin:admin' \
--header 'Content-Type: application/json' \
--data-raw '{
  "persistent": {
    "cluster.routing.allocation.enable": "all"
  }}'`

*/
/*
disableShardAllocation : Disable shard allocation: during upgrade
we do not want any shard movement as the cluster will be
taken down.
*/
func disableShardAllocation() error {
	url := "http://localhost:10141/_cluster/settings"
	method := "PUT"

	payload := strings.NewReader(`{
    "persistent": {
        "cluster.routing.allocation.enable": "primaries"
    }
}`)

	client := &http.Client{}
	req, err := http.NewRequest(method, url, payload) // nosemgrep

	if err != nil {
		return err
	}
	req.Header.Add("Content-Type", "application/json")

	res, err := client.Do(req)
	if err != nil {
		fmt.Println(err)
		return err
	}
	defer res.Body.Close()

	_, err = ioutil.ReadAll(res.Body) // nosemgrep
	if err != nil {
		return err
	}
	return nil
}

/*
flushRequest : Stop indexing, and perform a flush:
as the cluster will be taken down, indexing/searching
should be stopped and _flush can be used to permanently
store information into the index which will prevent any
data loss during upgrade
*/
func flushRequest() error {

	url := "http://localhost:10141/_flush"
	method := "POST"

	client := &http.Client{}
	req, err := http.NewRequest(method, url, nil) // nosemgrep

	if err != nil {
		fmt.Println(err)
		return err
	}
	req.Header.Add("Content-Type", "application/json")

	res, err := client.Do(req)
	if err != nil {
		return err
	}
	defer res.Body.Close()

	_, err = ioutil.ReadAll(res.Body) // nosemgrep
	if err != nil {
		fmt.Println(err)
		return err
	}
	return nil
}

/*
reEnableShardAllocation: Re-enable shard allocation
In case of Any error we can Undo the First request
*/
func reEnableShardAllocation() error {
	url := "http://localhost:10141/_cluster/settings"
	method := "PUT"

	payload := strings.NewReader(`{
		"persistent": {
			"cluster.routing.allocation.enable": null
		  }
}`)

	client := &http.Client{}
	req, err := http.NewRequest(method, url, payload) // nosemgrep

	if err != nil {
		return err
	}
	req.Header.Add("Content-Type", "application/json")

	res, err := client.Do(req)
	if err != nil {
		fmt.Println(err)
		return err
	}
	defer res.Body.Close()

	_, err = ioutil.ReadAll(res.Body) // nosemgrep
	if err != nil {
		return err
	}
	return nil
}

func disableSharding() Checklist {
	return Checklist{
		Name:        "disable sharding",
		Description: "confirmation check to disable sharding",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("This will disable Sharding on your elastic search")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(shardingError)
				return status.New(status.InvalidCommandArgsError, shardingError)
			}
			h.Writer.Println("Calling elasticsearch api to disable shard allocation...")
			err = disableShardAllocation()
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.DatabaseError, err.Error())
			}

			err = flushRequest()
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.DatabaseError, err.Error())
			}
			h.Writer.Println("Finished disabling shard allocation successfully.")
			return nil
		},
	}
}

func getHabRootPath(habrootcmd string) string {
	out, err := exec.Command("/bin/sh", "-c", habrootcmd).Output()
	if err != nil {
		return "/hab/"
	}
	pkgPath := string(out) // /a/b/c/hab    /hab/svc
	habIndex := strings.Index(string(pkgPath), "hab")
	rootHab := pkgPath[0 : habIndex+4] // this will give <>/<>/hab/
	if rootHab == "" {
		rootHab = "/hab/"
	}
	return rootHab
}

func replaceAndPatchS3backupUrl(h ChecklistHelper) error {
	res, err := client.GetAutomateConfig(int64(client.DefaultClientTimeout))
	if err != nil {
		h.Writer.Errorln("failed to get backup s3 url configuration: " + err.Error())
		return nil
	}
	endpoint := res.Config.GetGlobal().GetV1().GetBackups().GetS3().GetBucket().GetEndpoint().GetValue()
	re := regexp.MustCompile(s3regex)

	if re.MatchString(endpoint) {

		file, err := ioutil.TempFile("", filename) // nosemgrep
		if err != nil {
			h.Writer.Errorln("could not create temp file" + err.Error())
			return nil
		}
		defer os.Remove(file.Name())
		if _, err := file.Write([]byte(s3EndpointConf)); err != nil {
			h.Writer.Errorln("could not write toml file" + err.Error())
			return nil
		}
		out, err := exec.Command("/bin/sh", "-c", fmt.Sprintf(automatePatchCmd, file.Name())).Output()
		if !strings.Contains(string(out), "Configuration patched") || err != nil {
			h.Writer.Errorln("error in running automate patch command")
			return nil
		}
	}

	return nil
}

func replaceS3Url() Checklist {
	return Checklist{
		Name:        "Change s3 url",
		Description: "Changes backup s3 url during upgrade.",
		TestFunc:    replaceAndPatchS3backupUrl,
	}
}

func getESBasePath(timeout int64) string {
	var basePath = "http://localhost:10144/"
	cfg := dc.DefaultAutomateConfig()
	defaultHost := cfg.GetEsgateway().GetV1().GetSys().GetService().GetHost().GetValue()
	defaultPort := cfg.GetEsgateway().GetV1().GetSys().GetService().GetPort().GetValue()

	if defaultHost != "" || defaultPort > 0 {
		basePath = fmt.Sprintf(`http://%s/`, net.JoinHostPort(defaultHost, fmt.Sprintf("%d", defaultPort)))
	}

	res, err := client.GetAutomateConfig(timeout)
	if err == nil {
		host := res.Config.GetEsgateway().GetV1().GetSys().GetService().GetHost().GetValue()
		port := res.Config.GetEsgateway().GetV1().GetSys().GetService().GetPort().GetValue()
		if host != "" || port > 0 {
			url := net.JoinHostPort(host, fmt.Sprintf("%d", port))
			if url != "" {
				basePath = fmt.Sprintf(`http://%s/`, url)
			}
		}
	}
	return basePath
}

func getAllIndices(basePath string) ([]byte, error) {
	return execRequest(basePath+"_cat/indices?h=index", "GET", nil)
}

func fetchOldIndexInfo(basePath string) ([]indexData, error) {
	allIndexList, err := getAllIndices(basePath)
	if err != nil {
		return nil, errors.Wrap(err, "error while getting list of indices.")
	}
	indexList := strings.Split(strings.TrimSuffix(string(allIndexList), "\n"), "\n")
	additionalBatch := 0
	if len(indexList)%indexBatchSize > 0 {
		additionalBatch = 1
	}
	numOfBatches := len(indexList)/indexBatchSize + additionalBatch
	var indexDataArr []indexData
	for i := 0; i < numOfBatches; i++ {
		upper := i*indexBatchSize + indexBatchSize
		if upper > len(indexList)-1 {
			upper = len(indexList)
		}
		indexCSL := strings.Join(indexList[i*indexBatchSize:upper], ",")
		versionData, err := execRequest(basePath+indexCSL+"/_settings/index.version.created*?&human", "GET", nil)
		if err != nil {
			return nil, errors.Wrap(err, "error while getting indices details.")
		}
		data, err := getDataForOldIndices(versionData)
		if err != nil {
			return nil, errors.Wrap(err, "error while parsing indices details.")
		}
		indexDataArr = append(indexDataArr, data...)
	}
	return indexDataArr, nil
}

func doDeleteStaleIndices(timeout int64, h ChecklistHelper) error {
	basePath := getESBasePath(timeout)
	indexInfo, err := fetchOldIndexInfo(basePath)
	if err != nil {
		return err
	}
	for i, index := range indexInfo {
		h.Writer.Println(fmt.Sprintf("Automate is unable to upgrade because an index with name: %s is created using an older version of elasticsearch %s", index.Name, index.CreatedString))
		resp, err := h.Writer.Confirm("Do you wish to delete the index to continue?")
		if err != nil {
			h.Writer.Error(err.Error())
			return status.Errorf(status.InvalidCommandArgsError, err.Error())
		}
		errMsg := formErrorMsg(indexInfo)
		if !resp {
			return status.Errorf(status.UnknownError, fmt.Sprintf(oldIndexError, index.Name, index.CreatedString, msg, errMsg))
		}

		_, err = execRequest(fmt.Sprintf("%s%s?pretty", basePath, index.Name), "DELETE", nil)
		if err != nil {
			h.Writer.Error(err.Error())
			return status.Errorf(status.UnknownError, fmt.Sprintf(oldIndexError, index.Name, index.CreatedString, msg, errMsg))
		}
		indexInfo[i].IsDeleted = true //mark the deleted indices, so that those wont show in the list to end user
	}
	return nil
}

func formErrorMsg(IndexDetailsArray []indexData) error {
	msg := "\nUnsupported index versions. To continue with the upgrade, please reindex the indices shown below to version 6.\n"
	for _, index := range IndexDetailsArray {
		if !index.IsDeleted {
			msg += fmt.Sprintf("- Index Name: %s, Version: %s \n", index.Name, index.CreatedString)
		}
	}
	msg += "\nFollow the guide below to learn more about reindexing:\nhttps://www.elastic.co/guide/en/elasticsearch/reference/6.8/docs-reindex.html"
	return fmt.Errorf(msg)
}

func getDataForOldIndices(allIndexData []byte) ([]indexData, error) {
	var indexDataArray []indexData
	var parsed map[string]IndexInfo
	err := json.Unmarshal(allIndexData, &parsed)
	if err != nil {
		return nil, errors.Wrap(err, "error in unmarshalling the index data")
	}
	for key, data := range parsed {
		index, err := strconv.ParseInt(data.Settings.Index.Version.CreatedString[0:1], 10, 64)
		if err != nil {
			return nil, errors.Wrap(err, "failed to parse index version")
		}
		if index < 6 && key != ".watches" {
			indexDataArray = append(indexDataArray,
				indexData{Name: key, MajorVersion: index,
					CreatedString: data.Settings.Index.Version.CreatedString})
		}
	}
	return indexDataArray, nil
}

func deleteStaleIndices(timeout int64) Checklist {
	return Checklist{
		Name:        "check index version",
		Description: "confirmation check to delete stale indices",
		TestFunc: func(h ChecklistHelper) error {
			err := doDeleteStaleIndices(timeout, h)
			if err != nil {
				return err
			}
			return nil
		},
	}
}

func storeSearchEngineSettings() Checklist {
	return Checklist{
		Name:        "post_checklist_intimation_acceptance",
		Description: "confirmation check for post checklist intimation",
		TestFunc: func(h ChecklistHelper) error {
			isEmbeded := !IsExternalElasticSearch()
			if !isEmbeded {
				h.Writer.Warnf("Automate is running on external elastic search, not taking configuration backup")
				return nil
			} else {
				esSettings, _ := GetESSettings(h.Writer)
				esHeapSize, _ := extractNumericFromText(esSettings.HeapMemory, 0)
				fiftyPercentOfMemory := defaultHeapSizeInGB()
				isOkSettings := true
				inAppropriateSettings := []string{}
				if esHeapSize > MAX_POSSIBLE_HEAP_SIZE || int(math.Round(esHeapSize)) > fiftyPercentOfMemory {
					msg := fmt.Sprintf(heapSizeExceededError, esSettings.HeapMemory, fiftyPercentOfMemory, MAX_POSSIBLE_HEAP_SIZE)
					inAppropriateSettings = append(inAppropriateSettings, msg)
					isOkSettings = false
				}
				requiredShards := (esSettings.TotalShardSettings + INDICES_TOTAL_SHARD_INCREMENT_DEFAULT)
				if requiredShards > INDICES_TOTAL_SHARD_DEFAULT {
					msg := fmt.Sprintf(shardCountExceededError, requiredShards, INDICES_TOTAL_SHARD_DEFAULT, requiredShards)
					inAppropriateSettings = append(inAppropriateSettings, msg)
					isOkSettings = false
				}
				if !isOkSettings {
					for _, errMsg := range inAppropriateSettings {
						h.Writer.Warn(errMsg)
					}
					resp, err := h.Writer.Confirm(errorUserConcent)
					if err != nil {
						h.Writer.Error(err.Error())
						return status.Errorf(status.InappropriateSettingError, err.Error())
					}
					if !resp {
						return status.New(status.InappropriateSettingError, upgradeFailed)
					}
				}
				storeESSettings(h.Writer, esSettings)
				return nil
			}
		},
	}
}

func deleteA1Indexes(timeout int64) Checklist {
	basePath := getESBasePath(timeout)
	indexInfo, err := fetchOldIndexInfo(basePath)
	if err != nil {
		return Checklist{
			Name:        "delete A1 indexes",
			Description: "confirmation check to delete A1 indexes",
			TestFunc: func(h ChecklistHelper) error {
				return fmt.Errorf("Error fetching indices: %w", err)
			},
		}
	}

	existingIndexes := findMatch(sourceList, indexInfo)

	if len(existingIndexes) == 0 {
		return Checklist{
			Name:        "delete A1 indexes",
			Description: "confirmation check index version",
			TestFunc:    nil,
		}
	}

	return Checklist{
		Name:        "delete A1 indexes",
		Description: "confirmation check to delete A1 indexes",
		TestFunc: func(h ChecklistHelper) error {
			indexes := strings.Join(existingIndexes, ",")
			resp, err := h.Writer.Confirm(fmt.Sprintf("Following Indexes are of Automate 1 and are no longer use in automate, thus we will delete these indices:%s", indexes))
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				return status.New(status.InvalidCommandArgsError, "The Automate 1 stale indices needs to be deleted before upgrading.")
			}
			basePath := getESBasePath(timeout)
			err = batchDeleteIndexFromA1(timeout, existingIndexes, basePath)
			if err != nil {
				return err
			}
			return nil
		},
	}
}

func batchDeleteIndexFromA1(timeout int64, indexList []string, basePath string) error {
	additionalBatch := 0
	if len(indexList)%indexBatchSize > 0 {
		additionalBatch = 1
	}
	numOfBatches := len(indexList)/indexBatchSize + additionalBatch
	for i := 0; i < numOfBatches; i++ {
		upper := i*indexBatchSize + indexBatchSize
		if upper > len(indexList)-1 {
			upper = len(indexList)
		}
		indexCSL := strings.Join(indexList[i*indexBatchSize:upper], ",")
		_, err := execRequest(fmt.Sprintf("%s%s", basePath, indexCSL)+"?pretty", "DELETE", nil)
		if err != nil {
			return err
		}
	}
	return nil
}

// findMatch returns the list of items available in targetList from sourceList
func findMatch(sourceList []string, indexData []indexData) []string {
	matchedList := make(map[string]interface{})
	for _, item := range indexData {
		for _, sourceItem := range sourceList {
			if strings.Contains(item.Name, sourceItem) {
				matchedList[item.Name] = ""
				break
			}
		}
	}
	list := []string{}
	for key := range matchedList {
		list = append(list, key)
	}
	return list
}
