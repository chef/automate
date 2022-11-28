package migratorv4

import (
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	opensearch "github.com/chef/automate/api/config/opensearch"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/chef/automate/lib/platform/sys"
	"github.com/fatih/color"
)

const (
	MINIMUM_SHARD_VALUE                   = 1500
	MAX_OPEN_FILE_DEFAULT                 = "65535"
	MAX_LOCKED_MEM_DEFAULT                = "unlimited"
	MAX_OPEN_FILE_KEY                     = "Max open files"
	MAX_LOCKED_MEM_KEY                    = "Max locked memory"
	INDICES_BREAKER_TOTAL_LIMIT_DEFAULT   = "95%"
	INDICES_TOTAL_SHARD_INCREMENT_DEFAULT = 1000
	MAX_POSSIBLE_HEAP_SIZE                = 32
	ELASTICSEARCH_DATA_DIR                = "/svc/automate-elasticsearch/data"
	ELASTICSEARCH_VAR_DIR                 = "/svc/automate-elasticsearch/var"
	OPENSEARCH_DATA_DIR                   = "/svc/automate-opensearch/data"
	MIGRATE_ES_ID                         = "migrate_es"
)

type PatchOpensearchConfig struct {
	writer         *cli.Writer
	utils          MigratorV4Utils
	fileutils      fileutils.FileUtils
	spinner        *spinner.Spinner
	runError       error
	hasError       bool
	isExecuted     bool
	spinnerTimeout time.Duration
}

func NewPatchOpensearchConfig(w *cli.Writer, utils MigratorV4Utils, fileutils fileutils.FileUtils, spinnerTimeout time.Duration) *PatchOpensearchConfig {
	return &PatchOpensearchConfig{
		writer:         w,
		utils:          utils,
		fileutils:      fileutils,
		spinnerTimeout: spinnerTimeout,
	}
}

func (poc *PatchOpensearchConfig) Run() error {
	poc.showUpdating()
	opensearchSettings := poc.GetDefaultOpensearchSettings()
	habroot := poc.fileutils.GetHabRootPath()
	esTotalShards, _ := poc.utils.GetEsTotalShardSettings(habroot + majorupgrade_utils.V3_ES_SETTING_FILE)
	opensearchSettings.TotalShardSettings = poc.calculateMaxTotalShards(esTotalShards, MINIMUM_SHARD_VALUE, INDICES_TOTAL_SHARD_INCREMENT_DEFAULT)
	_, _, err := poc.utils.PatchOpensearchConfig(opensearchSettings)
	if err != nil {
		poc.showUpdateError()
		poc.setError(err)
		return err
	}
	poc.showUpdated()
	return nil
}

func (poc *PatchOpensearchConfig) calculateMaxTotalShards(usedShards, minimumShardValue, incrementShardValue int32) (maxShardTotal int32) {
	if usedShards < minimumShardValue {
		maxShardTotal = majorupgrade_utils.INDICES_TOTAL_SHARD_DEFAULT
	} else {
		maxShardTotal = usedShards + incrementShardValue
	}
	return maxShardTotal
}

func (poc *PatchOpensearchConfig) setError(err error) error {
	poc.runError = err
	poc.hasError = true
	return err
}

func defaultHeapSizeInGB() int {
	sysMem, err := sys.SystemMemoryKB()
	if err != nil {
		sysMem = 0
	}
	return opensearch.RecommendedHeapSizeGB(sysMem)
}

func (poc *PatchOpensearchConfig) GetDefaultOpensearchSettings() *ESSettings {
	defaultSettings := &ESSettings{}
	defaultSettings.HeapMemory = fmt.Sprintf("%dg", defaultHeapSizeInGB())
	defaultSettings.IndicesBreakerTotalLimit = INDICES_BREAKER_TOTAL_LIMIT_DEFAULT
	defaultSettings.RuntimeMaxLockedMem = MAX_LOCKED_MEM_DEFAULT
	defaultSettings.RuntimeMaxOpenFile = MAX_OPEN_FILE_DEFAULT
	defaultSettings.TotalShardSettings = majorupgrade_utils.INDICES_TOTAL_SHARD_DEFAULT
	return defaultSettings
}

func (poc *PatchOpensearchConfig) ErrorHandler() {
	if poc.hasError {
		poc.writer.Println("[" + color.New(color.FgRed).Sprint("Error") + "] " + poc.runError.Error())
	}
}

func (poc *PatchOpensearchConfig) showUpdateError() {
	poc.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + color.New(color.FgRed).Sprint("✖") + "  Failed to update OpenSearch configurations"
	poc.spinner.Stop()
	poc.writer.Println("")
}

func (poc *PatchOpensearchConfig) showUpdated() {
	poc.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + color.New(color.FgGreen).Sprint("✔") + "  OpenSearch configurations updated successfully"
	poc.spinner.Stop()
	poc.writer.Println("")
}

func (poc *PatchOpensearchConfig) showUpdating() {
	poc.spinner = poc.writer.NewSpinnerWithTab()
	poc.spinner.Suffix = fmt.Sprintf("  Updating OpenSearch configurations")
	poc.spinner.Start()
	time.Sleep(poc.spinnerTimeout)
}
