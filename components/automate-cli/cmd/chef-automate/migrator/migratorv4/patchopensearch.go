package migratorV4

import (
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	opensearch "github.com/chef/automate/api/config/opensearch"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/chef/automate/lib/platform/sys"
	"github.com/fatih/color"
)

type PatchOpensearchConfig struct {
	writer     *cli.Writer
	utils      MigratorV4Utils
	spinner    *spinner.Spinner
	runError   error
	hasError   bool
	isExecuted bool
}

func NewPatchOpensearchConfig(w *cli.Writer, utils MigratorV4Utils) *PatchOpensearchConfig {
	return &PatchOpensearchConfig{
		writer: w,
		utils:  utils,
	}
}

func (poc *PatchOpensearchConfig) Run() error {
	poc.showUpdating()
	opensearchSettings := poc.GetDefaultOpensearchSettings()

	esTotalShards, _ := poc.utils.GetEsTotalShardSettings()
	opensearchSettings.TotalShardSettings = poc.GetOverrideTotalShards(esTotalShards, opensearchSettings.TotalShardSettings)
	_, _, err := poc.utils.PatchOpensearchConfig(opensearchSettings)
	if err != nil {
		poc.showUpdateError()
		poc.setError(err)
		return err
	}
	poc.showUpdated()
	return nil
}

func (poc *PatchOpensearchConfig) setError(err error) error {
	poc.runError = err
	poc.hasError = true
	return err
}

func (poc *PatchOpensearchConfig) GetOverrideTotalShards(esShardSetting, osShardSetting int32) int32 {
	if esShardSetting > osShardSetting {
		return esShardSetting
	} else {
		return osShardSetting
	}
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

func (poc *PatchOpensearchConfig) Skip() error {
	return nil
}

func (poc *PatchOpensearchConfig) ErrorHandler() {
	if poc.hasError {
		poc.writer.Println(poc.runError.Error())
	}
}

func (poc *PatchOpensearchConfig) showUpdateError() {
	poc.spinner.FinalMSG = " "+color.New(color.FgRed).Sprint("✖") + "  Failed to update OpenSearch configurations"
	poc.spinner.Stop()
	poc.writer.Println("")
}

func (poc *PatchOpensearchConfig) showUpdated() {
	poc.spinner.FinalMSG = " "+color.New(color.FgGreen).Sprint("✔") + "  OpenSearch configurations updated successfully"
	poc.spinner.Stop()
	poc.writer.Println("")
}

func (poc *PatchOpensearchConfig) showUpdating() {
	poc.spinner = poc.writer.NewSpinner()
	poc.spinner.Suffix = fmt.Sprintf("  Updating OpenSearch configurations")
	poc.spinner.Start()
	time.Sleep(time.Second)
}
