package migratorV4

import (
	"fmt"

	opensearch "github.com/chef/automate/api/config/opensearch"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/chef/automate/lib/platform/sys"
)

type PatchOpensearchConfig struct {
	writer *cli.Writer
	utils  MigratorV4Utils
}

func NewPatchOpensearchConfig(w *cli.Writer, utils MigratorV4Utils) *PatchOpensearchConfig {
	return &PatchOpensearchConfig{
		writer: w,
		utils:  utils,
	}
}

func (poc *PatchOpensearchConfig) Run() error {
	opensearchSettings := poc.GetDefaultOpensearchSettings()
	esTotalShards, err := poc.utils.GetEsTotalShardSettings()
	if err != nil {
		return err
	}
	opensearchSettings.TotalShardSettings = poc.GetOverrideTotalShards(esTotalShards, opensearchSettings.TotalShardSettings)
	_, _, err = poc.utils.PatchOpensearchConfig(opensearchSettings)
	if err != nil {
		return err
	}
	return nil
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
	defaultSettings.HeapMemory = fmt.Sprintf("%d", defaultHeapSizeInGB())
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
	return
}
