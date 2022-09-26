package migratorV4

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/chef/automate/api/config/deployment"
	opensearch "github.com/chef/automate/api/config/opensearch"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector/upgradeinspectorv4"
	"github.com/chef/automate/lib/platform/sys"
	"google.golang.org/protobuf/types/known/wrapperspb"
)

const (
	MAX_OPEN_FILE_DEFAULT  = "65535"
	MAX_LOCKED_MEM_DEFAULT = "unlimited"
	MAX_OPEN_FILE_KEY      = "Max open files"
	MAX_LOCKED_MEM_KEY     = "Max locked memory"

	INDICES_TOTAL_SHARD_DEFAULT         = 2000
	INDICES_BREAKER_TOTAL_LIMIT_DEFAULT = "95"

	INDICES_TOTAL_SHARD_INCREMENT_DEFAULT = 500

	MAX_POSSIBLE_HEAP_SIZE = 32

	V3ESSettingFile               = "/hab/svc/deployment-service/old_es_v3_settings.json"
	AutomateOpensearchConfigPatch = "/hab/svc/deployment-service/oss-config.toml"

	heapSizeExceededError   = `heap size : %s, max allowed is (50%% of ram = %dgb) but not exceeding %dgb`
	shardCountExceededError = `total shards per node : %d, max allowed is %d, 
having this more than %d decreases perfomance to avoid breaching this limit, 
you can reduce data retention policy`
	errorUserConcent = `we recommend you to move to external/managed Opensearch cluster for better performance.
but if you still want to continue with the upgrade`

	upgradeFailed = "due to pre-condition check failed"
	ELASTICSEARCH_DATA_DIR      = "/hab/svc/automate-elasticsearch/data"
	ELASTICSEARCH_VAR_DIR       = "/hab/svc/automate-elasticsearch/var"


)

type MigratorV4Utils interface {
	CreateMigrationMetadata() error
	ReadMigrationMetadata() error
	UpdateMigrationMetadata() error
	PatchOpensearchSettings() error
	GetDefaultOpensearchSettings() *ESSettings
	GetEsTotalShardSettings() (int32, error)
	PatchOpensearchConfig(*ESSettings) (string, string, error)
}

type ESSettings struct {
	TotalShardSettings       int32  `json:"totalShardSettings" toml:"totalShardSettings"`
	IndicesBreakerTotalLimit string `json:"indicesBreakerTotalLimit" toml:"indicesBreakerTotalLimit"`
	RuntimeMaxOpenFile       string `json:"runtimeMaxOpenFile" toml:"runtimeMaxOpenFile"`
	RuntimeMaxLockedMem      string `json:"runtimeMaxLockedMem" toml:"runtimeMaxLockedMem"`
	HeapMemory               string `json:"heapMemory" toml:"heapMemory"`
}

type MigratorV4UtilsImpl struct {
}

func NewMigratorV4Utils() MigratorV4Utils {
	return &MigratorV4UtilsImpl{}
}

func (m *MigratorV4UtilsImpl) CreateMigrationMetadata() error {
	return nil
}

func (m *MigratorV4UtilsImpl) ReadMigrationMetadata() error {
	return nil
}

func (m *MigratorV4UtilsImpl) UpdateMigrationMetadata() error {
	return nil
}

func (m *MigratorV4UtilsImpl) PatchOpensearchSettings() error {
	opensearchSettings := m.GetDefaultOpensearchSettings()
	esTotalShards, err := m.GetEsTotalShardSettings()
	if err != nil {
		return err
	}
	opensearchSettings.TotalShardSettings = m.GetOverrideTotalShards(esTotalShards, opensearchSettings.TotalShardSettings)
	_, _, err = m.PatchOpensearchConfig(opensearchSettings)
	if err != nil {
		return err
	}
	return nil
}

func (m *MigratorV4UtilsImpl) PatchOpensearchConfig(osConfig *ESSettings) (string, string, error) {
	cfg := deployment.NewUserOverrideConfig()
	cfg.Opensearch = &opensearch.ConfigRequest{
		V1: &opensearch.ConfigRequest_V1{
			Sys: &opensearch.ConfigRequest_V1_System{
				Cluster: &opensearch.ConfigRequest_V1_Cluster{
					MaxShardsPerNode: wrapperspb.Int32(osConfig.TotalShardSettings),
				},
				Indices: &opensearch.ConfigRequest_V1_Indices{
					Breaker: &opensearch.ConfigRequest_V1_Breaker{
						TotalLimit: wrapperspb.String(osConfig.IndicesBreakerTotalLimit),
					},
				},
				Runtime: &opensearch.ConfigRequest_V1_Runtime{
					MaxOpenFiles:    wrapperspb.String(osConfig.RuntimeMaxOpenFile),
					MaxLockedMemory: wrapperspb.String(osConfig.RuntimeMaxLockedMem),
					Heapsize:        wrapperspb.String(osConfig.HeapMemory),
				},
			},
		},
	}
	tw := upgradeinspectorv4.NewTestWriter()
	err := client.PatchAutomateConfig(10, cfg, tw.CliWriter)
	if err != nil {
		return "", "", err
	}
	return tw.WriteBuffer.String(), tw.ErrorBuffer.String(), nil
}

func (m *MigratorV4UtilsImpl) GetDefaultOpensearchSettings() *ESSettings {
	defaultSettings := &ESSettings{}
	defaultSettings.HeapMemory = fmt.Sprintf("%d", defaultHeapSizeInGB())
	defaultSettings.IndicesBreakerTotalLimit = INDICES_BREAKER_TOTAL_LIMIT_DEFAULT
	defaultSettings.RuntimeMaxLockedMem = MAX_LOCKED_MEM_DEFAULT
	defaultSettings.RuntimeMaxOpenFile = MAX_OPEN_FILE_DEFAULT
	defaultSettings.TotalShardSettings = INDICES_TOTAL_SHARD_DEFAULT
	return defaultSettings
}

func (m *MigratorV4UtilsImpl) GetEsTotalShardSettings() (int32, error) {
	esSetting := &ESSettings{}
	jsonData, err := ioutil.ReadFile(V3ESSettingFile) // nosemgrep
	if err != nil {
		return -1, err
	}
	err = json.Unmarshal(jsonData, esSetting)
	if err != nil {
		return -1, err
	}
	return esSetting.TotalShardSettings, nil
}

func (m *MigratorV4UtilsImpl) GetOverrideTotalShards(esShardSetting, osShardSetting int32) int32 {
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

func preRequisteForESDataMigration() (bool, error) {
	existDir, err := dirExists(ELASTICSEARCH_DATA_DIR)
	if err != nil {
		return existDir, err
	}
	existDir, err = dirExists(ELASTICSEARCH_VAR_DIR)
	if err != nil {
		return existDir, err
	}
	return existDir, nil
}

func dirExists(path string) (bool, error) {
	_, err := os.Stat(path)
	if err == nil {
		return true, nil
	}
	if os.IsNotExist(err) { // nosemgrep
		return false, nil
	}
	return false, err
}
