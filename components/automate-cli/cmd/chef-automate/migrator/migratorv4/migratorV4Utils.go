package migratorV4

import (
	"context"
	"encoding/json"
	"io/ioutil"
	"os"
	"os/exec"
	"strings"
	"syscall"

	"github.com/chef/automate/api/config/deployment"
	opensearch "github.com/chef/automate/api/config/opensearch"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector/upgradeinspectorv4"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"google.golang.org/protobuf/types/known/wrapperspb"
)

const (
	MAX_OPEN_FILE_DEFAULT                 = "65535"
	MAX_LOCKED_MEM_DEFAULT                = "unlimited"
	MAX_OPEN_FILE_KEY                     = "Max open files"
	MAX_LOCKED_MEM_KEY                    = "Max locked memory"
	INDICES_BREAKER_TOTAL_LIMIT_DEFAULT   = "95"
	INDICES_TOTAL_SHARD_INCREMENT_DEFAULT = 500
	MAX_POSSIBLE_HEAP_SIZE                = 32
	AutomateOpensearchConfigPatch         = "/hab/svc/deployment-service/oss-config.toml"
	heapSizeExceededError                 = `heap size : %s, max allowed is (50%% of ram = %dgb) but not exceeding %dgb`
	shardCountExceededError               = `total shards per node : %d, max allowed is %d, 
having this more than %d decreases perfomance to avoid breaching this limit, 
you can reduce data retention policy`
	errorUserConcent = `we recommend you to move to external/managed Opensearch cluster for better performance.
but if you still want to continue with the upgrade`
	upgradeFailed          = "due to pre-condition check failed"
	ELASTICSEARCH_DATA_DIR = "/hab/svc/automate-elasticsearch/data"
	ELASTICSEARCH_VAR_DIR  = "/hab/svc/automate-elasticsearch/var"
)

type MigratorV4Utils interface {
	CreateMigrationMetadata() error
	ReadMigrationMetadata() error
	UpdateMigrationMetadata() error
	GetEsTotalShardSettings() (int32, error)
	PatchOpensearchConfig(*ESSettings) (string, string, error)
	IsExternalElasticSearch(timeout int64) bool
	StopAutomate() error
	StartAutomate() error
	GetHabRootPath(habrootcmd string) string
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

func (m *MigratorV4UtilsImpl) GetEsTotalShardSettings() (int32, error) {
	esSetting := &ESSettings{}
	jsonData, err := ioutil.ReadFile(majorupgrade_utils.V3_ES_SETTING_FILE) // nosemgrep
	if err != nil {
		return -1, err
	}
	err = json.Unmarshal(jsonData, esSetting)
	if err != nil {
		return -1, err
	}
	return esSetting.TotalShardSettings, nil
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

func (m *MigratorV4UtilsImpl) IsExternalElasticSearch(timeout int64) bool {
	return majorupgrade_utils.IsExternalElasticSearch(timeout)
}

func isDevMode() bool {
	return os.Getenv("CHEF_DEV_ENVIRONMENT") == "true"
}

func (m *MigratorV4UtilsImpl) StopAutomate() error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}
	if isDevMode() {
		_, err = connection.Stop(context.Background(), &api.StopRequest{})
		if err != nil {
			return err
		}
	} else {
		t := target.NewLocalTarget(true)
		if err := t.EnsureStopped(); err != nil {
			return err
		}
	}
	return nil
}

func (m *MigratorV4UtilsImpl) StartAutomate() error {
	if isDevMode() {
		if err := exec.Command("hab", "sup", "status").Run(); err == nil {
			return nil
		}

		if err := os.MkdirAll("/hab/sup/default", 0755); err != nil {
			return err
		}
		out, err := os.Create("/hab/sup/default/sup.log")
		if err != nil {
			return err
		}
		startSupCmd := exec.Command("hab", "sup", "run")
		startSupCmd.Env = os.Environ()
		startSupCmd.Env = append(startSupCmd.Env,
			"DS_DEV=true",
			"CHEF_AUTOMATE_SKIP_SYSTEMD=true",
			"HAB_LICENSE=accept-no-persist",
		)
		startSupCmd.Stdout = out
		startSupCmd.Stderr = out
		startSupCmd.SysProcAttr = &syscall.SysProcAttr{
			Setpgid: true,
		}
		if err := startSupCmd.Start(); err != nil {
			return err
		}
	} else {
		systemctlCmd := exec.Command("systemctl", "start", "chef-automate.service")
		systemctlCmd.Stdout = os.Stdout
		systemctlCmd.Stderr = os.Stderr
		if err := systemctlCmd.Run(); err != nil {
			return err
		}
	}

	return nil
}

func (m *MigratorV4UtilsImpl) GetHabRootPath(habrootcmd string) string {
	out, err := exec.Command("/bin/sh", "-c", habrootcmd).Output()
	if err != nil {
		return "/hab/"
	}
	pkgPath := string(out)
	habIndex := strings.Index(string(pkgPath), "hab")
	rootHab := pkgPath[0 : habIndex+4]
	if rootHab == "" {
		rootHab = "/hab/"
	}
	return rootHab
}
