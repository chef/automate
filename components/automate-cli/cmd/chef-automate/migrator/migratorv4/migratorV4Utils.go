package migratorv4

import (
	"context"
	"encoding/json"
	"io/ioutil"
	"os"
	"os/exec"
	"syscall"
	"time"

	"github.com/chef/automate/api/config/deployment"
	opensearch "github.com/chef/automate/api/config/opensearch"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/majorupgradechecklist"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/pkg/errors"
	"google.golang.org/protobuf/types/known/wrapperspb"
)

const (
	SPACES_BEFORE_STEPS          = "       "
	AUTOMATE_VERSION_4    string = "4"
	SPINNER_TEST_DURATION        = 100 * time.Millisecond
)

type MigratorV4Utils interface {
	GetEsTotalShardSettings(path string) (int32, error)
	PatchOpensearchConfig(es *ESSettings) (string, string, error)
	IsExternalElasticSearch(timeout int64) bool
	StopAutomate() error
	StartAutomate() error
	ReadV4Checklist(id, path string) (bool, error)
	UpdatePostChecklistFile(id, path string) error
	ExecuteCommand(command string, args []string, workingDir string) error
	GetServicesStatus() (bool, error)
	GetAutomateFQDN(timeout int64) string
	GetMaintenanceStatus(timeout int64) (bool, error)
	SetMaintenanceMode(timeout int64, status bool) (stdOut, stdErr string, err error)
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
	tw := majorupgrade_utils.NewCustomWriter()
	err := client.PatchAutomateConfig(10, cfg, tw.CliWriter)
	if err != nil {
		return "", "", errors.Wrap(err, "Failed to patch opensearch config")
	}
	return tw.WriteBuffer.String(), tw.ErrorBuffer.String(), nil
}

func (m *MigratorV4UtilsImpl) GetEsTotalShardSettings(path string) (int32, error) {
	esSetting := &ESSettings{}
	jsonData, err := ioutil.ReadFile(path) // nosemgrep
	if err != nil {
		return esSetting.TotalShardSettings, err
	}
	err = json.Unmarshal(jsonData, esSetting)
	if err != nil {
		return esSetting.TotalShardSettings, err
	}
	return esSetting.TotalShardSettings, nil
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
		return errors.Wrap(err, "Error in connection while stopping automate")
	}
	if isDevMode() {
		_, err = connection.Stop(context.Background(), &api.StopRequest{})
		if err != nil {
			return errors.Wrap(err, "Error stopping automate in dev mode")
		}
	} else {
		t := target.NewLocalTarget(true)
		if err := t.EnsureStopped(); err != nil {
			return errors.Wrap(err, "Error stopping automate")
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
			return errors.Wrap(err, "Failed trying to start automate dev mode")
		}
		out, err := os.Create("/hab/sup/default/sup.log")
		if err != nil {
			return errors.Wrap(err, "Failed trying to start automatedev mode")
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
			return errors.Wrap(err, "Failed trying to start automate dev mode")
		}
	} else {
		systemctlCmd := exec.Command("systemctl", "start", "chef-automate.service")
		systemctlCmd.Stdout = os.Stdout
		systemctlCmd.Stderr = os.Stderr
		if err := systemctlCmd.Run(); err != nil {
			return errors.Wrap(err, "Failed trying to start automate")
		}
	}

	return nil
}

func (m *MigratorV4UtilsImpl) ReadV4Checklist(id, path string) (bool, error) {
	ci, err := majorupgradechecklist.NewPostChecklistManager(AUTOMATE_VERSION_4)
	if err != nil {
		return false, err
	}
	return ci.ReadPostChecklistById(id, path)
}

func (m *MigratorV4UtilsImpl) UpdatePostChecklistFile(id, path string) error {
	ci, err := majorupgradechecklist.NewPostChecklistManager(AUTOMATE_VERSION_4)
	if err != nil {
		return err
	}
	return ci.UpdatePostChecklistFile(id, path)
}

func (m *MigratorV4UtilsImpl) ExecuteCommand(command string, args []string, workingDir string) error {
	c := exec.Command(command, args...)
	if len(workingDir) > 0 {
		c.Dir = workingDir
	}
	err := c.Run()
	return err
}

func (m *MigratorV4UtilsImpl) GetServicesStatus() (bool, error) {
	return majorupgrade_utils.EnsureStatus()
}

func (m *MigratorV4UtilsImpl) GetAutomateFQDN(timeout int64) string {
	res, err := client.GetAutomateConfig(timeout)
	if err != nil {
		return "http://path.local.automate.instance.io"
	}
	return res.Config.GetGlobal().GetV1().GetFqdn().Value
}

func (m *MigratorV4UtilsImpl) GetMaintenanceStatus(timeout int64) (bool, error) {
	return majorupgrade_utils.GetMaintenanceStatus(timeout)
}

func (m *MigratorV4UtilsImpl) SetMaintenanceMode(timeout int64, status bool) (stdOut, stdErr string, err error) {
	return majorupgrade_utils.SetMaintenanceMode(timeout, status)
}
