package upgradeinspectorv4

import (
	"strings"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
)

type DisableShardingInspection struct {
	writer       *cli.Writer
	upgradeUtils UpgradeV4Utils
}

func NewDisableShardingInspection(w *cli.Writer, utls UpgradeV4Utils) *DisableShardingInspection {
	return &DisableShardingInspection{
		writer:       w,
		upgradeUtils: utls,
	}
}

func (ds *DisableShardingInspection) ShowInfo(index *int) error {
	return nil
}
func (ds *DisableShardingInspection) Skip() {
	return
}
func (ds *DisableShardingInspection) GetShortInfo() []string {
	return nil
}

func (ds *DisableShardingInspection) Inspect() (err error) {
	url := "http://localhost:10141/_cluster/settings"
	payload := strings.NewReader(`{
		"persistent": {
			"cluster.routing.allocation.enable": "primaries"
		}
	}`)
	_, err = ds.upgradeUtils.ExecRequest(url, "PUT", payload)
	return
}

func (ds *DisableShardingInspection) PreExit() (err error) {
	url := "http://localhost:10141/_cluster/settings"
	payload := strings.NewReader(`{
		"persistent": {
			"cluster.routing.allocation.enable": null
		}
	}`)
	_, err = ds.upgradeUtils.ExecRequest(url, "PUT", payload)
	return
}

func (ds *DisableShardingInspection) GetInstallationType() inspector.InstallationType {
	return inspector.EMBEDDED
}
