package upgradeinspectorv4

import (
	"strings"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
	"github.com/fatih/color"
)

const (
	ES_URL = "http://localhost:10141/_cluster/settings"
)

type DisableShardingInspection struct {
	writer          *cli.Writer
	upgradeUtils    UpgradeV4Utils
	isExecuted      bool
	exitError       error
	exitedWithError bool
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
	disableShardingPayload := strings.NewReader(`{
		"persistent": {
			"cluster.routing.allocation.enable": "primaries"
		}
	}`)
	_, err = ds.upgradeUtils.ExecRequest(ES_URL, "PUT", disableShardingPayload)
	if err != nil {
		ds.setExitError(err)
		return err
	}
	ds.setExecuted()
	return nil
}

func (ds *DisableShardingInspection) setExitError(err error) {
	ds.exitError = err
	ds.exitedWithError = true
}

func (ds *DisableShardingInspection) setExecuted() {
	ds.isExecuted = true
}

func (ds *DisableShardingInspection) RollBackHandler() (err error) {
	if !ds.isExecuted {
		return nil
	}
	enableShardingPayload := strings.NewReader(`{
		"persistent": {
			"cluster.routing.allocation.enable": null
		}
	}`)
	_, err = ds.upgradeUtils.ExecRequest(ES_URL, "PUT", enableShardingPayload)
	return
}

func (ds *DisableShardingInspection) GetInstallationType() inspector.InstallationType {
	return inspector.EMBEDDED
}

func (ds *DisableShardingInspection) ExitHandler() error {
	if ds.exitedWithError {
		ds.writer.Println("[" + color.New(color.FgRed).Sprint("Error") + "] " + ds.exitError.Error())
		ds.writer.Println(UPGRADE_TERMINATED)
	}
	return nil
}
