package upgradeinspectorv5

import (
	"fmt"
	"strings"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
	"github.com/fatih/color"
	"github.com/pkg/errors"
)

const (
	OS_URL = "http://localhost:10144/_cluster/settings"
)

type DisableShardingInspection struct {
	writer          *cli.Writer
	upgradeUtils    UpgradeV5Utils
	isExecuted      bool
	exitError       error
	exitedWithError bool
}

func NewDisableShardingInspection(w *cli.Writer, utls UpgradeV5Utils) *DisableShardingInspection {
	return &DisableShardingInspection{
		writer:       w,
		upgradeUtils: utls,
	}
}

func (ds *DisableShardingInspection) ShowInfo(index *int) error {
	return nil
}
func (ds *DisableShardingInspection) Skip() {
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
	_, err = ds.upgradeUtils.ExecRequest(OS_URL, "PUT", disableShardingPayload)
	if err != nil {
		err = errors.Wrap(err, "Failed to disable sharding")
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
	_, err = ds.upgradeUtils.ExecRequest(OS_URL, "PUT", enableShardingPayload)
	if err != nil {
		return errors.Wrap(err, "Failed to enable sharding")
	}
	return nil
}

func (ds *DisableShardingInspection) GetInstallationType() inspector.InstallationType {
	return inspector.EMBEDDED
}

func (ds *DisableShardingInspection) ExitHandler() error {
	if ds.exitedWithError {
		ds.writer.Println(fmt.Errorf("["+color.New(color.FgRed).Sprint("Error")+"] %w", ds.exitError).Error())
	}
	return nil
}
